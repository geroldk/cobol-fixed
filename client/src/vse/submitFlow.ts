import { access, mkdir, readFile, writeFile } from "node:fs/promises";
import * as path from "path";
import * as vscode from "vscode";
import { parseMemberConf } from "./confParser";
import { createMemberConfWithWizard } from "./confGenerator";
import { resolveConfForSource } from "./confResolver";
import { assembleJobText, selectBuildMode } from "./jobAssembler";
import { getVseSettings, validateConnectionSettings } from "./settings";
import { VseSettings } from "./types";

type VseRuntimeModule = {
  VsePowerClient: new (config: {
    host: string;
    port?: number;
    user: string;
    password: string;
    charset?: string;
  }) => {
    connect(): Promise<void>;
    disconnect(): Promise<void>;
    runJob(request: {
      jobSource: { kind: "string"; content: string };
      timeoutSec?: number;
      wait?: boolean;
      release?: boolean;
      loglst?: string;
      logpun?: string;
      nowait?: boolean;
    }): Promise<{
      maxReturnCode: number;
      jobRef: { name: string; number: number };
      outputs: { list: Array<{ path: string }>; punch: Array<{ path: string }> };
    }>;
  };
  VseAuthError: new (...args: unknown[]) => Error;
  VseConnectionError: new (...args: unknown[]) => Error;
  VseTimeoutError: new (...args: unknown[]) => Error;
};

const backgroundRuns = new Set<Promise<void>>();

function requireActiveFileEditor(): vscode.TextEditor {
  const editor = vscode.window.activeTextEditor;
  if (!editor) throw new Error("No active editor.");
  if (editor.document.uri.scheme !== "file") throw new Error("Only file-backed documents are supported.");
  return editor;
}

function secretKey(host: string, port: number, user: string): string {
  return `cobol85.vse.password::${host}::${port}::${user}`;
}

function timestampPart(): string {
  return new Date().toISOString().replace(/[-:]/g, "").replace(/\..+$/, "").replace("T", "_");
}

async function loadVseModule(): Promise<VseRuntimeModule> {
  return await import("vseconnector-ts") as VseRuntimeModule;
}

async function ensurePassword(context: vscode.ExtensionContext, settings: VseSettings): Promise<string> {
  const key = secretKey(settings.host, settings.port, settings.user);
  const existing = await context.secrets.get(key);
  if (existing) return existing;

  const password = await vscode.window.showInputBox({
    title: "VSE password",
    prompt: `Password for ${settings.user}@${settings.host}:${settings.port}`,
    password: true,
    ignoreFocusOut: true,
    validateInput: (v) => v ? undefined : "Password is required.",
  });
  if (!password) throw new Error("Password input cancelled.");
  await context.secrets.store(key, password);
  return password;
}

async function resolveOutputDir(docUri: vscode.Uri, baseDirSetting: string): Promise<string> {
  const folder = vscode.workspace.getWorkspaceFolder(docUri);
  const baseDir = path.isAbsolute(baseDirSetting)
    ? baseDirSetting
    : path.join(folder?.uri.fsPath ?? path.dirname(docUri.fsPath), baseDirSetting);
  await mkdir(baseDir, { recursive: true });
  return baseDir;
}

async function readUtf8IfExists(filePath: string): Promise<string | undefined> {
  try {
    await access(filePath);
    return await readFile(filePath, "utf8");
  } catch {
    return undefined;
  }
}

async function openCombinedList(phase: string, mode: "T" | "P", baseDir: string, listPaths: string[]): Promise<void> {
  const unique = Array.from(new Set(listPaths));
  if (unique.length === 0) {
    vscode.window.showWarningMessage("No LST output entries returned.");
    return;
  }

  const parts: string[] = [];
  for (const p of unique) {
    const content = await readUtf8IfExists(p);
    if (content === undefined) continue;
    parts.push(`===== ${path.basename(p)} =====\n${content.trimEnd()}\n`);
  }

  if (parts.length === 0) {
    vscode.window.showWarningMessage("LST output files were listed but could not be read.");
    return;
  }

  const combinedPath = path.join(baseDir, `${phase}-${mode}-${timestampPart()}.combined.lst`);
  await writeFile(combinedPath, parts.join("\n"), "utf8");
  const doc = await vscode.workspace.openTextDocument(vscode.Uri.file(combinedPath));
  await vscode.window.showTextDocument(doc, { preview: false });
}

function mapSubmitError(error: unknown): string {
  if (error instanceof Error) {
    if (error.name === "VseAuthError") return `Authentication failed: ${error.message}`;
    if (error.name === "VseConnectionError") return `Connection failed: ${error.message}`;
    if (error.name === "VseTimeoutError") return `Submit timeout: ${error.message}`;
    return error.message;
  }
  return String(error);
}

async function confirmExpandedJobPreview(
  settings: VseSettings,
  phase: string,
  mode: "T" | "P",
  jobText: string
): Promise<boolean> {
  if (!settings.submit.previewBeforeSubmit) return true;

  const previewDoc = await vscode.workspace.openTextDocument({
    language: "plaintext",
    content: jobText,
  });
  await vscode.window.showTextDocument(previewDoc, { preview: true, preserveFocus: false });

  const answer = await vscode.window.showInformationMessage(
    `Submit expanded job for phase ${phase} in mode ${mode}?`,
    { modal: true },
    "Submit",
    "Cancel"
  );
  return answer === "Submit";
}

async function runSubmitInternal(context: vscode.ExtensionContext): Promise<void> {
  const editor = requireActiveFileEditor();
  const settings = getVseSettings();
  const settingErrors = validateConnectionSettings(settings);
  if (settingErrors.length > 0) {
    throw new Error(settingErrors.join(" "));
  }

  const sourcePath = editor.document.uri.fsPath;
  const resolved = await resolveConfForSource(sourcePath);
  let confPath: string | undefined;
  if (resolved.kind === "resolved") {
    confPath = resolved.confPath;
  } else if (settings.conf.autoCreateOnMissing) {
    confPath = await createMemberConfWithWizard(
      context.extensionPath,
      sourcePath,
      resolved.suggestedPath,
      settings.conf.previewBeforeCreate
    );
    if (!confPath) throw new Error("Missing .conf and auto-create was cancelled.");
  } else {
    throw new Error(`No .conf found for ${path.basename(sourcePath)}.`);
  }

  const confContent = await readFile(confPath, "utf8");
  const conf = parseMemberConf(confContent);
  const mode = await selectBuildMode(context.workspaceState);
  if (!mode) return;

  const { jobText, phase } = assembleJobText({
    conf,
    mode,
    sourceText: editor.document.getText(),
    settings,
  });

  const approved = await confirmExpandedJobPreview(settings, phase, mode, jobText);
  if (!approved) {
    vscode.window.showInformationMessage("Submit cancelled.");
    return;
  }

  const outputDir = await resolveOutputDir(editor.document.uri, settings.output.baseDir);
  const stamp = timestampPart();
  const lstPath = path.join(outputDir, `${phase}-${mode}-${stamp}.lst`);
  const punPath = path.join(outputDir, `${phase}-${mode}-${stamp}.pun`);
  const password = await ensurePassword(context, settings);
  const vse = await loadVseModule();
  const client = new vse.VsePowerClient({
    host: settings.host,
    port: settings.port,
    user: settings.user,
    password,
    charset: settings.charset,
  });

  let result: Awaited<ReturnType<InstanceType<VseRuntimeModule["VsePowerClient"]>["runJob"]>> | undefined;
  await client.connect();
  try {
    result = await client.runJob({
      jobSource: { kind: "string", content: jobText },
      timeoutSec: settings.submit.timeoutSec,
      wait: true,
      release: true,
      nowait: false,
      loglst: lstPath,
      logpun: punPath,
    });
  } finally {
    await client.disconnect().catch(() => undefined);
  }

  if (!result) {
    throw new Error("Submit returned no result.");
  }

  const listPaths = result.outputs.list.map((item) => item.path);
  await openCombinedList(phase, mode, outputDir, listPaths);
  vscode.window.showInformationMessage(
    `Submit done: ${result.jobRef.name}.${result.jobRef.number} maxRC=${result.maxReturnCode}`
  );
}

export async function runVseSubmitCompileJob(context: vscode.ExtensionContext): Promise<void> {
  const settings = getVseSettings();
  const execute = async () => {
    try {
      await runSubmitInternal(context);
    } catch (error) {
      vscode.window.showErrorMessage(mapSubmitError(error));
    }
  };

  if (settings.submit.executionMode === "background") {
    const run = execute();
    backgroundRuns.add(run);
    void run.finally(() => {
      backgroundRuns.delete(run);
    });
    vscode.window.showInformationMessage("VSE submit started in background.");
    return;
  }

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: "Submitting VSE compile job...",
      cancellable: false,
    },
    async () => {
      await execute();
    }
  );
}

export async function runVseCreateMemberConf(context: vscode.ExtensionContext): Promise<void> {
  try {
    const editor = requireActiveFileEditor();
    const sourcePath = editor.document.uri.fsPath;
    const targetPath = path.join(
      path.dirname(sourcePath),
      `${path.basename(sourcePath, path.extname(sourcePath))}.conf`
    );
    const settings = getVseSettings();
    const created = await createMemberConfWithWizard(
      context.extensionPath,
      sourcePath,
      targetPath,
      settings.conf.previewBeforeCreate
    );
    if (!created) return;
    const doc = await vscode.workspace.openTextDocument(vscode.Uri.file(created));
    await vscode.window.showTextDocument(doc, { preview: false });
    vscode.window.showInformationMessage(`Created ${path.basename(created)}.`);
  } catch (error) {
    vscode.window.showErrorMessage(mapSubmitError(error));
  }
}

async function promptConnectionIdentity(settings: VseSettings): Promise<{ host: string; port: number; user: string } | undefined> {
  const host = await vscode.window.showInputBox({
    title: "VSE host",
    value: settings.host,
    ignoreFocusOut: true,
    validateInput: (v) => v.trim() ? undefined : "Host is required.",
  });
  if (!host) return undefined;

  const portRaw = await vscode.window.showInputBox({
    title: "VSE port",
    value: String(settings.port),
    ignoreFocusOut: true,
    validateInput: (v) => {
      const n = Number.parseInt(v, 10);
      if (!Number.isFinite(n) || n <= 0 || n > 65535) return "Port must be 1..65535.";
      return undefined;
    },
  });
  if (!portRaw) return undefined;
  const port = Number.parseInt(portRaw, 10);

  const user = await vscode.window.showInputBox({
    title: "VSE user",
    value: settings.user,
    ignoreFocusOut: true,
    validateInput: (v) => v.trim() ? undefined : "User is required.",
  });
  if (!user) return undefined;

  return { host: host.trim(), port, user: user.trim() };
}

export async function runVseSetPassword(context: vscode.ExtensionContext): Promise<void> {
  try {
    const settings = getVseSettings();
    const identity = await promptConnectionIdentity(settings);
    if (!identity) return;
    const password = await vscode.window.showInputBox({
      title: "VSE password",
      password: true,
      ignoreFocusOut: true,
      validateInput: (v) => v ? undefined : "Password is required.",
    });
    if (!password) return;
    await context.secrets.store(secretKey(identity.host, identity.port, identity.user), password);
    vscode.window.showInformationMessage("VSE password stored in SecretStorage.");
  } catch (error) {
    vscode.window.showErrorMessage(mapSubmitError(error));
  }
}

export async function runVseClearPassword(context: vscode.ExtensionContext): Promise<void> {
  try {
    const settings = getVseSettings();
    const identity = await promptConnectionIdentity(settings);
    if (!identity) return;
    await context.secrets.delete(secretKey(identity.host, identity.port, identity.user));
    vscode.window.showInformationMessage("VSE password removed from SecretStorage.");
  } catch (error) {
    vscode.window.showErrorMessage(mapSubmitError(error));
  }
}
