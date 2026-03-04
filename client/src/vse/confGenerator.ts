import { mkdir, readFile, stat, writeFile } from "node:fs/promises";
import * as path from "path";
import * as vscode from "vscode";
import { serializeMemberConf } from "./confParser";
import { CobolMemberType, MemberConf } from "./types";

type NewTypeOption = Exclude<CobolMemberType, 0 | 1>;

const TYPE_OPTIONS: Array<{ label: string; type: NewTypeOption; description: string }> = [
  { label: "2 - COBOL85", type: 2, description: "COBOL85 without DLI/CICS defaults" },
  { label: "3 - COBOL85 + DLI", type: 3, description: "COBOL85DLI skeleton + DLI XOPTS" },
  { label: "4 - COBOL85 + DLI + CICS", type: 4, description: "COBOL85DLICICS skeleton + DLI+CICS XOPTS" },
];

async function readText(filePath: string): Promise<string> {
  return readFile(filePath, "utf8");
}

function normalizeForConf(text: string): string {
  return text.replace(/\r\n/g, "\n").replace(/\r/g, "\n").trim();
}

async function getDefaultsByType(extensionRoot: string, type: NewTypeOption): Promise<{
  tjcl: string;
  pjcl: string;
  txopts: string;
  pxopts: string;
  compileOptions: string;
}> {
  const skelDir = path.join(extensionRoot, "skel");
  const skeletonName = type === 2 ? "COBOL85.skel" : (type === 3 ? "COBOL85DLI.skel" : "COBOL85DLICICS.skel");
  const xoptName = type === 3 ? "COBOL85DLI.xopt" : (type === 4 ? "COBOL85DLICICS.xopt" : undefined);

  const skeleton = normalizeForConf(await readText(path.join(skelDir, skeletonName)));
  const compileOptions = normalizeForConf(await readText(path.join(skelDir, "COMPILEROPTIONS.opt")));
  const xopts = xoptName ? normalizeForConf(await readText(path.join(skelDir, xoptName))) : "";

  return {
    tjcl: skeleton,
    pjcl: skeleton,
    txopts: xopts,
    pxopts: xopts,
    compileOptions,
  };
}

async function promptType(): Promise<NewTypeOption | undefined> {
  const pick = await vscode.window.showQuickPick(
    TYPE_OPTIONS.map((opt) => ({
      label: opt.label,
      description: opt.description,
      type: opt.type,
    })),
    {
      title: "Select member type for new .conf",
      placeHolder: "Only 2/3/4 are offered for new configs",
      ignoreFocusOut: true,
    }
  );
  return pick?.type;
}

async function promptPhase(defaultPhase: string): Promise<string | undefined> {
  const value = await vscode.window.showInputBox({
    title: "Phase name",
    prompt: "Phase for the generated .conf",
    value: defaultPhase,
    ignoreFocusOut: true,
    validateInput: (input) => input.trim() ? undefined : "Phase is required.",
  });
  if (!value) return undefined;
  return value.trim();
}

async function confirmCreate(confPath: string, previewText: string, showPreview: boolean): Promise<boolean> {
  if (showPreview) {
    const previewDoc = await vscode.workspace.openTextDocument({
      language: "ini",
      content: previewText,
    });
    await vscode.window.showTextDocument(previewDoc, { preview: true, preserveFocus: false });
  }

  const answer = await vscode.window.showInformationMessage(
    `Create .conf at ${confPath}?`,
    { modal: true },
    "Create",
    "Cancel"
  );
  return answer === "Create";
}

async function confirmOverwrite(confPath: string): Promise<boolean> {
  try {
    await stat(confPath);
  } catch {
    return true;
  }

  const answer = await vscode.window.showWarningMessage(
    `File already exists: ${confPath}. Overwrite?`,
    { modal: true },
    "Overwrite",
    "Cancel"
  );
  return answer === "Overwrite";
}

export async function createMemberConfWithWizard(
  extensionRoot: string,
  sourceDocPath: string,
  confPath: string,
  previewBeforeCreate: boolean
): Promise<string | undefined> {
  const baseName = path.basename(sourceDocPath, path.extname(sourceDocPath)).toUpperCase();
  const type = await promptType();
  if (!type) return undefined;

  const phase = await promptPhase(baseName);
  if (!phase) return undefined;

  const defaults = await getDefaultsByType(extensionRoot, type);
  const conf: MemberConf = {
    source: 0,
    phase,
    type,
    tjcl: defaults.tjcl,
    pjcl: defaults.pjcl,
    toptions: defaults.compileOptions,
    poptions: defaults.compileOptions,
    txopts: defaults.txopts,
    pxopts: defaults.pxopts,
  };

  const text = serializeMemberConf(conf);
  const canOverwrite = await confirmOverwrite(confPath);
  if (!canOverwrite) return undefined;

  const approved = await confirmCreate(confPath, text, previewBeforeCreate);
  if (!approved) return undefined;

  await mkdir(path.dirname(confPath), { recursive: true });
  await writeFile(confPath, text, "utf8");
  return confPath;
}

