import * as vscode from "vscode";
import { BuildMode, MemberConf, VSE_LAST_BUILD_MODE_KEY, VseSettings } from "./types";

const REQUIRED_TOKENS = ["#PHASENAME#", "#CATALOG#", "#ID#", "#LNKSTEP#", "#XOPTS#", "#COMPILEOPTIONS#", "#SOURCE#"] as const;

type AssembleInput = {
  conf: MemberConf;
  mode: BuildMode;
  sourceText: string;
  settings: VseSettings;
};

export async function selectBuildMode(state: vscode.Memento): Promise<BuildMode | undefined> {
  const lastMode = state.get<BuildMode>(VSE_LAST_BUILD_MODE_KEY, "T");
  const pick = await vscode.window.showQuickPick(
    [
      { label: "T", description: "Test", mode: "T" as const },
      { label: "P", description: "Production", mode: "P" as const },
    ],
    {
      title: "Select build mode",
      placeHolder: `Last mode: ${lastMode}`,
      ignoreFocusOut: true,
    }
  );

  if (!pick) return undefined;
  await state.update(VSE_LAST_BUILD_MODE_KEY, pick.mode);
  return pick.mode;
}

function requirePlaceholderValue(value: string, settingKey: string): string {
  if (!value.trim()) throw new Error(`Missing setting '${settingKey}'.`);
  return value;
}

function expandLnkStepToken(jcl: string, lnkstep: string): string {
  const token = "#LNKSTEP#";
  if (!jcl.includes(token)) return jcl;

  return jcl.replace(/#LNKSTEP#/g, (_match, offset: number, source: string) => {
    if (!lnkstep) return "";

    const nextChar = source[offset + token.length];
    const hasTextOnSameLine = nextChar !== undefined && nextChar !== "\n" && nextChar !== "\r";
    const endsWithNewline = lnkstep.endsWith("\n") || lnkstep.endsWith("\r");

    // Keep following JCL statement on the next line when token is used inline
    // like "#LNKSTEP#// EXEC LISTLOG".
    if (hasTextOnSameLine && !endsWithNewline) {
      return `${lnkstep}\n`;
    }
    return lnkstep;
  });
}

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function enforceLnkStepLineBreak(jobText: string, lnkstep: string): string {
  const normalized = lnkstep.trimEnd();
  if (!normalized) return jobText;

  // Safety net: if an expanded lnkstep is immediately followed by another
  // JCL command prefix on the same line, split it into a new line.
  const pattern = new RegExp(`(${escapeRegExp(normalized)})([ \\t]*//)`, "g");
  return jobText.replace(pattern, "$1\n$2");
}

export function assembleJobText(input: AssembleInput): { jobText: string; phase: string } {
  const { conf, mode, sourceText, settings } = input;

  const jcl = mode === "T" ? conf.tjcl : conf.pjcl;
  const compileOptions = mode === "T" ? conf.toptions : conf.poptions;
  const xopts = mode === "T" ? conf.txopts : conf.pxopts;
  const catalog = (() => {
    const isCics = conf.type === 4;
    if (mode === "T") {
      return isCics
        ? requirePlaceholderValue(settings.placeholders.catalogCicsTest, "cobol85.vse.placeholders.catalogCicsTest")
        : requirePlaceholderValue(settings.placeholders.catalogBatchTest, "cobol85.vse.placeholders.catalogBatchTest");
    }
    return isCics
      ? requirePlaceholderValue(settings.placeholders.catalogCicsProd, "cobol85.vse.placeholders.catalogCicsProd")
      : requirePlaceholderValue(settings.placeholders.catalogBatchProd, "cobol85.vse.placeholders.catalogBatchProd");
  })();
  const lnkstep = requirePlaceholderValue(settings.placeholders.lnkstep, "cobol85.vse.placeholders.lnkstep");

  const replacements: Array<[string, string]> = [
    ["#PHASENAME#", conf.phase],
    ["#CATALOG#", catalog],
    ["#ID#", requirePlaceholderValue(settings.placeholders.id, "cobol85.vse.placeholders.id")],
    ["#XOPTS#", xopts],
    ["#COMPILEOPTIONS#", compileOptions],
    ["#SOURCE#", sourceText],
  ];

  let result = expandLnkStepToken(jcl, lnkstep);
  for (const [token, value] of replacements) {
    result = result.split(token).join(value);
  }
  result = enforceLnkStepLineBreak(result, lnkstep);

  const unresolved = REQUIRED_TOKENS.filter((token) => result.includes(token));
  if (unresolved.length > 0) {
    throw new Error(`Unresolved placeholders in job text: ${unresolved.join(", ")}`);
  }

  return { jobText: result, phase: conf.phase };
}
