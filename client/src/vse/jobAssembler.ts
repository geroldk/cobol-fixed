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

/**
 * Wraps raw compiler options into fixed-format ` CBL ` lines (max 72 chars each).
 * Input is a plain comma-separated string like "LIB, APOST, NOADV, ...".
 * Output is one or more lines starting with " CBL " prefix, each ≤ 72 chars.
 */
function formatCompileOptions(raw: string): string {
  const CBL_PREFIX = " CBL ";
  const MAX_LINE = 72;

  const trimmed = raw.trim();
  if (!trimmed) return "";

  // Split on commas, trim each option (comma is used as separator, not trailing)
  const options = trimmed.split(/,/).map((t) => t.trim()).filter(Boolean);
  if (options.length === 0) return "";

  const lines: string[] = [];
  let current = CBL_PREFIX + options[0];

  for (let i = 1; i < options.length; i++) {
    const candidate = current + ", " + options[i];
    if (candidate.length > MAX_LINE) {
      lines.push(current);
      current = CBL_PREFIX + options[i];
    } else {
      current = candidate;
    }
  }
  lines.push(current);

  return lines.join("\n");
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
    ["#COMPILEOPTIONS#", formatCompileOptions(compileOptions)],
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
