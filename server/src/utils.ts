/**
 * Utility functions: fixed-format helpers, file I/O, caching, URI handling,
 * text utilities, and small pure algorithms.
 */
import { Diagnostic, DiagnosticSeverity, Range } from "vscode-languageserver/node";

import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "url";

import { FileCacheEntry, LineSlice } from "./types";

// ---- Fixed-format constants ----

export const FIXED_COMMENT_INDICATORS = new Set(["*", "/", "D", "d"]);
export const FIXED_CODE_INDICATORS = new Set([" ", "-"]);

// ---- Fixed-format helpers ----

export function hasFixedColumns(line: string): boolean {
  return line.length >= 7;
}

export function isFixedCommentIndicator(indicator: string): boolean {
  return FIXED_COMMENT_INDICATORS.has(indicator);
}

export function isValidFixedIndicator(indicator: string): boolean {
  return FIXED_CODE_INDICATORS.has(indicator) || isFixedCommentIndicator(indicator);
}

// ---- URI / path helpers ----

export function fsPathFromUri(uri: string): string | undefined {
  try {
    const u = new URL(uri);
    if (u.protocol !== "file:") return undefined;
    return fileURLToPath(u);
  } catch {
    return undefined;
  }
}

export function uriFromFsPath(p: string): string {
  // simple file:// URI
  let fixed = p.replace(/\\/g, "/");
  if (!fixed.startsWith("/")) fixed = "/" + fixed;
  return "file://" + fixed;
}

// ---- Caches ----

export const fileCache = new Map<string, FileCacheEntry>();
export const copybookResolveCache = new Map<string, string | null>();

// ---- File I/O ----

export function loadTextCached(fullPath: string): string | undefined {
  try {
    const st = fs.statSync(fullPath);
    const prev = fileCache.get(fullPath);
    if (prev && prev.mtimeMs === st.mtimeMs) return prev.text;

    const text = fs.readFileSync(fullPath, "utf8");
    fileCache.set(fullPath, { mtimeMs: st.mtimeMs, text });
    return text;
  } catch {
    return undefined;
  }
}

// ---- Copybook resolving ----

export function resolveCopybook(name: string, baseDirs: string[]): string | undefined {
  const cacheKey = `${name}\u0000${baseDirs.join("\u0001")}`;
  const cached = copybookResolveCache.get(cacheKey);
  if (cached !== undefined) return cached ?? undefined;

  const candidates = copybookCandidates(name);
  for (const dir of baseDirs) {
    for (const cand of candidates) {
      const full = path.isAbsolute(cand) ? cand : path.join(dir, cand);
      if (fs.existsSync(full)) {
        copybookResolveCache.set(cacheKey, full);
        return full;
      }
    }
  }

  copybookResolveCache.set(cacheKey, null);
  return undefined;
}

export function copybookCandidates(name: string): string[] {
  const cleaned = name.replace(/\.+$/, "");
  const ext = path.extname(cleaned);
  if (ext) return [cleaned];
  return [
    cleaned,
    `${cleaned}.cpy`,
    `${cleaned}.CPY`,
    `${cleaned}.cob`,
    `${cleaned}.COB`,
    `${cleaned}.cbl`,
    `${cleaned}.CBL`,
  ];
}

export function summarizeDirs(dirs: string[], max: number): string {
  if (dirs.length <= max) return dirs.join(", ");
  const shown = dirs.slice(0, max).join(", ");
  return `${shown}, ... (+${dirs.length - max} weitere)`;
}

// ---- Text utilities ----

export function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

export function excerpt(s: string, max: number): string {
  return (s || "").replace(/\s+/g, " ").slice(0, max);
}

export function pushDiag(map: Map<string, Diagnostic[]>, uri: string, d: Diagnostic) {
  const arr = map.get(uri);
  if (arr) arr.push(d);
  else map.set(uri, [d]);
}

export function hasSeparatorPeriodOutsideLiterals(s: string): boolean {
  let inSingle = false;
  let inDouble = false;

  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch === "'" && !inDouble) {
      inSingle = !inSingle;
      continue;
    }
    if (ch === "\"" && !inSingle) {
      inDouble = !inDouble;
      continue;
    }
    if (ch !== "." || inSingle || inDouble) continue;

    const prev = i > 0 ? s[i - 1] : " ";
    const next = i + 1 < s.length ? s[i + 1] : " ";
    const isDecimalDot = /[0-9]/.test(prev) && /[0-9]/.test(next);
    if (!isDecimalDot) return true;
  }

  return false;
}

// ---- Algorithm ----

export function levenshteinDistanceWithinLimit(a: string, b: string, limit: number): number {
  if (a === b) return 0;

  const al = a.length;
  const bl = b.length;
  if (Math.abs(al - bl) > limit) return limit + 1;

  let prev = new Array<number>(bl + 1);
  let curr = new Array<number>(bl + 1);

  for (let j = 0; j <= bl; j++) prev[j] = j;

  for (let i = 1; i <= al; i++) {
    curr[0] = i;
    let minRow = curr[0];

    const ca = a.charCodeAt(i - 1);
    for (let j = 1; j <= bl; j++) {
      const cost = ca === b.charCodeAt(j - 1) ? 0 : 1;
      curr[j] = Math.min(
        prev[j] + 1,
        curr[j - 1] + 1,
        prev[j - 1] + cost
      );
      if (curr[j] < minRow) minRow = curr[j];
    }

    if (minRow > limit) return limit + 1;
    [prev, curr] = [curr, prev];
  }

  return prev[bl];
}

// ---- COBOL line slicing ----

export function sliceLines(text: string): LineSlice[] {
  const lines = text.split(/\r?\n/);
  const out: LineSlice[] = [];

  for (let i = 0; i < lines.length; i++) {
    const full = lines[i];
    if (hasFixedColumns(full)) {
      const indicator = full[6];
      const isComment = isFixedCommentIndicator(indicator);
      const langStart = 7;
      const langEnd = Math.min(full.length, 72);
      const lang = full.slice(langStart, langEnd);
      out.push({ lineNo: i, full, isFixed: true, indicator, langStart, lang, isComment });
      continue;
    }

    // Strict fixed-mode: short lines are invalid fixed lines and carry no language area.
    out.push({
      lineNo: i,
      full,
      isFixed: false,
      indicator: " ",
      langStart: 0,
      lang: "",
      isComment: false,
    });
  }

  return out;
}

export function findFirstCodeAnchor(text: string): Range {
  const slices = sliceLines(text);

  for (const sl of slices) {
    if (sl.isComment) continue;
    const idx = sl.lang.search(/\S/);
    if (idx >= 0) {
      const ch = sl.langStart + idx;
      return Range.create(sl.lineNo, ch, sl.lineNo, ch + 1);
    }
  }
  return Range.create(0, 0, 0, 1);
}
