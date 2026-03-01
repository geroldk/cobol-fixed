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

// ---- BMS (Basic Mapping Support) map-to-COBOL conversion ----

export type BmsField = { name: string; length: number };
export type BmsMap = { mapName: string; fields: BmsField[] };
export type BmsMapset = { mapsetName: string; maps: BmsMap[]; tioapfx: boolean };

/**
 * Detect whether the given text is a BMS (Basic Mapping Support) assembler
 * source rather than COBOL.  BMS sources always contain the DFHMSD macro.
 */
export function isBmsSource(text: string): boolean {
  return /\bDFHMSD\b/.test(text);
}

type BmsStmt = { label: string; op: string; operands: string };

/**
 * Parse 80-column assembler statements, joining continuation lines
 * (non-blank column 72).  Returns label / operation / operands per statement.
 */
function joinBmsStatements(text: string): BmsStmt[] {
  const lines = text.split(/\r?\n/);
  const stmts: BmsStmt[] = [];
  let i = 0;

  while (i < lines.length) {
    const line = lines[i];
    // Skip empty lines and assembler comments (col 1 = '*')
    if (line.length === 0 || line[0] === "*") { i++; continue; }

    const content = line.substring(0, Math.min(line.length, 71)).trimEnd();
    if (!content.trim() || content.trim() === "END") { i++; continue; }

    let cont = line.length >= 72 ? line[71] : " ";

    // Parse label (starts at col 1; blank = no label)
    let label = "";
    let rest = content;
    if (content[0] !== " ") {
      const m = content.match(/^(\S+)\s+(.*)/);
      if (m) { label = m[1]; rest = m[2]; }
      else { label = content; rest = ""; }
    } else {
      rest = content.trimStart();
    }

    // Parse operation and operands
    const m2 = rest.match(/^(\S+)\s*(.*)/);
    const op = m2 ? m2[1].toUpperCase() : "";
    let operands = m2 ? m2[2].trimEnd() : "";

    // Accumulate continuation lines (col 72 non-blank → next line continues)
    while (cont !== " " && i + 1 < lines.length) {
      i++;
      const nextLine = lines[i];
      cont = nextLine.length >= 72 ? nextLine[71] : " ";
      // Continuation operands start at column 16 (0-indexed 15)
      const contText = nextLine.length > 15
        ? nextLine.substring(15, Math.min(nextLine.length, 71)).trimEnd()
        : "";
      operands += contText;
    }

    if (op) stmts.push({ label, op, operands });
    i++;
  }

  return stmts;
}

/**
 * Parse a BMS mapset source into a structured representation.
 * Returns undefined if the source contains no maps.
 */
export function parseBmsMapset(text: string): BmsMapset | undefined {
  const stmts = joinBmsStatements(text);

  let mapsetName = "";
  let tioapfx = false;
  const maps: BmsMap[] = [];
  let currentMap: BmsMap | undefined;

  for (const stmt of stmts) {
    if (stmt.op === "DFHMSD") {
      if (/\bTYPE\s*=\s*FINAL\b/i.test(stmt.operands)) {
        if (currentMap) { maps.push(currentMap); currentMap = undefined; }
      } else {
        mapsetName = stmt.label;
        tioapfx = /\bTIOAPFX\s*=\s*YES\b/i.test(stmt.operands);
      }
    } else if (stmt.op === "DFHMDI") {
      if (currentMap) maps.push(currentMap);
      currentMap = { mapName: stmt.label, fields: [] };
    } else if (stmt.op === "DFHMDF") {
      if (currentMap && stmt.label) {
        const m = stmt.operands.match(/\bLENGTH\s*=\s*0*(\d+)/i);
        const length = m ? parseInt(m[1], 10) : 0;
        if (length > 0) {
          currentMap.fields.push({ name: stmt.label, length });
        }
      }
    }
  }

  if (currentMap) maps.push(currentMap);
  if (maps.length === 0) return undefined;

  return { mapsetName, maps, tioapfx };
}

/**
 * Generate COBOL copybook text from a parsed BMS mapset.
 *
 * For each map (DFHMDI label = mapName) with named fields, two 01-levels
 * are emitted: mapNameI (input) and mapNameO (output, REDEFINES …I).
 * Each named DFHMDF field with LENGTH>0 produces five 02/03-level items
 * with suffixes L (length), F (flag), A (attribute), I/O (data).
 */
export function generateCobolFromBms(mapset: BmsMapset): string {
  const lines: string[] = [];
  const p1 = "       ";         // cols 1-7  (sequence + indicator)
  const p2 = "           ";     // cols 1-7 + 4-space indent for 02-level
  const p3 = "               "; // cols 1-7 + 8-space indent for 03-level

  for (const map of mapset.maps) {
    const iName = map.mapName + "I";
    const oName = map.mapName + "O";

    // ---- Input map ----
    lines.push(`${p1}01  ${iName}.`);
    if (mapset.tioapfx) {
      lines.push(`${p2}02  FILLER PIC X(12).`);
    }
    for (const f of map.fields) {
      lines.push(`${p2}02  ${f.name}L PIC S9(4) COMP.`);
      lines.push(`${p2}02  ${f.name}F PIC X.`);
      lines.push(`${p2}02  FILLER REDEFINES ${f.name}F.`);
      lines.push(`${p3}03  ${f.name}A PIC X.`);
      lines.push(`${p2}02  ${f.name}I PIC X(${f.length}).`);
    }

    // ---- Output map (REDEFINES input) ----
    lines.push(`${p1}01  ${oName} REDEFINES ${iName}.`);
    if (mapset.tioapfx) {
      lines.push(`${p2}02  FILLER PIC X(12).`);
    }
    for (const f of map.fields) {
      lines.push(`${p2}02  ${f.name}L PIC S9(4) COMP.`);
      lines.push(`${p2}02  ${f.name}F PIC X.`);
      lines.push(`${p2}02  FILLER REDEFINES ${f.name}F.`);
      lines.push(`${p3}03  ${f.name}A PIC X.`);
      lines.push(`${p2}02  ${f.name}O PIC X(${f.length}).`);
    }
  }

  return lines.join("\n") + "\n";
}
