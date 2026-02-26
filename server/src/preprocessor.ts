/**
 * COPY expansion, REPLACING, segment mapping, statement parsing.
 */
import { Diagnostic, DiagnosticSeverity, Range } from "vscode-languageserver/node";
import * as path from "path";

import {
  CopyParseError,
  CopyStmt,
  CollectedStmt,
  LineSlice,
  PreprocessedDoc,
  ReplaceStat,
  ReplacingPair,
  Segment,
  StmtSeg,
  TextRange,
  Tok,
  TokKind,
  GenDiag,
} from "./types";

import {
  isValidFixedIndicator,
  pushDiag,
  fsPathFromUri,
  uriFromFsPath,
  loadTextCached,
  resolveCopybook,
  summarizeDirs,
  hasSeparatorPeriodOutsideLiterals,
  escapeRegExp,
  sliceLines,
  isFixedCommentIndicator,
} from "./utils";

import { normalizeFixedLineForParser } from "./normalizer";
import { basicFixedFormatChecks } from "./lint";

// ======================= PreBuilder =======================

export class PreBuilder {
  private parts: string[] = [];
  private segs: Segment[] = [];
  private off = 0;

  appendSourceLine(sourceUri: string, sourceLine: number, sourceCharStart: number, lineText: string, sourceLineTextLen: number) {
    // content segment (no newline)
    const start = this.off;
    const end = start + lineText.length;
    this.parts.push(lineText);
    this.segs.push({
      kind: "source",
      genStart: start,
      genEnd: end,
      sourceUri,
      sourceLine,
      sourceCharStart,
      sourceLineTextLen,
    });
    this.off = end;

    // newline as generated
    this.appendGeneratedText(sourceUri, Range.create(sourceLine, sourceCharStart, sourceLine, sourceCharStart), "\n");
  }

  appendGeneratedText(anchorUri: string, anchorRange: Range, text: string) {
    if (!text) return;
    const start = this.off;
    const end = start + text.length;
    this.parts.push(text);
    this.segs.push({ kind: "generated", genStart: start, genEnd: end, anchorUri, anchorRange });
    this.off = end;
  }

  appendPreprocessed(pre: PreprocessedDoc) {
    if (!pre.text) return;

    // optional: ensure trailing newline, damit "line world" stabil bleibt
    const text = pre.text.endsWith("\n") ? pre.text : pre.text + "\n";

    const startOff = this.off;
    this.parts.push(text);

    // segments uebernehmen, offsets shiften
    for (const s of pre.segments) {
      this.segs.push({
        ...s,
        genStart: s.genStart + startOff,
        genEnd: s.genEnd + startOff,
      } as Segment);
    }

    this.off += text.length;
  }

  build(): PreprocessedDoc {
    return { text: this.parts.join(""), segments: this.segs };
  }
}

// ======================= Line helpers =======================

export function appendOriginalLineForParser(builder: PreBuilder, uri: string, sl: LineSlice): void {
  if (sl.isComment) {
    if (sl.isFixed) {
      const line72 = sl.full.slice(0, 72);
      const muted = normalizeFixedLineForParser(line72, sl.indicator, true);
      builder.appendSourceLine(uri, sl.lineNo, 0, muted, line72.length);
    } else {
      builder.appendSourceLine(uri, sl.lineNo, 0, "", sl.full.length);
    }
    return;
  }

  if (sl.isFixed && !isValidFixedIndicator(sl.indicator)) {
    builder.appendSourceLine(uri, sl.lineNo, 0, "", sl.full.length);
    return;
  }

  if (sl.isFixed) {
    const line72 = sl.full.slice(0, 72);
    const normalized = normalizeFixedLineForParser(line72, sl.indicator, false);
    builder.appendSourceLine(uri, sl.lineNo, 0, normalized, line72.length);
    return;
  }

  // Strict fixed-mode: invalid short lines are neutralized for parser stability.
  builder.appendSourceLine(uri, sl.lineNo, 0, "", sl.full.length);
}

// ======================= preprocessUri =======================

export function preprocessUri(args: {
  uri: string;
  text: string;
  baseDirs: string[];
  diagsByUri: Map<string, Diagnostic[]>;
  includeStack: string[]; // fsPaths of currently included files
  depth: number;
  maxDepth: number;
}): PreprocessedDoc {
  const { uri, text, baseDirs, diagsByUri, includeStack, depth, maxDepth } = args;

  const builder = new PreBuilder();
  const slices = sliceLines(text);

  let i = 0;
  while (i < slices.length) {
    const sl = slices[i];

    // Kommentarzeilen nicht parsen; nur Zeilenstruktur fuer Mapping erhalten.
    if (sl.isComment) {
      if (sl.isFixed) {
        const line72 = sl.full.slice(0, 72);
        const muted = normalizeFixedLineForParser(line72, sl.indicator, true);
        builder.appendSourceLine(uri, sl.lineNo, 0, muted, line72.length);
      } else {
        builder.appendSourceLine(uri, sl.lineNo, 0, "", sl.full.length);
      }
      i++;
      continue;
    }

    // Strict fixed-mode: indicator in column 7 must be valid.
    if (sl.isFixed && !isValidFixedIndicator(sl.indicator)) {
      builder.appendSourceLine(uri, sl.lineNo, 0, "", sl.full.length);
      i++;
      continue;
    }

    // start COPY only on non-continuation lines (fixed-format rule)
    const isContinuation = sl.isFixed && sl.indicator === "-";

    if (!isContinuation && /\bCOPY\b/i.test(sl.lang)) {
      const stmt = collectCopyStatement(uri, slices, i);

      if (stmt) {
        const copyStmts = parseCopyStatements(stmt.text);

        // Wenn nichts parsebar ist: Originalzeilen NICHT verlieren -> einfach durchreichen
        if (copyStmts.length === 0) {
          for (const seg of stmt.segs) {
            const sl2 = slices[seg.lineNo];
            appendOriginalLineForParser(builder, uri, sl2);
          }
          i = stmt.lastLine + 1;
          continue;
        }

        for (const c of copyStmts) {
          // Punkt fehlt? -> Warnung am COPY Keyword (oder an '.' falls du willst)
          // (Wenn du den Fehler schon in parseCopyStatements erzeugst, kannst du das hier weglassen.)
          if (!c.terminatedByDot) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Warning,
              range: mapStmtTextRangeToDocRange(stmt, c.copyKeywordRange),
              message: "COPY-Statement ohne '.' (Punkt). Expansion kann unzuverlaessig sein.",
              source: "cobol85",
              code: "COPY_MISSING_PERIOD",
            });

            // Fallback: Originalzeilen durchreichen (damit nichts verschwindet)
            for (const seg of stmt.segs) {
              const sl2 = slices[seg.lineNo];
              appendOriginalLineForParser(builder, uri, sl2);
            }
            continue;
          }

          // 1) Resolve copybook (WICHTIG: baseDirs um aktuellen Datei-Ordner ergaenzen!)
          const uriFs = fsPathFromUri(uri);
          const thisDir = uriFs ? path.dirname(uriFs) : undefined;
          const resolveDirs = thisDir ? [thisDir, ...baseDirs] : baseDirs;
          const uniqueResolveDirs = Array.from(new Set(resolveDirs));

          const resolved = resolveCopybook(c.copybookName, uniqueResolveDirs);
          if (!resolved) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Error,
              range: mapStmtTextRangeToDocRange(stmt, c.nameRange),
              message: `COPY book nicht gefunden: ${c.copybookName}. Gesucht in: ${summarizeDirs(uniqueResolveDirs, 3)}`,
              source: "cobol85",
              code: "COPYBOOK_NOT_FOUND",
            });

            // Copybook fehlt -> Originalzeilen durchreichen (kein "Text verschlucken")
            for (const seg of stmt.segs) {
              const sl2 = slices[seg.lineNo];
              appendOriginalLineForParser(builder, uri, sl2);
            }
            continue;
          }

          // 2) Cycle + depth detection wie bisher
          if (includeStack.includes(resolved)) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Error,
              range: mapStmtTextRangeToDocRange(stmt, c.nameRange),
              message: `Zyklisches COPY erkannt: ${path.basename(resolved)} ist bereits im Include-Stack.`,
              source: "cobol85",
              code: "COPYBOOK_CYCLE",
            });
            // Originalzeilen durchreichen
            for (const seg of stmt.segs) {
              const sl2 = slices[seg.lineNo];
              appendOriginalLineForParser(builder, uri, sl2);
            }
            continue;
          }

          if (depth >= maxDepth) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Error,
              range: mapStmtTextRangeToDocRange(stmt, c.nameRange),
              message: `COPY max depth (${maxDepth}) erreicht. Abbruch bei: ${path.basename(resolved)}`,
              source: "cobol85",
              code: "COPYBOOK_MAX_DEPTH",
            });
            for (const seg of stmt.segs) {
              const sl2 = slices[seg.lineNo];
              appendOriginalLineForParser(builder, uri, sl2);
            }
            continue;
          }

          // 3) REPLACING Syntax Errors
          for (const err of c.errors) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Error,
              range: mapStmtTextRangeToDocRange(stmt, err.range),
              message: err.message,
              source: "cobol85",
              code: err.code,
            });
          }

          if (c.hasReplacingKeyword && c.replacing.length === 0) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Warning,
              range: mapStmtTextRangeToDocRange(stmt, c.replacingKeywordRange),
              message: "REPLACING gefunden, aber keine gueltigen 'FROM BY TO' Paare erkannt.",
              source: "cobol85",
              code: "COPY_REPLACING_EMPTY",
            });
          }

          const copyUri = uriFromFsPath(resolved);
          const copyText = loadTextCached(resolved);
          if (copyText === undefined) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Error,
              range: mapStmtTextRangeToDocRange(stmt, c.nameRange),
              message: `COPY book konnte nicht gelesen werden: ${resolved}`,
              source: "cobol85",
              code: "COPYBOOK_READ_ERROR",
            });
            for (const seg of stmt.segs) {
              const sl2 = slices[seg.lineNo];
              appendOriginalLineForParser(builder, uri, sl2);
            }
            continue;
          }

          basicFixedFormatChecks(copyUri, copyText, diagsByUri);

          const hasReplacing = c.replacing.length > 0;

          // Wenn REPLACING: apply auf Fixed-Text, damit nested COPY & continuation erhalten bleiben
          let copyTextToProcess = copyText;
          if (hasReplacing) {
            const applied = applyReplacingToFixedText(copyText, c.replacing);
            for (const stat of applied.stats) {
              if (stat.count === 0) {
                pushDiag(diagsByUri, uri, {
                  severity: DiagnosticSeverity.Warning,
                  range: mapStmtTextRangeToDocRange(stmt, stat.fromRange),
                  message: `REPLACING: FROM-Pattern kommt im Copybook nicht vor: ${stat.fromDisplay}`,
                  source: "cobol85",
                  code: "COPY_REPLACING_NO_MATCH",
                });
              }
            }
            copyTextToProcess = applied.text;
          }

          const rec = preprocessUri({
            uri: copyUri,
            text: copyTextToProcess,
            baseDirs: uniqueResolveDirs,
            diagsByUri,
            includeStack: [...includeStack, resolved],
            depth: depth + 1,
            maxDepth,
          });

          if (hasReplacing) {
            // bei transformiertem Text: anchor am Callsite
            builder.appendGeneratedText(uri, mapStmtTextRangeToDocRange(stmt, c.nameRange), rec.text);
          } else {
            // echter Merge mit Segmenten
            builder.appendPreprocessed(rec);
          }
        }

        i = stmt.lastLine + 1;
        continue;
      }
    }

    // Normal line: fixed-format fuer Parser sanft normalisieren
    appendOriginalLineForParser(builder, uri, sl);
    i++;
  }

  return builder.build();
}

// ======================= Segment mapping =======================

export function mapOffset(pre: PreprocessedDoc, off: number): { uri: string; line: number; character: number } {
  // Find segment containing off (linear MVP)
  let seg = pre.segments.find((s) => off >= s.genStart && off < s.genEnd);
  if (!seg && off === pre.text.length && pre.segments.length) {
    seg = pre.segments[pre.segments.length - 1];
  }
  if (!seg) return { uri: "file:///", line: 0, character: 0 };

  if (seg.kind === "generated") {
    return {
      uri: seg.anchorUri,
      line: seg.anchorRange.start.line,
      character: seg.anchorRange.start.character,
    };
  }

  const rel = Math.max(0, Math.min(off - seg.genStart, seg.genEnd - seg.genStart));
  const char = Math.min(seg.sourceCharStart + rel, seg.sourceLineTextLen);
  return { uri: seg.sourceUri, line: seg.sourceLine, character: char };
}

export function flattenLang(slices: LineSlice[]): string {
  // keep line structure for later mapping
  return slices.map((s) => (s.isComment ? "" : s.lang)).join("\n");
}

export function collectCopyStatement(uri: string, slices: LineSlice[], startLine: number): CollectedStmt | undefined {
  const segs: StmtSeg[] = [];
  let text = "";
  let offset = 0;

  const maxLines = 50;
  let lastLine = startLine;
  let foundPeriod = false;

  for (let k = 0; k < maxLines; k++) {
    const idx = startLine + k;
    if (idx >= slices.length) break;

    const sl = slices[idx];

    if (k > 0) {
      // Comment lines are skipped but don't break the statement
      if (sl.isComment) continue;

      // Continuation lines (indicator '-') are always part of the statement
      const isContinuation = sl.isFixed && sl.indicator === "-";

      if (!isContinuation) {
        // If we already collected a period, stop before this line
        if (foundPeriod) break;

        // Non-continuation, non-comment lines: only continue if they look
        // like part of the COPY statement (REPLACING clauses, pseudo-text, etc.)
        // Stop if the line starts a clearly new statement.
        const langTrim = sl.lang.trimStart();
        if (langTrim.length === 0) break; // blank code line

        // If line starts with a known COBOL verb that cannot be part of COPY, stop
        const firstWord = langTrim.match(/^([A-Z][A-Z0-9-]*)\b/i);
        if (firstWord) {
          const upper = firstWord[1].toUpperCase();
          const copyClauseWords = new Set([
            "REPLACING", "BY", "IN", "OF", "SUPPRESS", "LEADING", "TRAILING",
          ]);
          // If it's not a COPY-related keyword and not a continuation token,
          // check heuristics
          if (!copyClauseWords.has(upper) && /^(MOVE|ADD|SUBTRACT|MULTIPLY|DIVIDE|COMPUTE|PERFORM|IF|EVALUATE|DISPLAY|CALL|READ|WRITE|OPEN|CLOSE|COPY|EXEC|GO|STOP|EXIT|ACCEPT|SET|SEARCH|SORT|MERGE|INITIALIZE|INSPECT|STRING|UNSTRING|DELETE|REWRITE|RELEASE|RETURN|START|ALTER|CANCEL|CONTINUE|ENTRY|GOBACK|USE|NEXT)$/i.test(upper)) {
            break;
          }
        }
      }
    }

    segs.push({ lineNo: sl.lineNo, langStart: sl.langStart, text: sl.lang, startOffset: offset });
    text += sl.lang;
    offset += sl.lang.length;

    text += "\n";
    offset += 1;

    lastLine = sl.lineNo;

    // Check if this line's lang area contains a period (potential statement terminator)
    if (hasSeparatorPeriodOutsideLiterals(sl.lang)) {
      foundPeriod = true;
    }
  }

  if (segs.length === 0) return undefined;
  return { text, segs, firstLine: startLine, lastLine, inUri: uri };
}

export function mapStmtTextRangeToDocRange(stmt: CollectedStmt, r: TextRange): Range {
  const s = mapStmtOffset(stmt, r.start);
  const e = mapStmtOffset(stmt, r.end);
  return Range.create(s.line, s.character, e.line, e.character);
}

export function mapStmtOffset(stmt: CollectedStmt, off: number): { line: number; character: number } {
  let segIndex = stmt.segs.length - 1;

  for (let i = 0; i < stmt.segs.length; i++) {
    const seg = stmt.segs[i];
    const segEnd = seg.startOffset + seg.text.length;
    if (off <= segEnd) {
      segIndex = i;
      break;
    }
  }

  const seg = stmt.segs[segIndex];
  const segEnd = seg.startOffset + seg.text.length;
  const clamped = Math.max(seg.startOffset, Math.min(off, segEnd));
  const colInLang = clamped - seg.startOffset;

  return { line: seg.lineNo, character: seg.langStart + colInLang };
}

export function mapGenRange(pre: PreprocessedDoc, startOff: number, endOff: number): { uri: string; range: Range } | undefined {
  const a = mapGenPoint(pre, startOff);
  const b = mapGenPoint(pre, Math.max(startOff, endOff));

  if (!a) return undefined;

  // Wenn Endpunkt auf anderes URI mappt, pinnen wir auf Startpunkt (MVP)
  if (!b || a.uri !== b.uri) {
    return {
      uri: a.uri,
      range: Range.create(a.line, a.character, a.line, a.character + Math.max(1, endOff - startOff)),
    };
  }

  return {
    uri: a.uri,
    range: Range.create(a.line, a.character, b.line, b.character),
  };
}

export function mapGenPoint(pre: PreprocessedDoc, off: number): { uri: string; line: number; character: number } | undefined {
  // end exklusiv behandeln
  let seg = pre.segments.find((s) => off >= s.genStart && off < s.genEnd);
  if (!seg && off === pre.text.length && pre.segments.length) {
    seg = pre.segments[pre.segments.length - 1];
  }
  if (!seg) return undefined;

  if (seg.kind === "generated") {
    return {
      uri: seg.anchorUri,
      line: seg.anchorRange.start.line,
      character: seg.anchorRange.start.character,
    };
  }

  const rel = Math.max(0, Math.min(off - seg.genStart, seg.genEnd - seg.genStart));
  const char = Math.min(seg.sourceCharStart + rel, seg.sourceLineTextLen);
  return { uri: seg.sourceUri, line: seg.sourceLine, character: char };
}

// ======================= COPY parsing =======================

export function parseCopyStatements(text: string): CopyStmt[] {
  const toks = tokenizeCobolText(text);
  const out: CopyStmt[] = [];

  for (let i = 0; i < toks.length; i++) {
    if (toks[i].upper !== "COPY") continue;

    const errors: CopyParseError[] = [];
    const replacing: ReplacingPair[] = [];

    const nameTok = toks[i + 1];
    if (!nameTok || nameTok.kind === "dot") {
      errors.push({
        code: "COPY_MISSING_NAME",
        message: "COPY ohne Copybook-Name.",
        range: { start: toks[i].start, end: toks[i].end },
      });
      continue;
    }

    if (nameTok.kind === "pseudo" && nameTok.closed === false) {
      errors.push({
        code: "COPY_PSEUDOTEXT_UNCLOSED",
        message: "Unclosed Pseudotext: erwartet abschliessendes '=='.",
        range: { start: nameTok.start, end: nameTok.end },
      });
    }

    const copybookName = normalizeCopyName(nameTok.text);

    const stmt: CopyStmt = {
      copybookName,
      nameRange: { start: nameTok.start, end: nameTok.end },
      replacing,
      hasReplacingKeyword: false,
      replacingKeywordRange: { start: nameTok.end, end: nameTok.end },
      copyKeywordRange: { start: toks[i].start, end: toks[i].end },
      terminatedByDot: false,
      dotRange: undefined,

      errors,
    };

    // Terminierung durch '.' finden (erste DOT nach COPY)
    for (let k = i + 1; k < toks.length; k++) {
      if (toks[k].kind === "dot") {
        stmt.terminatedByDot = true;
        stmt.dotRange = { start: toks[k].start, end: toks[k].end };
        break;
      }
    }
    if (!stmt.terminatedByDot) {
      errors.push({
        code: "COPY_MISSING_PERIOD",
        message: "COPY-Statement ohne '.' (Punkt). Expansion kann unzuverlaessig sein.",
        range: stmt.copyKeywordRange,
      });
    }

    let j = i + 2;

    while (j < toks.length && toks[j].kind !== "dot") {
      const u = toks[j].upper;
      if (u === "IN" || u === "OF") {
        j += 2;
        continue;
      }
      if (u === "SUPPRESS") {
        j += 1;
        continue;
      }
      if (u === "REPLACING") break;
      j += 1;
    }

    if (j < toks.length && toks[j].upper === "REPLACING") {
      stmt.hasReplacingKeyword = true;
      stmt.replacingKeywordRange = { start: toks[j].start, end: toks[j].end };
      j += 1;

      while (j < toks.length && toks[j].kind !== "dot") {
        if (toks[j].upper === "LEADING" || toks[j].upper === "TRAILING") {
          j += 1;
          if (j >= toks.length || toks[j].kind === "dot") break;
        }

        const fromTok = toks[j];
        if (!fromTok || fromTok.kind === "dot") break;
        j += 1;

        if (fromTok.kind === "pseudo" && fromTok.closed === false) {
          errors.push({
            code: "COPY_PSEUDOTEXT_UNCLOSED",
            message: "Unclosed Pseudotext: erwartet abschliessendes '=='.",
            range: { start: fromTok.start, end: fromTok.end },
          });
        }

        const byTok = toks[j];
        if (!byTok || byTok.kind === "dot" || byTok.upper !== "BY") {
          errors.push({
            code: "COPY_REPLACING_MISSING_BY",
            message: "COPY REPLACING: erwartet 'BY' nach dem FROM-Teil.",
            range: { start: fromTok.start, end: fromTok.end },
          });

          while (j < toks.length && toks[j].kind !== "dot" && toks[j].upper !== "BY") j++;
          if (j < toks.length && toks[j].upper === "BY") j++;
        } else {
          j += 1;
        }

        const toTok = toks[j];
        if (!toTok || toTok.kind === "dot") {
          errors.push({
            code: "COPY_REPLACING_MISSING_TO",
            message: "COPY REPLACING: erwartet TO-Teil nach 'BY'.",
            range: byTok
              ? { start: byTok.start, end: byTok.end }
              : { start: fromTok.start, end: fromTok.end },
          });
          break;
        }

        if (toTok.kind === "pseudo" && toTok.closed === false) {
          errors.push({
            code: "COPY_PSEUDOTEXT_UNCLOSED",
            message: "Unclosed Pseudotext: erwartet abschliessendes '=='.",
            range: { start: toTok.start, end: toTok.end },
          });
        }

        replacing.push({
          from: fromTok.text,
          to: toTok.text,
          fromRange: { start: fromTok.start, end: fromTok.end },
          toRange: { start: toTok.start, end: toTok.end },
          fromKind: fromTok.kind,
          toKind: toTok.kind,
        });

        j += 1;
      }
    }

    out.push(stmt);
  }

  return out;
}

export function tokenizeCobolText(text: string): Tok[] {
  const toks: Tok[] = [];
  const n = text.length;
  let i = 0;

  const pushTok = (t: Omit<Tok, "upper">) => toks.push({ ...t, upper: t.text.toUpperCase() });

  while (i < n) {
    while (i < n && /\s/.test(text[i])) i++;
    if (i >= n) break;

    const ch = text[i];

    if (ch === ".") {
      pushTok({ text: ".", start: i, end: i + 1, kind: "dot" });
      i += 1;
      continue;
    }

    if (ch === "=" && i + 1 < n && text[i + 1] === "=") {
      const start = i;
      i += 2;
      const close = text.indexOf("==", i);
      if (close === -1) {
        pushTok({ text: text.slice(start), start, end: n, kind: "pseudo", closed: false });
        break;
      } else {
        const end = close + 2;
        pushTok({ text: text.slice(start, end), start, end, kind: "pseudo", closed: true });
        i = end;
        continue;
      }
    }

    if (ch === "\"" || ch === "'") {
      const quote = ch;
      const start = i;
      i += 1;
      while (i < n && text[i] !== quote) i++;
      if (i < n && text[i] === quote) i += 1;
      pushTok({ text: text.slice(start, i), start, end: i, kind: "string" });
      continue;
    }

    {
      const start = i;
      while (i < n && !/\s/.test(text[i]) && text[i] !== ".") i++;
      pushTok({ text: text.slice(start, i), start, end: i, kind: "word" });
      continue;
    }
  }

  return toks;
}

export function normalizeCopyName(s: string): string {
  if ((s.startsWith("\"") && s.endsWith("\"")) || (s.startsWith("'") && s.endsWith("'"))) {
    s = s.slice(1, -1);
  }
  if (s.startsWith("==") && s.endsWith("==")) {
    s = s.slice(2, -2);
  }
  return s.replace(/\.+$/, "");
}

// ======================= REPLACING apply (MVP) =======================

export function applyReplacingMvp(text: string, pairs: ReplacingPair[]): { text: string; stats: ReplaceStat[] } {
  let out = text;
  const stats: ReplaceStat[] = [];

  for (const p of pairs) {
    const from = unwrapReplacingToken(p.from, p.fromKind);
    const to = unwrapReplacingToken(p.to, p.toKind);

    const { nextText, count } = replaceAllHeuristic(out, from, to, p.fromKind);
    out = nextText;

    stats.push({ fromRange: p.fromRange, fromDisplay: p.from, count });
  }

  return { text: out, stats };
}

export function applyReplacingToFixedText(
  original: string,
  pairs: ReplacingPair[]
): { text: string; stats: ReplaceStat[] } {
  const slices = sliceLines(original);

  // counts pro pair
  const stats: ReplaceStat[] = pairs.map((p) => ({
    fromRange: p.fromRange,
    fromDisplay: p.from,
    count: 0,
  }));

  const outLines: string[] = [];

  for (const sl of slices) {
    if (sl.isComment) {
      outLines.push(sl.full);
      continue;
    }

    if (sl.isFixed) {
      const prefix = sl.full.slice(0, 7);          // cols 1-7 inkl indicator
      let lang = sl.lang;

      for (let i = 0; i < pairs.length; i++) {
        const p = pairs[i];
        const from = unwrapReplacingToken(p.from, p.fromKind);
        const to = unwrapReplacingToken(p.to, p.toKind);
        const r = replaceAllHeuristic(lang, from, to, p.fromKind);
        lang = r.nextText;
        stats[i].count += r.count;
      }

      // Language area klassisch 65 chars (8-72). Wir halten das "halbwegs" stabil:
      const langFixed = lang.padEnd(65, " ").slice(0, 65);

      outLines.push(prefix + langFixed);
    } else {
      // free format
      let line = sl.full;
      for (let i = 0; i < pairs.length; i++) {
        const p = pairs[i];
        const from = unwrapReplacingToken(p.from, p.fromKind);
        const to = unwrapReplacingToken(p.to, p.toKind);
        const r = replaceAllHeuristic(line, from, to, p.fromKind);
        line = r.nextText;
        stats[i].count += r.count;
      }
      outLines.push(line);
    }
  }

  return { text: outLines.join("\n"), stats };
}

export function unwrapReplacingToken(t: string, kind: TokKind): string {
  if (kind === "pseudo" && t.startsWith("==") && t.endsWith("==")) return t.slice(2, -2);
  if (kind === "string" && ((t.startsWith("\"") && t.endsWith("\"")) || (t.startsWith("'") && t.endsWith("'"))))
    return t.slice(1, -1);
  return t;
}

export function replaceAllHeuristic(haystack: string, needle: string, repl: string, needleKind: TokKind): { nextText: string; count: number } {
  if (!needle) return { nextText: haystack, count: 0 };

  if (needleKind === "word" && /^[A-Za-z0-9-]+$/.test(needle)) {
    const re = new RegExp(`(^|[^A-Za-z0-9-])(${escapeRegExp(needle)})(?=[^A-Za-z0-9-]|$)`, "g");
    let count = 0;
    const nextText = haystack.replace(re, (_m, p1) => {
      count++;
      return `${p1}${repl}`;
    });
    return { nextText, count };
  }

  let count = 0;
  let idx = 0;
  let out = "";
  while (true) {
    const j = haystack.indexOf(needle, idx);
    if (j === -1) break;
    out += haystack.slice(idx, j) + repl;
    idx = j + needle.length;
    count++;
  }
  out += haystack.slice(idx);
  return { nextText: count ? out : haystack, count };
}
