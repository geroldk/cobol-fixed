/**
 * Lint rules for preprocessed COBOL text.
 * Also contains basic fixed-format checks and ID-DIVISION detection.
 */
import { Diagnostic, DiagnosticSeverity, Range } from "vscode-languageserver/node";

import {
  GenDiag,
  LintTok,
  OffsetRange,
  ProcedureLineContext,
} from "./types";

import {
  hasFixedColumns,
  isFixedCommentIndicator,
  isValidFixedIndicator,
  pushDiag,
  hasSeparatorPeriodOutsideLiterals,
  levenshteinDistanceWithinLimit,
  fsPathFromUri,
  sliceLines,
} from "./utils";

import { isCompilerDirectiveLine } from "./normalizer";

// ======================= Basic checks =======================

export function basicFixedFormatChecks(uri: string, text: string, diagsByUri: Map<string, Diagnostic[]>) {
  const lines = text.split(/\r?\n/);
  for (let lineNo = 0; lineNo < lines.length; lineNo++) {
    const full = lines[lineNo];

    const tabIdx = full.indexOf("\t");
    if (tabIdx >= 0) {
      pushDiag(diagsByUri, uri, {
        severity: DiagnosticSeverity.Warning,
        range: Range.create(lineNo, tabIdx, lineNo, tabIdx + 1),
        message: "TAB gefunden: Fixed-Columns koennen kaputt gehen. Besser Spaces verwenden.",
        source: "cobol85",
        code: "TAB_FIXED_FORMAT",
      });
    }

    if (full.length > 80) {
      pushDiag(diagsByUri, uri, {
        severity: DiagnosticSeverity.Warning,
        range: Range.create(lineNo, 80, lineNo, full.length),
        message: "Zeile ueberschreitet 80 Spalten (Fixed-Format).",
        source: "cobol85",
        code: "LINE_OVERFLOW",
      });
    }

    // Strict fixed-mode: every non-empty line must have fixed columns incl. indicator.
    if (full.trim().length === 0) continue;
    if (isCompilerDirectiveLine(full)) continue;

    if (!hasFixedColumns(full)) {
      pushDiag(diagsByUri, uri, {
        severity: DiagnosticSeverity.Error,
        range: Range.create(lineNo, 0, lineNo, Math.max(1, full.length)),
        message: "Kein gueltiges Fixed-Format: Spalte 7 (Indicator) fehlt.",
        source: "cobol85",
        code: "FIXED_FORMAT_REQUIRED",
      });
      continue;
    }

    const indicator = full[6];
    if (!isValidFixedIndicator(indicator)) {
      const shown = indicator === " " ? "<space>" : indicator;
      pushDiag(diagsByUri, uri, {
        severity: DiagnosticSeverity.Error,
        range: Range.create(lineNo, 6, lineNo, 7),
        message: `Ungueltiger Indicator in Spalte 7: '${shown}'. Erlaubt sind Leerzeichen, '*', '/', '-', 'D'.`,
        source: "cobol85",
        code: "INVALID_FIXED_INDICATOR",
      });
    }
  }
}

export function shouldRequireIdentificationDivision(uri: string, text: string): boolean {
  const fsPath = fsPathFromUri(uri);
  if (fsPath && /[\\/](copybook|copybooks)[\\/]/i.test(fsPath)) return false;

  const slices = sliceLines(text);
  const lang = slices
    .filter((s) => !s.isComment)
    .map((s) => s.lang)
    .join("\n");

  // Copybook-like snippets without division/program markers should not get this error.
  if (/\b(PROGRAM-ID|FUNCTION-ID|CLASS-ID|INTERFACE-ID)\s*\./i.test(lang)) return true;
  if (/\b(ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\s*\./i.test(lang)) return true;

  return false;
}

export function hasIdentificationDivision(text: string): boolean {
  // quick heuristic on language areas
  const slices = sliceLines(text);
  return slices.some((s) => !s.isComment && /\b(?:IDENTIFICATION|ID)\s+DIVISION\b/i.test(s.lang));
}

// ======================= Lint (preprocessed text) =======================

export function lintPreprocessed(text: string): GenDiag[] {
  const diags: GenDiag[] = [];

  type Frame = { kind: "IF" | "EVALUATE" | "EXEC"; startOff: number; tokenLen: number };
  type ExecTok = { upper: string; start: number; end: number };
  type ExecLintState = {
    execStartOff: number;
    execTokenLen: number;
    subtypeTok?: ExecTok;
    firstArgTok?: ExecTok;
    tokens: ExecTok[];
  };

  const stack: Frame[] = [];
  let execState: ExecLintState | undefined;

  for (const tok of scanLintTokens(text)) {
    if (tok.kind === "dot") {
      if (execState) continue;
      closeBlocksByPeriod(stack, diags);
      continue;
    }

    const u = tok.upper;
    const start = tok.start;
    const end = tok.end;

    if (execState) {
      if (u === "END-EXEC") {
        if (!popExpected(stack, "EXEC")) {
          diags.push({
            startOff: start,
            endOff: end,
            severity: DiagnosticSeverity.Error,
            code: "END_EXEC_WITHOUT_EXEC",
            message: "END-EXEC ohne passendes EXEC.",
          });
        }
        lintExecBlock(execState, diags);
        execState = undefined;
        continue;
      }

      if (!execState.subtypeTok) {
        execState.subtypeTok = { upper: u, start, end };
      } else if (!execState.firstArgTok) {
        execState.firstArgTok = { upper: u, start, end };
      }
      if (execState.tokens.length < 200) {
        execState.tokens.push({ upper: u, start, end });
      }
      continue;
    }

    // ---- Opens
    if (u === "IF") {
      stack.push({ kind: "IF", startOff: start, tokenLen: end - start });
      continue;
    }
    if (u === "EVALUATE") {
      stack.push({ kind: "EVALUATE", startOff: start, tokenLen: end - start });
      continue;
    }
    if (u === "EXEC") {
      // MVP: treat any EXEC ... END-EXEC
      stack.push({ kind: "EXEC", startOff: start, tokenLen: end - start });
      execState = { execStartOff: start, execTokenLen: end - start, tokens: [] };
      continue;
    }

    // ---- Closes
    if (u === "END-IF") {
      if (!popExpected(stack, "IF")) {
        diags.push({
          startOff: start,
          endOff: end,
          severity: DiagnosticSeverity.Error,
          code: "END_IF_WITHOUT_IF",
          message: "END-IF ohne passendes IF.",
        });
      }
      continue;
    }

    if (u === "END-EVALUATE") {
      if (!popExpected(stack, "EVALUATE")) {
        diags.push({
          startOff: start,
          endOff: end,
          severity: DiagnosticSeverity.Error,
          code: "END_EVALUATE_WITHOUT_EVALUATE",
          message: "END-EVALUATE ohne passendes EVALUATE.",
        });
      }
      continue;
    }

    if (u === "END-EXEC") {
      if (!popExpected(stack, "EXEC")) {
        diags.push({
          startOff: start,
          endOff: end,
          severity: DiagnosticSeverity.Error,
          code: "END_EXEC_WITHOUT_EXEC",
          message: "END-EXEC ohne passendes EXEC.",
        });
      }
      continue;
    }
  }

  if (execState) lintExecBlock(execState, diags);

  const lines = text.split("\n");
  lintDataDivisionEntryPeriods(lines, diags);
  lintProcedureVerbTypos(lines, diags);
  lintVsamStatusCobol85Checks(lines, diags);

  // ---- Unclosed blocks at EOF
  for (let i = stack.length - 1; i >= 0; i--) {
    const f = stack[i];
    const expected =
      f.kind === "IF" ? "END-IF" : f.kind === "EVALUATE" ? "END-EVALUATE" : "END-EXEC";

    diags.push({
      startOff: f.startOff,
      endOff: f.startOff + f.tokenLen,
      severity: DiagnosticSeverity.Error,
      code: "UNCLOSED_BLOCK",
      message: `${f.kind} eroeffnet, aber kein ${expected} gefunden.`,
    });
  }

  return diags;
}

export function lintDataDivisionEntryPeriods(lines: string[], diags: GenDiag[]): void {
  type PendingEntry = { startOff: number; tokenLen: number };

  let inDataDivision = false;
  let pending: PendingEntry | undefined;
  let off = 0;

  const flushPending = () => {
    if (!pending) return;
    diags.push({
      startOff: pending.startOff,
      endOff: pending.startOff + pending.tokenLen,
      severity: DiagnosticSeverity.Error,
      code: "DATA_ENTRY_MISSING_PERIOD",
      message: "Data-Description-Entry ohne abschliessenden Punkt (Separator Period).",
    });
    pending = undefined;
  };

  for (const line of lines) {
    const langStart = line.length >= 7 ? 7 : 0;
    const lang = line.slice(langStart);
    const trimmedStartLen = lang.length - lang.trimStart().length;
    const trimmed = lang.trim();
    const lineCodeStart = off + langStart + trimmedStartLen;

    if (/^(IDENTIFICATION|ENVIRONMENT|PROCEDURE)\s+DIVISION\b/i.test(trimmed)) {
      flushPending();
      inDataDivision = false;
    } else if (/^DATA\s+DIVISION\b/i.test(trimmed)) {
      flushPending();
      inDataDivision = true;
    }

    if (inDataDivision && trimmed.length > 0) {
      const fileEntry = /^(FD|SD|RD|CD)\b/i.exec(trimmed);
      const levelEntry = /^\s*(0?[1-9]|[1-4][0-9]|66|77|88)\s+([A-Z][A-Z0-9-]*)\b/i.exec(trimmed);
      const nonEntrySecondToken = new Set([
        "TIMES", "THRU", "THROUGH", "TO", "BY", "DEPENDING", "OCCURS",
        "PIC", "PICTURE", "VALUE", "VALUES", "IS", "USAGE", "INDEXED",
        "ASCENDING", "DESCENDING", "SIGN", "SYNCHRONIZED", "SYNC",
        "JUSTIFIED", "JUST",
      ]);
      const startsLevelEntry =
        !!levelEntry &&
        (levelEntry[2].toUpperCase() === "FILLER" ||
          !nonEntrySecondToken.has(levelEntry[2].toUpperCase()));
      const startsEntry = !!fileEntry || startsLevelEntry;

      if (pending && startsEntry) {
        // A new entry starts before the previous one has seen a separator period.
        flushPending();
      }

      if (startsEntry) {
        const tokenLen = fileEntry ? fileEntry[0].length : (levelEntry?.[1].length ?? 1);
        pending = { startOff: lineCodeStart, tokenLen };
      }

      if (pending && hasSeparatorPeriodOutsideLiterals(trimmed)) {
        pending = undefined;
      }
    }

    off += line.length + 1;
  }

  flushPending();
}

export function lintProcedureVerbTypos(lines: string[], diags: GenDiag[]): void {
  let inProcedureDivision = false;
  let off = 0;
  let prevProcedureLine: ProcedureLineContext | undefined;

  const knownStatementLeaders = new Set([
    "ACCEPT", "ADD", "ALTER", "CALL", "CANCEL", "CLOSE", "COMPUTE", "CONTINUE",
    "COPY", "DELETE", "DISPLAY", "DIVIDE", "ELSE", "END-ACCEPT", "END-ADD",
    "END-CALL", "END-COMPUTE", "END-DELETE", "END-DISPLAY", "END-DIVIDE",
    "END-EVALUATE", "END-IF", "END-MULTIPLY", "END-PERFORM", "END-READ",
    "END-RECEIVE", "END-RETURN", "END-REWRITE", "END-SEARCH", "END-START",
    "END-STRING", "END-SUBTRACT", "END-UNSTRING", "END-WRITE", "ENTRY",
    "EVALUATE", "EXEC", "EXIT", "GOBACK", "GO", "IF", "INITIALIZE", "INITIATE",
    "INSPECT", "MERGE", "MOVE", "MULTIPLY", "NEXT", "OPEN", "PERFORM",
    "READ", "RELEASE", "RETURN", "REWRITE", "SEARCH", "SET", "SORT", "START",
    "STOP", "STRING", "SUBTRACT", "UNSTRING", "USE", "WHEN", "WRITE",
  ]);

  const maybeClauseLeaders = new Set([
    "AT", "NOT", "ON", "INVALID", "SIZE", "WITH", "OR", "AND",
    "USING", "GIVING", "INTO", "TO", "FROM", "BY", "VARYING", "UNTIL", "TIMES",
    "THROUGH", "THRU", "END", "EOP", "PAGE", "OVERFLOW", "KEY", "ERROR",
    "EXCEPTION", "MODE", "LOCK", "RECORD", "RECORDS", "REPLACING", "CONVERTING",
    "COUNT", "DELIMITED", "POINTER", "TALLYING", "ORDER", "OFF", "IS", "ARE",
    "IN", "OF", "ASCENDING", "DESCENDING", "INDEXED", "DEPENDING", "SUPPRESS",
    "ACCESS", "ASSIGN", "ORGANIZATION", "SELECT", "FILE", "STATUS", "RELATIVE",
    "SEQUENTIAL", "RANDOM", "DYNAMIC", "LINAGE", "LABEL", "BLOCK", "CONTAINS",
    "CHARACTERS", "RECORDING", "AFTER", "BEFORE", "DECIMAL-POINT", "CURRENCY",
    "SYMBOLIC", "FILE-CONTROL", "I-O-CONTROL", "SPECIAL-NAMES", "SOURCE-COMPUTER",
    "OBJECT-COMPUTER", "PCB", "SEGMENT", "SEGLENGTH", "WHERE", "FIELDLENGTH",
    "COLLATING", "SEQUENCE",
    "LENGTH", "ENTRYLEN", "KEYLEN", "ALSO",
    "UPON", "CONSOLE", "VARIABLE", "INPUT", "OUTPUT", "I-O", "EXTEND",
  ]);

  const knownVerbsForSuggestion = [
    "ACCEPT", "ADD", "ALTER", "CALL", "CANCEL", "CLOSE", "COMPUTE", "CONTINUE",
    "COPY", "DELETE", "DISPLAY", "DIVIDE", "EVALUATE", "EXIT", "GOBACK", "GO",
    "IF", "INITIALIZE", "INITIATE", "INSPECT", "MERGE", "MOVE", "MULTIPLY",
    "OPEN", "PERFORM", "READ", "RELEASE", "RETURN", "REWRITE", "SEARCH", "SET",
    "SORT", "START", "STOP", "STRING", "SUBTRACT", "UNSTRING", "WRITE",
  ];

  const removedCobol74Keywords = new Map<string, string>([
    ["TRANSFORM", "INSPECT ... CONVERTING"],
    ["EXAMINE", "INSPECT"],
    ["ENTER", "CALL"],
    ["READY", "modernes Debugging (TRACE-Verben sind in COBOL 85 nicht standard)"],
    ["RESET", "modernes Debugging (TRACE-Verben sind in COBOL 85 nicht standard)"],
    ["TRACE", "modernes Debugging (TRACE-Verben sind in COBOL 85 nicht standard)"],
    ["NOTE", "Kommentarzeile mit *> (free) oder * in Spalte 7 (fixed)"],
  ]);

  for (const line of lines) {
    const isFixed = hasFixedColumns(line);
    const indicator = isFixed ? line[6] : " ";
    const isCommentLine = isFixed && isFixedCommentIndicator(indicator);
    const isContinuationLine = isFixed && indicator === "-";
    const langStart = isFixed ? 7 : 0;
    const lang = line.slice(langStart);
    const trimmedStartLen = lang.length - lang.trimStart().length;
    const trimmed = lang.trim();
    const lineCodeStart = off + langStart + trimmedStartLen;

    if (isCommentLine) {
      off += line.length + 1;
      continue;
    }

    if (/^(IDENTIFICATION|ENVIRONMENT|DATA)\s+DIVISION\b/i.test(trimmed)) {
      inProcedureDivision = false;
      prevProcedureLine = undefined;
    } else if (/^PROCEDURE\s+DIVISION\b/i.test(trimmed)) {
      inProcedureDivision = true;
      prevProcedureLine = undefined;
      off += line.length + 1;
      continue;
    }

    if (isCompilerDirectiveLine(line)) {
      off += line.length + 1;
      continue;
    }

    if (inProcedureDivision && trimmed.length > 0) {
      if (isContinuationLine) {
        prevProcedureLine = {
          indent: trimmedStartLen,
          startsKnownOrClause: true,
          hasSeparatorPeriod: hasSeparatorPeriodOutsideLiterals(trimmed),
        };
        off += line.length + 1;
        continue;
      }

      // Paragraph/Section labels terminate sentence context for verb-typo checks.
      if (/^[A-Z0-9-]+\s*(SECTION)?\s*\.$/i.test(trimmed)) {
        prevProcedureLine = undefined;
        off += line.length + 1;
        continue;
      }

      let stmtText = trimmed;
      let stmtIndent = trimmedStartLen;
      let stmtStartOff = lineCodeStart;

      // Support inline paragraph labels: "PARA. MOVE ...".
      const inlineLabel = /^([A-Z][A-Z0-9-]*)(?:\s+SECTION)?\.\s*(.+)$/i.exec(trimmed);
      if (inlineLabel) {
        const label = inlineLabel[1].toUpperCase();
        const labelLooksLikeVerb =
          knownStatementLeaders.has(label) || maybeClauseLeaders.has(label);

        if (!labelLooksLikeVerb) {
          const dotIndex = trimmed.indexOf(".");
          const afterDot = dotIndex >= 0 ? trimmed.slice(dotIndex + 1) : "";
          const afterDotTrimmed = afterDot.trimStart();
          const afterDotLeading = afterDot.length - afterDotTrimmed.length;

          stmtText = afterDotTrimmed;
          stmtIndent = trimmedStartLen + Math.max(0, dotIndex + 1 + afterDotLeading);
          stmtStartOff = lineCodeStart + Math.max(0, dotIndex + 1 + afterDotLeading);
          prevProcedureLine = undefined;
        }
      }

      const m = /^([A-Z][A-Z0-9-]*)\b/i.exec(stmtText);
      let startsKnownOrClause = false;
      const hasSeparatorPeriod = hasSeparatorPeriodOutsideLiterals(stmtText);
      let treatedAsContinuation = false;
      if (m) {
        const token = m[1].toUpperCase();

        const isKnown =
          knownStatementLeaders.has(token) ||
          maybeClauseLeaders.has(token);
        startsKnownOrClause = isKnown;

        if (!isKnown && /^[A-Z]+$/.test(token)) {
          const replacement = removedCobol74Keywords.get(token);
          if (replacement) {
            diags.push({
              startOff: stmtStartOff,
              endOff: stmtStartOff + token.length,
              severity: DiagnosticSeverity.Error,
              code: "COBOL74_KEYWORD_REMOVED",
              message: `COBOL-74-Schluesselwort ${token} ist in COBOL 85 nicht zulaessig. Ersatz: ${replacement}.`,
            });
            off += line.length + 1;
            continue;
          }

          const suggestion = suggestProcedureVerb(token, knownVerbsForSuggestion);
          treatedAsContinuation =
            !suggestion &&
            isLikelyProcedureContinuationLine(
              stmtText,
              stmtIndent,
              prevProcedureLine
            );
          if (!treatedAsContinuation) {
            const message = suggestion
              ? `Unbekanntes COBOL-Statement: ${token}. Meintest du ${suggestion}?`
              : `Unbekanntes COBOL-Statement: ${token}.`;

            diags.push({
              startOff: stmtStartOff,
              endOff: stmtStartOff + token.length,
              severity: DiagnosticSeverity.Error,
              code: "PROCEDURE_VERB_UNKNOWN",
              message,
            });
          }
        }
      }

      prevProcedureLine = {
        indent: stmtIndent,
        startsKnownOrClause: startsKnownOrClause || treatedAsContinuation,
        hasSeparatorPeriod,
      };
    }

    off += line.length + 1;
  }
}

export function isLikelyProcedureContinuationLine(
  trimmed: string,
  indent: number,
  prev: ProcedureLineContext | undefined
): boolean {
  if (!prev) return false;
  if (prev.hasSeparatorPeriod) return false;
  if (!prev.startsKnownOrClause) return false;

  // Typical fixed-format continuation style: arguments are indented deeper.
  if (indent > prev.indent) return true;

  // Common data-reference continuation, e.g. "ANZ OF A-TABLE".
  if (/^[A-Z][A-Z0-9-]*\s+(OF|IN)\b/i.test(trimmed)) return true;

  return false;
}

export function lintVsamStatusCobol85Checks(lines: string[], diags: GenDiag[]): void {
  let inProcedureDivision = false;
  let off = 0;

  for (const line of lines) {
    const langStart = line.length >= 7 ? 7 : 0;
    const lang = line.slice(langStart);
    const trimmedStartLen = lang.length - lang.trimStart().length;
    const trimmed = lang.trim();
    const lineCodeStart = off + langStart + trimmedStartLen;

    if (/^(IDENTIFICATION|ENVIRONMENT|DATA)\s+DIVISION\b/i.test(trimmed)) {
      inProcedureDivision = false;
    } else if (/^PROCEDURE\s+DIVISION\b/i.test(trimmed)) {
      inProcedureDivision = true;
      off += line.length + 1;
      continue;
    }

    if (inProcedureDivision && trimmed.length > 0) {
      // Paragraph/Section labels are not executable statements.
      if (!/^[A-Z0-9-]+\s*(SECTION)?\s*\.$/i.test(trimmed)) {
        const lit00 = /['"]00['"]/i.exec(trimmed);
        if (lit00 && looksLikeVsamStatusContext(trimmed)) {
          const hasSuccessRange = /['"]00['"]\s*(THRU|THROUGH)\s*['"]09['"]/i.test(trimmed);
          const hasOtherSuccessCodes = /['"]0[1-9]['"]/i.test(trimmed);

          if (!hasSuccessRange && !hasOtherSuccessCodes && hasLegacyStatus00Comparison(trimmed)) {
            const litStart = lineCodeStart + (lit00.index ?? 0);
            diags.push({
              startOff: litStart,
              endOff: litStart + lit00[0].length,
              severity: DiagnosticSeverity.Warning,
              code: "VSAM_RETURN_CODE_74_CHECK",
              message: "COBOL 85: Bei VSAM gilt '00' THRU '09' als OK. Pruefe nicht nur auf '00'.",
            });
          }
        }
      }
    }

    off += line.length + 1;
  }
}

function looksLikeVsamStatusContext(line: string): boolean {
  return /\b(?:RETURN-CO(?:DE)?|FILE-?STATUS|IO-?STATUS|VSAM(?:-?[A-Z0-9-]*)?|[A-Z0-9-]*STATUS)\b/i.test(line);
}

function hasLegacyStatus00Comparison(line: string): boolean {
  const patterns = [
    /\b[A-Z][A-Z0-9-]*\b\s*(?:IS\s+)?(?:NOT\s+)?(?:=|EQUAL\s+TO)\s*['"]00['"]/i,
    /['"]00['"]\s*(?:IS\s+)?(?:NOT\s+)?(?:=|EQUAL\s+TO)\s*\b[A-Z][A-Z0-9-]*\b/i,
  ];
  return patterns.some((re) => re.test(line));
}

export function suggestProcedureVerb(token: string, knownVerbs: string[]): string | undefined {
  if (token.length < 4) return undefined;
  if (!/^[A-Z]+$/.test(token)) return undefined;

  let bestKeyword: string | undefined;
  let bestDistance = Number.POSITIVE_INFINITY;

  for (const kw of knownVerbs) {
    const limit = kw.length >= 8 ? 2 : 1;
    const dist = levenshteinDistanceWithinLimit(token, kw, 2);
    if (dist > limit) continue;
    if (dist < bestDistance) {
      bestDistance = dist;
      bestKeyword = kw;
    }
  }

  return bestKeyword;
}

// ======================= EXEC block lint =======================

export function lintExecBlock(
  exec: {
    execStartOff: number;
    execTokenLen: number;
    subtypeTok?: { upper: string; start: number; end: number };
    firstArgTok?: { upper: string; start: number; end: number };
    tokens: { upper: string; start: number; end: number }[];
  },
  diags: GenDiag[]
) {
  if (!exec.subtypeTok) {
    diags.push({
      startOff: exec.execStartOff,
      endOff: exec.execStartOff + exec.execTokenLen,
      severity: DiagnosticSeverity.Error,
      code: "EXEC_MISSING_TYPE",
      message: "EXEC ohne Typ. Erwartet z. B. DLI, CICS oder SQL.",
    });
    return;
  }

  const subtype = exec.subtypeTok.upper;

  if (subtype === "DLI") {
    if (!exec.firstArgTok) {
      diags.push({
        startOff: exec.subtypeTok.start,
        endOff: exec.subtypeTok.end,
        severity: DiagnosticSeverity.Error,
        code: "EXEC_DLI_MISSING_CALL",
        message: "EXEC DLI ohne IMS-Aufruf (z. B. GU, GN, GNP, GHU, ISRT, REPL).",
      });
      return;
    }

    // IBM DLI request names (batch/BMP + DBCTL/CICS variants).
    const knownDliCalls = new Set([
      "GU", "GN", "GNP", "GHU", "GHN", "GHNP",
      "ISRT", "REPL", "DLET", "PURG", "LOAD",
      "CHKP", "SYMCHKP", "XRST", "SYNC", "DEQ",
      "ROLL", "ROLB", "ROLS", "STAT", "LOG",
      "INIT", "TERM", "PCB", "SETS", "POS",
      "GMSG", "ICMD", "RCMD", "CMD", "GCMD",
      "CHNG", "GSCD", "RETRIEVE", "QUERY",
      "ACCEPT", "REFRESH", "SCHD"
    ]);

    const req = resolveDliRequest(exec.tokens);
    const requestForLookup = req.request ?? exec.firstArgTok.upper;
    if (!knownDliCalls.has(requestForLookup)) {
      diags.push({
        startOff: exec.firstArgTok.start,
        endOff: exec.firstArgTok.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_UNKNOWN_CALL",
        message: `EXEC DLI mit unbekanntem IMS-Aufruf: ${exec.firstArgTok.upper}.`,
      });
    }

    if (req.request) {
      validateDliClausesByRequest(req.request, req.requestTokenCount, exec.tokens, diags);
    }

    const usingIdx = exec.tokens.findIndex((t) => t.upper === "USING");
    if (usingIdx >= 0) {
      const afterUsing = exec.tokens[usingIdx + 1];
      if (!afterUsing) {
        diags.push({
          startOff: exec.tokens[usingIdx].start,
          endOff: exec.tokens[usingIdx].end,
          severity: DiagnosticSeverity.Error,
          code: "EXEC_DLI_USING_MISSING_TARGET",
          message: "EXEC DLI USING ohne Ziel. Erwartet z. B. PCB(n).",
        });
      } else if (afterUsing.upper === "PSB") {
        diags.push({
          startOff: afterUsing.start,
          endOff: afterUsing.end,
          severity: DiagnosticSeverity.Error,
          code: "EXEC_DLI_USING_PSB_INVALID",
          message: "USING PSB(...) ist in EXEC DLI ungueltig. Verwende USING PCB(...) oder AIB(...).",
        });
      } else if (afterUsing.upper !== "PCB" && afterUsing.upper !== "AIB") {
        diags.push({
          startOff: afterUsing.start,
          endOff: afterUsing.end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_USING_UNKNOWN_TARGET",
          message: `EXEC DLI USING mit unbekanntem Ziel: ${afterUsing.upper}. Erwartet typischerweise PCB oder AIB.`,
        });
      }
    }
    return;
  }

  if (subtype === "CICS" && !exec.firstArgTok) {
    diags.push({
      startOff: exec.subtypeTok.start,
      endOff: exec.subtypeTok.end,
      severity: DiagnosticSeverity.Error,
      code: "EXEC_CICS_MISSING_COMMAND",
      message: "EXEC CICS ohne Kommando (z. B. LINK, XCTL, READ, WRITE, RETURN).",
    });
  }
}

function resolveDliRequest(tokens: { upper: string; start: number; end: number }[]): { request?: string; requestTokenCount: number } {
  // tokens layout in EXEC block: [DLI, <request...>, <clauses...>]
  const t1 = tokens[1]?.upper;
  const t2 = tokens[2]?.upper;
  const t3 = tokens[3]?.upper;
  const t4 = tokens[4]?.upper;
  const t5 = tokens[5]?.upper;

  if (!t1) return { requestTokenCount: 0 };

  if (t1 === "GET") {
    if (t2 === "UNIQUE") return { request: "GU", requestTokenCount: 2 };
    if (t2 === "NEXT" && t3 === "IN" && t4 === "PARENT") return { request: "GNP", requestTokenCount: 4 };
    if (t2 === "NEXT") return { request: "GN", requestTokenCount: 2 };
    if (t2 === "HOLD" && t3 === "UNIQUE") return { request: "GHU", requestTokenCount: 3 };
    if (t2 === "HOLD" && t3 === "NEXT" && t4 === "IN" && t5 === "PARENT") {
      return { request: "GHNP", requestTokenCount: 5 };
    }
    if (t2 === "HOLD" && t3 === "NEXT") return { request: "GHN", requestTokenCount: 3 };
    return { requestTokenCount: 1 };
  }

  if (t1 === "INSERT") return { request: "ISRT", requestTokenCount: 1 };
  if (t1 === "REPLACE") return { request: "REPL", requestTokenCount: 1 };
  if (t1 === "DELETE") return { request: "DLET", requestTokenCount: 1 };
  if (t1 === "CHECKPOINT") return { request: "CHKP", requestTokenCount: 1 };
  if (t1 === "SYMBOLIC" && t2 === "CHECKPOINT") return { request: "SYMCHKP", requestTokenCount: 2 };

  return { request: t1, requestTokenCount: 1 };
}

function validateDliClausesByRequest(
  request: string,
  requestTokenCount: number,
  tokens: { upper: string; start: number; end: number }[],
  diags: GenDiag[]
) {
  const clauseKeywords = new Set([
    "USING",
    "VARIABLE",
    "SEGMENT",
    "INTO",
    "FROM",
    "SEGLENGTH",
    "WHERE",
    "FIELDLENGTH",
    "KEYS",
    "KEYLENGTH",
    "SETPARENT",
    "VSAM",
    "FORMATTED",
    "LENGTH",
  ]);

  const requestSpecs: Record<string, { allowed: Set<string>; required: Set<string> }> = {
    GU: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    GN: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    GNP: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    GHU: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    GHN: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    GHNP: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    ISRT: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "FROM", "SEGLENGTH", "KEYS", "KEYLENGTH", "WHERE", "FIELDLENGTH"]),
      required: new Set(["USING", "SEGMENT"]),
    },
    REPL: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "FROM", "SEGLENGTH"]),
      required: new Set(["USING", "SEGMENT"]),
    },
    DLET: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "FROM", "SEGLENGTH", "KEYS", "KEYLENGTH", "WHERE", "FIELDLENGTH"]),
      required: new Set(["USING", "SEGMENT"]),
    },
    STAT: {
      allowed: new Set(["USING", "INTO", "VSAM", "FORMATTED", "LENGTH"]),
      required: new Set(["USING"]),
    },
    QUERY: {
      allowed: new Set(["USING"]),
      required: new Set(["USING"]),
    },
    TERM: {
      allowed: new Set<string>(),
      required: new Set<string>(),
    },
  };

  const spec = requestSpecs[request];
  if (!spec) return;

  const clauseStart = Math.min(tokens.length, 1 + Math.max(0, requestTokenCount));
  const seen = new Set<string>();
  const clauseKeywordList = Array.from(clauseKeywords);

  for (let i = clauseStart; i < tokens.length; i++) {
    const t = tokens[i];
    if (clauseKeywords.has(t.upper)) {
      seen.add(t.upper);

      if (!spec.allowed.has(t.upper)) {
        diags.push({
          startOff: t.start,
          endOff: t.end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_CLAUSE_NOT_ALLOWED",
          message: `Klausel ${t.upper} ist fuer EXEC DLI ${request} nicht vorgesehen.`,
        });
      }
      continue;
    }

    const suggestion = suggestDliClauseKeyword(t.upper, clauseKeywordList);
    if (suggestion && isLikelyDliClausePosition(tokens, i, clauseStart, clauseKeywords)) {
      diags.push({
        startOff: t.start,
        endOff: t.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_CLAUSE_UNKNOWN",
        message: `Unbekannte DLI-Klausel: ${t.upper}. Meintest du ${suggestion}?`,
      });
    }
  }

  for (const needed of spec.required) {
    if (!seen.has(needed)) {
      diags.push({
        startOff: tokens[1]?.start ?? tokens[0]?.start ?? 0,
        endOff: tokens[1]?.end ?? tokens[0]?.end ?? 1,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_CLAUSE_MISSING",
        message: `Bei EXEC DLI ${request} fehlt die erwartete Klausel ${needed}.`,
      });
    }
  }

  validateDliWhereClause(request, clauseStart, tokens, clauseKeywords, diags);
}

function isLikelyDliClausePosition(
  tokens: { upper: string; start: number; end: number }[],
  idx: number,
  clauseStart: number,
  clauseKeywords: Set<string>
): boolean {
  if (idx <= clauseStart) return true;

  const next = tokens[idx + 1]?.upper;

  const win = 3;
  const left = Math.max(clauseStart, idx - win);
  const right = Math.min(tokens.length - 1, idx + win);

  for (let i = idx - 1; i >= left; i--) {
    if (clauseKeywords.has(tokens[i].upper)) return true;
  }
  for (let i = idx + 1; i <= right; i++) {
    if (clauseKeywords.has(tokens[i].upper)) return true;
  }

  if (next && /^[0-9]+$/.test(next)) return true;

  return false;
}

function suggestDliClauseKeyword(token: string, clauseKeywords: string[]): string | undefined {
  if (token.length < 4) return undefined;
  if (!/^[A-Z]+$/.test(token)) return undefined;
  if (token.includes("-")) return undefined;
  if (/^[0-9]+$/.test(token)) return undefined;

  let bestKeyword: string | undefined;
  let bestDistance = Number.POSITIVE_INFINITY;

  for (const kw of clauseKeywords) {
    const limit = kw.length >= 8 ? 2 : 1;
    const dist = levenshteinDistanceWithinLimit(token, kw, 2);
    if (dist > limit) continue;
    if (dist < bestDistance) {
      bestDistance = dist;
      bestKeyword = kw;
    }
  }

  return bestKeyword;
}

function validateDliWhereClause(
  request: string,
  clauseStart: number,
  tokens: { upper: string; start: number; end: number }[],
  clauseKeywords: Set<string>,
  diags: GenDiag[]
) {
  type SegmentWhereState = {
    whereIdxs: number[];
    firstFieldLengthIdx: number;
  };

  const segmentStates: SegmentWhereState[] = [{ whereIdxs: [], firstFieldLengthIdx: -1 }];
  let currentState = segmentStates[0];

  for (let i = clauseStart; i < tokens.length; i++) {
    const u = tokens[i].upper;

    if (u === "SEGMENT" && i > clauseStart) {
      currentState = { whereIdxs: [], firstFieldLengthIdx: -1 };
      segmentStates.push(currentState);
      continue;
    }

    if (u === "WHERE") currentState.whereIdxs.push(i);
    if (u === "FIELDLENGTH" && currentState.firstFieldLengthIdx < 0) currentState.firstFieldLengthIdx = i;
  }

  const allWhereIdxs: number[] = [];

  for (const state of segmentStates) {
    allWhereIdxs.push(...state.whereIdxs);

    if (state.whereIdxs.length === 0) {
      if (state.firstFieldLengthIdx >= 0) {
        const t = tokens[state.firstFieldLengthIdx];
        diags.push({
          startOff: t.start,
          endOff: t.end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_FIELDLENGTH_WITHOUT_WHERE",
          message: `FIELDLENGTH ohne WHERE bei EXEC DLI ${request}.`,
        });
      }
      continue;
    }

    if (state.firstFieldLengthIdx >= 0 && state.firstFieldLengthIdx < state.whereIdxs[0]) {
      const t = tokens[state.firstFieldLengthIdx];
      diags.push({
        startOff: t.start,
        endOff: t.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_FIELDLENGTH_BEFORE_WHERE",
        message: `FIELDLENGTH steht vor WHERE bei EXEC DLI ${request}.`,
      });
    }

    for (let i = 1; i < state.whereIdxs.length; i++) {
      const t = tokens[state.whereIdxs[i]];
      diags.push({
        startOff: t.start,
        endOff: t.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_WHERE_DUPLICATE",
        message: `Mehrfaches WHERE bei EXEC DLI ${request}.`,
      });
    }
  }

  for (const whereIdx of allWhereIdxs) {
    const whereTok = tokens[whereIdx];
    const nextClauseIdx = findNextClauseKeywordIndex(tokens, whereIdx + 1, clauseKeywords);
    if (!hasNonClauseToken(tokens, whereIdx + 1, nextClauseIdx, clauseKeywords)) {
      diags.push({
        startOff: whereTok.start,
        endOff: whereTok.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_WHERE_EMPTY",
        message: `WHERE ohne erkennbaren Suchausdruck bei EXEC DLI ${request}.`,
      });
    }
  }
}

function findNextClauseKeywordIndex(
  tokens: { upper: string; start: number; end: number }[],
  startIdx: number,
  clauseKeywords: Set<string>
): number {
  for (let i = startIdx; i < tokens.length; i++) {
    if (clauseKeywords.has(tokens[i].upper)) return i;
  }
  return tokens.length;
}

function hasNonClauseToken(
  tokens: { upper: string; start: number; end: number }[],
  startIdx: number,
  endIdx: number,
  clauseKeywords: Set<string>
): boolean {
  for (let i = startIdx; i < endIdx; i++) {
    if (!clauseKeywords.has(tokens[i].upper)) return true;
  }
  return false;
}

// ======================= Token scanning =======================

export function scanLintTokens(text: string): LintTok[] {
  const toks: LintTok[] = [];
  const n = text.length;
  let i = 0;

  const isWordChar = (ch: string) => /[A-Za-z0-9-]/.test(ch);
  const isDigit = (ch: string) => /[0-9]/.test(ch);

  while (i < n) {
    const ch = text[i];

    // Inline-Kommentar im free-format: *>
    if (ch === "*" && i + 1 < n && text[i + 1] === ">") {
      i += 2;
      while (i < n && text[i] !== "\n") i++;
      continue;
    }

    // Stringliterale fuer die Keyword-Erkennung auslassen.
    if (ch === "'" || ch === "\"") {
      const quote = ch;
      i++;
      while (i < n) {
        const c = text[i];
        // Keep lint token scanning line-local to avoid false carry-over with
        // fixed-format continuation string quoting.
        if (c === "\n" || c === "\r") break;

        if (c === quote) {
          // COBOL escaped quote: '' oder ""
          if (i + 1 < n && text[i + 1] === quote) {
            i += 2;
            continue;
          }
          i++;
          break;
        }
        i++;
      }
      continue;
    }

    if (ch === ".") {
      const prev = i > 0 ? text[i - 1] : "";
      const next = i + 1 < n ? text[i + 1] : "";
      // Dezimalpunkt nicht als Statement-Terminierung werten.
      if (!(isDigit(prev) && isDigit(next))) {
        toks.push({ kind: "dot", start: i, end: i + 1 });
      }
      i++;
      continue;
    }

    if (isWordChar(ch)) {
      const start = i;
      i++;
      while (i < n && isWordChar(text[i])) i++;
      const upper = text.slice(start, i).toUpperCase();
      toks.push({ kind: "word", upper, start, end: i });
      continue;
    }

    i++;
  }

  return toks;
}

export function collectExecDliRanges(text: string): OffsetRange[] {
  const toks = scanLintTokens(text);
  const out: OffsetRange[] = [];

  let execStart: number | undefined;
  let subtype: string | undefined;

  for (const t of toks) {
    if (t.kind !== "word") continue;
    const u = t.upper;

    if (execStart === undefined) {
      if (u === "EXEC") {
        execStart = t.start;
        subtype = undefined;
      }
      continue;
    }

    if (!subtype) {
      subtype = u;
      continue;
    }

    if (u === "END-EXEC") {
      if (subtype === "DLI") out.push({ start: execStart, end: t.end });
      execStart = undefined;
      subtype = undefined;
    }
  }

  if (execStart !== undefined && subtype === "DLI") out.push({ start: execStart, end: text.length });
  return out;
}

export function overlapsAnyRange(start: number, end: number, ranges: OffsetRange[]): boolean {
  for (const r of ranges) {
    if (start < r.end && end > r.start) return true;
  }
  return false;
}

export function closeBlocksByPeriod(
  stack: { kind: "IF" | "EVALUATE" | "EXEC"; startOff: number; tokenLen: number }[],
  diags: GenDiag[]
) {
  while (stack.length > 0) {
    const top = stack[stack.length - 1];
    if (top.kind === "EXEC") return;

    const f = stack.pop()!;
    const expected = f.kind === "IF" ? "END-IF" : "END-EVALUATE";

    diags.push({
      startOff: f.startOff,
      endOff: f.startOff + f.tokenLen,
      severity: DiagnosticSeverity.Warning,
      code: "BLOCK_CLOSED_BY_PERIOD",
      message: `${f.kind} endet durch '.' statt ${expected}. Bitte ${expected} verwenden.`,
    });
  }
}

function popExpected(stack: { kind: string }[], expected: string): boolean {
  if (stack.length === 0) return false;
  const top = stack[stack.length - 1];
  if (top.kind === expected) {
    stack.pop();
    return true;
  }
  return false;
}
