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
import { DefinitionIndex } from "./definition";

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
  type ExecTok = { upper: string; start: number; end: number; inParen?: boolean };
  type ExecLintState = {
    execStartOff: number;
    execTokenLen: number;
    subtypeTok?: ExecTok;
    firstArgTok?: ExecTok;
    tokens: ExecTok[];
    parenDepth: number;
    lastPos: number;
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
      // Scan raw text between lastPos and this token's start for ( and )
      for (let p = execState.lastPos; p < start; p++) {
        if (text[p] === "(") execState.parenDepth++;
        else if (text[p] === ")") { if (execState.parenDepth > 0) execState.parenDepth--; }
      }
      execState.lastPos = end;

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

      const inParen = execState.parenDepth > 0;
      if (!execState.subtypeTok) {
        execState.subtypeTok = { upper: u, start, end, inParen };
      } else if (!execState.firstArgTok) {
        execState.firstArgTok = { upper: u, start, end, inParen };
      }
      if (execState.tokens.length < 200) {
        execState.tokens.push({ upper: u, start, end, inParen });
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
      execState = { execStartOff: start, execTokenLen: end - start, tokens: [], parenDepth: 0, lastPos: end };
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
  let inExecBlock = false;

  const knownStatementLeaders = new Set([
    "ACCEPT", "ADD", "ALTER", "BASIS", "CALL", "CANCEL", "CLOSE", "COMPUTE", "CONTINUE",
    "COPY", "DELETE", "DISPLAY", "DIVIDE", "ELSE", "END-ACCEPT", "END-ADD",
    "END-CALL", "END-COMPUTE", "END-DELETE", "END-DISPLAY", "END-DIVIDE",
    "END-EVALUATE", "END-IF", "END-MULTIPLY", "END-PERFORM", "END-READ",
    "END-RECEIVE", "END-RETURN", "END-REWRITE", "END-SEARCH", "END-START",
    "END-STRING", "END-SUBTRACT", "END-UNSTRING", "END-WRITE", "END-EXEC", "ENTRY",
    "EVALUATE", "EXEC", "EXIT", "GOBACK", "GO", "IF", "INITIALIZE", "INITIATE",
    "INSERT", "INSPECT", "MERGE", "MOVE", "MULTIPLY", "NEXT", "OPEN", "PERFORM",
    "READ", "RELEASE", "REPLACE", "RETURN", "REWRITE", "SEARCH", "SERVICE", "SET", "SORT", "START",
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
    ["NOTE", "Kommentarzeile mit *> (free) oder * in Spalte 7 (fixed)"],
  ]);

  const ibmExtensionKeywords = new Map<string, string>([
    ["READY", "IBM-Erweiterung: In Standard-COBOL-85 nicht zulaessig, aber in VSE/ESA unterstuetzt."],
    ["RESET", "IBM-Erweiterung: In Standard-COBOL-85 nicht zulaessig, aber in VSE/ESA unterstuetzt."],
    ["TRACE", "IBM-Erweiterung: In Standard-COBOL-85 nicht zulaessig, aber in VSE/ESA unterstuetzt."],
  ]);

  const hasStandaloneToken = (text: string, token: string): boolean => {
    const escaped = token.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    const pattern = new RegExp(`(^|[^A-Z0-9-])${escaped}([^A-Z0-9-]|$)`, "i");
    return pattern.test(text);
  };

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
      const hasExecKeyword = hasStandaloneToken(trimmed, "EXEC");
      const hasEndExecKeyword = hasStandaloneToken(trimmed, "END-EXEC");
      if (inExecBlock || hasExecKeyword || hasEndExecKeyword) {
        if (hasExecKeyword && !hasEndExecKeyword) inExecBlock = true;
        if (hasEndExecKeyword) inExecBlock = false;

        prevProcedureLine = {
          indent: trimmedStartLen,
          startsKnownOrClause: true,
          hasSeparatorPeriod: hasSeparatorPeriodOutsideLiterals(trimmed),
        };
        off += line.length + 1;
        continue;
      }

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

          const ibmExtension = ibmExtensionKeywords.get(token);
          if (ibmExtension) {
            diags.push({
              startOff: stmtStartOff,
              endOff: stmtStartOff + token.length,
              severity: DiagnosticSeverity.Warning,
              code: "IBM_EXTENSION_KEYWORD",
              message: ibmExtension,
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

type ExecToken = { upper: string; start: number; end: number; inParen?: boolean };

export type CicsCommandSpec = {
  allowedOptions: Set<string>;
  requiredAll?: string[];
  requiredOneOf?: string[][];
};

type CicsCommandMatch = {
  command: string;
  commandTokenCount: number;
};

export const CICS_PRIMARY_COMMANDS = new Set([
  "ABEND",
  "ACQUIRE",
  "ADD",
  "ADDRESS",
  "ALLOCATE",
  "ASKTIME",
  "ASSIGN",
  "BIF",
  "BUILD",
  "CANCEL",
  "CHANGE",
  "CHECK",
  "CONNECT",
  "CONVERSE",
  "CONVERTTIME",
  "DEFINE",
  "DELAY",
  "DELETE",
  "DELETEQ",
  "DEQ",
  "DOCUMENT",
  "DUMP",
  "ENDBR",
  "ENDBROWSE",
  "ENQ",
  "ENTER",
  "EXTRACT",
  "FETCH",
  "FORCE",
  "FORMATTIME",
  "FREE",
  "FREEMAIN",
  "GDS",
  "GET",
  "GET64",
  "GETMAIN",
  "GETNEXT",
  "HANDLE",
  "IGNORE",
  "INQUIRE",
  "INVOKE",
  "ISSUE",
  "LINK",
  "LOAD",
  "MONITOR",
  "MOVE",
  "POINT",
  "POP",
  "POST",
  "PURGE",
  "PUSH",
  "PUT",
  "PUT64",
  "QUERY",
  "READ",
  "READNEXT",
  "READPREV",
  "READQ",
  "RECEIVE",
  "RELEASE",
  "REMOVE",
  "REQUEST",
  "RESET",
  "RESETBR",
  "RESUME",
  "RETRIEVE",
  "RETURN",
  "REWIND",
  "REWRITE",
  "ROUTE",
  "RUN",
  "SEND",
  "SIGNAL",
  "SIGNOFF",
  "SIGNON",
  "SOAPFAULT",
  "SPOOLCLOSE",
  "SPOOLOPEN",
  "SPOOLREAD",
  "SPOOLWRITE",
  "START",
  "STARTBR",
  "STARTBROWSE",
  "SUSPEND",
  "SYNCPOINT",
  "TEST",
  "TRANSFORM",
  "UNLOCK",
  "UPDATE",
  "VERIFY",
  "WAIT",
  "WAITCICS",
  "WEB",
  "WRITE",
  "WRITEQ",
  "WSACONTEXT",
  "WSAEPR",
  "XCTL",
]);

export const CICS_KNOWN_CONDITIONS = new Set([
  "CBIDERR",
  "CHANNELERR",
  "CINVREQ",
  "CONTAINERERR",
  "COREQIDERR",
  "DUPKEY",
  "DUPREC",
  "ENDDATA",
  "ENDFILE",
  "ENDINPT",
  "ENDOUTPT",
  "ENQBUSY",
  "ENVDEFERR",
  "EOC",
  "EODS",
  "ERROR",
  "EXPIRED",
  "FILENOTFOUND",
  "ILLOGIC",
  "INBFMH",
  "INCOMPLETE",
  "INQBUSY",
  "INVERRTERM",
  "INVEST",
  "INVMPSZ",
  "INVREQ",
  "IOERR",
  "ISCINVREQ",
  "ITEMERR",
  "LENGERR",
  "LINKABEND",
  "MAPFAIL",
  "NATLANGERR",
  "NOSTART",
  "NOSPACE",
  "NOSPOOL",
  "NOSTG",
  "NOTALLOC",
  "NOTAUTH",
  "NOTFND",
  "NORMAL",
  "NOTOPEN",
  "PGMIDERR",
  "QBUSY",
  "QIDERR",
  "QZERO",
  "READONLY",
  "RECORDBUSY",
  "REQIDERR",
  "ROLLEDBACK",
  "SESSIONERR",
  "SIGNOFF",
  "SPOLBUSY",
  "SPOLERR",
  "SUPPRESSED",
  "SYSBUSY",
  "SYSIDERR",
  "TASKIDERR",
  "TERMERR",
  "TERMIDERR",
  "TIMERERR",
  "TRANSIDERR",
  "TSIOERR",
  "UNEXPIN",
  "WRBRK",
]);

// CICS option synonyms: older CICS (especially VSE) used DATASET instead of FILE.
// Both are accepted; the canonical name is listed first.
const CICS_OPTION_SYNONYMS: Record<string, string> = {
  DATASET: "FILE",
};

export const CICS_COMMAND_SPECS: Record<string, CicsCommandSpec> = {
  // ── File Control ──────────────────────────────────────────────────────
  READ: {
    allowedOptions: new Set([
      "FILE", "DATASET", "INTO", "SET", "RIDFLD", "RBA", "RRN", "XRBA", "KEYLENGTH",
      "GENERIC", "GTEQ", "EQUAL", "TOKEN", "LENGTH", "UPDATE", "NOSUSPEND",
      "CONSISTENT", "SYSID",
    ]),
    requiredAll: ["FILE"],
    requiredOneOf: [["INTO", "SET"]],
  },
  READNEXT: {
    allowedOptions: new Set([
      "FILE", "DATASET", "INTO", "SET", "RIDFLD", "RBA", "RRN", "XRBA", "KEYLENGTH",
      "REQID", "TOKEN", "LENGTH", "UPDATE", "NOSUSPEND", "SYSID",
    ]),
    requiredAll: ["FILE"],
    requiredOneOf: [["INTO", "SET"]],
  },
  READPREV: {
    allowedOptions: new Set([
      "FILE", "DATASET", "INTO", "SET", "RIDFLD", "RBA", "RRN", "XRBA", "KEYLENGTH",
      "REQID", "TOKEN", "LENGTH", "UPDATE", "NOSUSPEND", "SYSID",
    ]),
    requiredAll: ["FILE"],
    requiredOneOf: [["INTO", "SET"]],
  },
  STARTBR: {
    allowedOptions: new Set([
      "FILE", "DATASET", "RIDFLD", "RBA", "RRN", "XRBA", "KEYLENGTH", "GENERIC", "GTEQ",
      "EQUAL", "REQID", "DEBKEY", "DEBREC", "NOSUSPEND", "SYSID",
    ]),
    requiredAll: ["FILE"],
  },
  RESETBR: {
    allowedOptions: new Set([
      "FILE", "DATASET", "RIDFLD", "RBA", "RRN", "XRBA", "KEYLENGTH", "GENERIC", "REQID",
      "NOSUSPEND", "SYSID",
    ]),
    requiredAll: ["FILE"],
  },
  ENDBR: {
    allowedOptions: new Set(["FILE", "DATASET", "REQID", "SYSID"]),
    requiredAll: ["FILE"],
  },
  WRITE: {
    allowedOptions: new Set([
      "FILE", "DATASET", "FROM", "RIDFLD", "RBA", "RRN", "KEYLENGTH", "LENGTH",
      "MASSINSERT", "NOSUSPEND", "SYSID",
    ]),
    requiredAll: ["FILE", "FROM"],
  },
  REWRITE: {
    allowedOptions: new Set(["FILE", "DATASET", "FROM", "TOKEN", "LENGTH", "NOSUSPEND", "SYSID"]),
    requiredAll: ["FILE", "FROM"],
  },
  DELETE: {
    allowedOptions: new Set([
      "FILE", "DATASET", "RIDFLD", "RBA", "RRN", "XRBA", "KEYLENGTH", "GENERIC", "TOKEN",
      "NOSUSPEND", "SYSID",
    ]),
    requiredAll: ["FILE"],
    // RIDFLD etc. is optional — a DELETE after READ UPDATE deletes the held record
  },
  UNLOCK: {
    allowedOptions: new Set(["FILE", "DATASET", "TOKEN", "SYSID"]),
    requiredAll: ["FILE"],
  },

  // ── Program Control ───────────────────────────────────────────────────
  LINK: {
    allowedOptions: new Set([
      "PROGRAM", "COMMAREA", "LENGTH", "DATALENGTH", "CHANNEL", "INPUTMSG",
      "INPUTMSGLEN", "SYNCONRETURN", "SYSID",
    ]),
    requiredAll: ["PROGRAM"],
  },
  XCTL: {
    allowedOptions: new Set(["PROGRAM", "COMMAREA", "LENGTH", "CHANNEL", "SYSID"]),
    requiredAll: ["PROGRAM"],
  },
  RETURN: {
    allowedOptions: new Set([
      "TRANSID", "COMMAREA", "LENGTH", "CHANNEL", "IMMEDIATE", "INPUTMSG",
      "INPUTMSGLEN",
    ]),
  },
  LOAD: {
    allowedOptions: new Set(["PROGRAM", "SET", "LENGTH", "FLENGTH", "ENTRY", "HOLD"]),
    requiredAll: ["PROGRAM"],
  },
  RELEASE: {
    allowedOptions: new Set(["PROGRAM"]),
    requiredAll: ["PROGRAM"],
  },

  // ── Interval Control ──────────────────────────────────────────────────
  START: {
    allowedOptions: new Set([
      "TRANSID", "TERMID", "INTERVAL", "TIME", "AFTER", "AT", "REQID", "FROM",
      "LENGTH", "RTRANSID", "RTERMID", "QUEUE", "NOCHECK", "SYSID",
    ]),
    requiredAll: ["TRANSID"],
  },
  CANCEL: {
    allowedOptions: new Set(["REQID", "SYSID", "TRANSID"]),
  },
  DELAY: {
    allowedOptions: new Set([
      "INTERVAL", "TIME", "FOR", "HOURS", "MINUTES", "SECONDS", "MILLISECS",
      "UNTIL", "REQID",
    ]),
  },
  ASKTIME: {
    allowedOptions: new Set(["ABSTIME"]),
  },
  FORMATTIME: {
    allowedOptions: new Set([
      "ABSTIME", "DATE", "FULLDATE", "DATEFORM", "DATESEP", "DATESTRING",
      "STRINGZONE", "DAYCOUNT", "DAYOFMONTH", "DAYOFWEEK", "DDMMYY",
      "DDMMYYYY", "MILLISECONDS", "MMDDYY", "MMDDYYYY", "MONTHOFYEAR",
      "STRINGFORMAT", "TIME", "TIMESEP", "YEAR", "YYDDD", "YYDDMM", "YYMMDD",
      "YYYYDDD", "YYYYDDMM", "YYYYMMDD",
    ]),
    requiredAll: ["ABSTIME"],
  },
  RETRIEVE: {
    allowedOptions: new Set(["INTO", "SET", "LENGTH", "RTRANSID", "RTERMID", "QUEUE", "WAIT"]),
    requiredOneOf: [["INTO", "SET"]],
  },

  // ── Storage Control ───────────────────────────────────────────────────
  GETMAIN: {
    allowedOptions: new Set([
      "SET", "FLENGTH", "LENGTH", "BELOW", "INITIMG", "EXECUTABLE", "SHARED",
      "NOSUSPEND", "USERDATAKEY", "CICSDATAKEY",
    ]),
    requiredAll: ["SET"],
    requiredOneOf: [["FLENGTH", "LENGTH"]],
  },
  FREEMAIN: {
    allowedOptions: new Set(["DATA", "DATAPOINTER"]),
    requiredOneOf: [["DATA", "DATAPOINTER"]],
  },

  // ── Task Control ──────────────────────────────────────────────────────
  ABEND: {
    allowedOptions: new Set(["ABCODE", "CANCEL", "NODUMP"]),
  },
  SUSPEND: {
    allowedOptions: new Set([]),
  },
  ENQ: {
    allowedOptions: new Set(["RESOURCE", "LENGTH", "UOW", "TASK", "MAXLIFETIME", "NOSUSPEND"]),
    requiredAll: ["RESOURCE"],
  },
  DEQ: {
    allowedOptions: new Set(["RESOURCE", "LENGTH", "UOW", "TASK", "MAXLIFETIME"]),
    requiredAll: ["RESOURCE"],
  },

  // ── Event/Timer Control ───────────────────────────────────────────────
  POST: {
    allowedOptions: new Set([
      "INTERVAL", "TIME", "SET", "REQID", "AFTER", "AT", "HOURS", "MINUTES",
      "SECONDS",
    ]),
    requiredAll: ["SET"],
  },
  "WAIT EVENT": {
    allowedOptions: new Set(["ECADDR"]),
    requiredAll: ["ECADDR"],
  },

  // ── BMS ────────────────────────────────────────────────────────────────
  "SEND MAP": {
    allowedOptions: new Set([
      "MAP", "MAPSET", "FROM", "LENGTH", "DATAONLY", "MAPONLY", "ERASE",
      "ALARM", "FREEKB", "FRSET", "CURSOR", "WAIT", "LAST", "PAGING",
    ]),
    // MAP is implicit — it's the command name itself (SEND MAP)
  },
  "SEND TEXT": {
    allowedOptions: new Set([
      "FROM", "LENGTH", "ERASE", "ALARM", "FREEKB", "WAIT", "LAST", "PAGING",
    ]),
    requiredAll: ["FROM"],
  },
  "SEND CONTROL": {
    allowedOptions: new Set(["ERASE", "ALARM", "FREEKB", "FRSET"]),
  },
  "SEND PAGE": {
    allowedOptions: new Set([
      "RELEASE", "TRANSID", "TRAILER", "RETAIN", "AUTOPAGE", "NOAUTOPAGE",
      "OPERPURGE", "LAST", "FMHPARM",
    ]),
  },
  "RECEIVE MAP": {
    allowedOptions: new Set(["MAP", "MAPSET", "INTO", "LENGTH", "ASIS", "NOTRUNCATE"]),
    // MAP is implicit — it's the command name itself (RECEIVE MAP)
    // INTO is optional when the mapset uses STORAGE=AUTO
  },
  "PURGE MESSAGE": {
    allowedOptions: new Set([]),
  },

  // ── Queue: Transient Data (TD) ────────────────────────────────────────
  "READQ TD": {
    allowedOptions: new Set(["QUEUE", "INTO", "SET", "LENGTH"]),
    requiredAll: ["QUEUE"],
    requiredOneOf: [["INTO", "SET"]],
  },
  "WRITEQ TD": {
    allowedOptions: new Set(["QUEUE", "FROM", "LENGTH"]),
    requiredAll: ["QUEUE", "FROM"],
  },
  "DELETEQ TD": {
    allowedOptions: new Set(["QUEUE"]),
    requiredAll: ["QUEUE"],
  },

  // ── Queue: Temporary Storage (TS) ─────────────────────────────────────
  "READQ TS": {
    allowedOptions: new Set(["QUEUE", "INTO", "SET", "LENGTH", "ITEM", "NEXT", "NUMITEMS", "SYSID"]),
    requiredAll: ["QUEUE"],
    requiredOneOf: [["INTO", "SET"]],
  },
  "WRITEQ TS": {
    allowedOptions: new Set([
      "QUEUE", "FROM", "LENGTH", "ITEM", "REWRITE", "NOSUSPEND", "MAIN",
      "AUXILIARY", "SYSID",
    ]),
    requiredAll: ["QUEUE", "FROM"],
  },
  "DELETEQ TS": {
    allowedOptions: new Set(["QUEUE", "SYSID"]),
    requiredAll: ["QUEUE"],
  },

  // ── Exception Handling ────────────────────────────────────────────────
  "HANDLE ABEND": {
    allowedOptions: new Set(["PROGRAM", "LABEL", "CANCEL", "RESET"]),
    requiredOneOf: [["PROGRAM", "LABEL", "CANCEL", "RESET"]],
  },
  "PUSH HANDLE": {
    allowedOptions: new Set([]),
  },
  "POP HANDLE": {
    allowedOptions: new Set([]),
  },

  // ── Sync Control ──────────────────────────────────────────────────────
  SYNCPOINT: {
    allowedOptions: new Set([]),
  },
  "SYNCPOINT ROLLBACK": {
    allowedOptions: new Set([]),
  },

  // ── Terminal Control ──────────────────────────────────────────────────
  "ISSUE ERASE": {
    allowedOptions: new Set([]),
  },
  "ISSUE ERASEAUP": {
    allowedOptions: new Set([]),
  },

  // ── System Info ───────────────────────────────────────────────────────
  ADDRESS: {
    allowedOptions: new Set(["ACEE", "COMMAREA", "CWA", "EIB", "TCTUA", "TWA"]),
  },
  ASSIGN: {
    allowedOptions: new Set([
      "ABCODE", "ABDUMP", "ABOFFSET", "ABPROGRAM", "ACTIVITY", "ACTIVITYID",
      "ALTSCRNHT", "ALTSCRNWD", "APLKYBD", "APLTEXT", "APPLICATION", "APPLID",
      "ASRAINTRPT", "ASRAKEY", "ASRAPSW", "ASRAREGS", "ASRASPC", "ASRASTG",
      "BRIDGE", "BTRANS", "CHANNEL", "CMDSEC", "COLOR", "CWALENG",
      "DEFSCRNHT", "DEFSCRNWD", "DELIMITER", "DESTCOUNT", "DESTID",
      "DESTIDLENG", "DSSCS", "DS3270", "ERRORMSG", "ERRORMSGLEN", "EWASUPP",
      "EXTDS", "FACILITY", "FCI", "GCHARS", "GCODES", "GMMI", "GMEXITOPT",
      "HILIGHT", "INITPARM", "INITPARMLEN", "INPARTN", "INPUTMSGLEN",
      "INVOKINGPROG", "KATAKANA", "LANGINUSE", "LDCMNEM", "LDCNUM",
      "LINKLEVEL", "LOCALCCSID", "MAPCOLUMN", "MAPHEIGHT", "MAPLINE",
      "MAPWIDTH", "MSRCONTROL", "NATLANGINUSE", "NETNAME", "NEXTTRANSID",
      "NUMTAB", "OPCLASS", "OPERKEYS", "OPID", "OPSECURITY", "ORGABCODE",
      "OUTLINE", "PAGENUM", "PARTNPAGE", "PARTNS", "PARTNSET", "PLATFORM",
      "PRINSYSID", "PROCESSTYPE", "PROGRAM", "PS", "QNAME", "RESSEC",
      "RESTART", "RETURNPROG", "SCRNHT", "SCRNWD", "SIGDATA", "SOSI",
      "STARTCODE", "STATIONID", "SYSID", "TASKPRIORITY", "TCTUALENG",
      "TELLERID", "TERMCODE", "TERMPRIORITY", "TEXTKYBD", "TEXTPRINT",
      "TNADDR", "TNIPFAMILY", "TNPORT", "TRANPRIORITY", "TWALENG",
      "UNATTEND", "USERID", "USERNAME", "USERPRIORITY", "VALIDATION",
    ]),
  },

  // ── Diagnostic ────────────────────────────────────────────────────────
  "DUMP TRANSACTION": {
    allowedOptions: new Set([
      "DUMPCODE", "FROM", "LENGTH", "FLENGTH", "COMPLETE", "TASK", "STORAGE",
      "PROGRAM", "TERMINAL", "TABLES", "FCT", "PCT", "PPT", "SIT", "TCT",
    ]),
    requiredAll: ["DUMPCODE"],
  },
  "ENTER TRACENUM": {
    allowedOptions: new Set(["FROM", "FROMLENGTH", "RESOURCE", "EXCEPTION"]),
  },
  "WRITE OPERATOR": {
    allowedOptions: new Set([
      "TEXT", "TEXTLENGTH", "ROUTECODES", "NUMROUTES", "ACTION", "CRITICAL",
      "EVENTUAL", "IMMEDIATE", "REPLY", "MAXLENGTH", "TIMEOUT",
    ]),
    requiredAll: ["TEXT"],
  },

  // ── Exception Handling (special commands — conditions/AID keys, no normal options) ──
  "HANDLE CONDITION": {
    allowedOptions: new Set([]),
  },
  "IGNORE CONDITION": {
    allowedOptions: new Set([]),
  },
  "HANDLE AID": {
    allowedOptions: new Set([
      "ANYKEY", "CLEAR", "CLRPARTN", "ENTER", "LIGHTPEN", "OPERID", "TRIGGER",
      "PA1", "PA2", "PA3",
      "PF1", "PF2", "PF3", "PF4", "PF5", "PF6", "PF7", "PF8", "PF9",
      "PF10", "PF11", "PF12", "PF13", "PF14", "PF15", "PF16", "PF17", "PF18",
      "PF19", "PF20", "PF21", "PF22", "PF23", "PF24",
    ]),
  },
};

const CICS_COMMAND_ALIASES = new Map<string, string>([
  ["SEND MAP MAPPINGDEV", "SEND MAP"],
  ["SEND TEXT MAPPED", "SEND TEXT"],
  ["RECEIVE MAP MAPPINGDEV", "RECEIVE MAP"],
]);

const CICS_ADDITIONAL_MULTIWORD_COMMANDS = [
  "HANDLE AID",
  "HANDLE CONDITION",
  "IGNORE CONDITION",
];

const CICS_COMMAND_MATCH_ORDER = buildCicsCommandMatchOrder();
const CICS_KNOWN_OPTION_KEYWORDS = buildKnownCicsOptionKeywords();

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
    return;
  }

  if (subtype === "CICS") {
    lintExecCicsBlock(exec, diags);
  }
}

function lintExecCicsBlock(
  exec: {
    execStartOff: number;
    execTokenLen: number;
    subtypeTok?: ExecToken;
    firstArgTok?: ExecToken;
    tokens: ExecToken[];
  },
  diags: GenDiag[]
) {
  if (!exec.firstArgTok) return;

  const match = resolveCicsCommand(exec.tokens);
  if (!match) {
    if (!CICS_PRIMARY_COMMANDS.has(exec.firstArgTok.upper)) {
      const suggestion = suggestCicsPrimaryCommand(exec.firstArgTok.upper);
      const hint = suggestion ? ` Meintest du ${suggestion}?` : "";
      diags.push({
        startOff: exec.firstArgTok.start,
        endOff: exec.firstArgTok.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_CICS_UNKNOWN_COMMAND",
        message: `Unbekanntes EXEC CICS Kommando: ${exec.firstArgTok.upper}.${hint}`,
      });
    }
    return;
  }

  if (match.command === "HANDLE CONDITION") {
    lintHandleCondition(exec.tokens, 1 + match.commandTokenCount, diags);
    return;
  }

  if (match.command === "IGNORE CONDITION") {
    lintIgnoreCondition(exec.tokens, 1 + match.commandTokenCount, diags);
    return;
  }

  if (match.command === "HANDLE AID") {
    // HANDLE AID takes AID key names optionally followed by labels — no strict option check
    return;
  }

  const spec = CICS_COMMAND_SPECS[match.command];
  if (!spec) return;

  lintCicsCommandOptions(match.command, spec, exec.tokens, 1 + match.commandTokenCount, diags);
}

function lintCicsCommandOptions(
  command: string,
  spec: CicsCommandSpec,
  tokens: ExecToken[],
  optionStartIdx: number,
  diags: GenDiag[]
) {
  const seen = new Map<string, ExecToken>();

  for (let i = optionStartIdx; i < tokens.length; i++) {
    const t = tokens[i];
    if (t.inParen) continue;
    if (!CICS_KNOWN_OPTION_KEYWORDS.has(t.upper)) continue;
    if (!seen.has(t.upper)) seen.set(t.upper, t);
  }

  for (const [opt, tok] of seen.entries()) {
    if (!spec.allowedOptions.has(opt)) {
      diags.push({
        startOff: tok.start,
        endOff: tok.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_CICS_OPTION_NOT_ALLOWED",
        message: `Option ${opt} ist bei EXEC CICS ${command} nicht zulaessig.`,
      });
    }
  }

  // Resolve synonyms: if DATASET is seen, treat it as FILE being present too
  const resolvedSeen = new Set(seen.keys());
  for (const [syn, canonical] of Object.entries(CICS_OPTION_SYNONYMS)) {
    if (resolvedSeen.has(syn)) resolvedSeen.add(canonical);
  }

  for (const req of spec.requiredAll ?? []) {
    if (resolvedSeen.has(req)) continue;
    const anchor = tokens[1] ?? tokens[0];
    diags.push({
      startOff: anchor?.start ?? 0,
      endOff: anchor?.end ?? 1,
      severity: DiagnosticSeverity.Warning,
      code: "EXEC_CICS_OPTION_MISSING",
      message: `Bei EXEC CICS ${command} fehlt die erwartete Option ${req}.`,
    });
  }

  for (const reqGroup of spec.requiredOneOf ?? []) {
    const hasAny = reqGroup.some((opt) => resolvedSeen.has(opt));
    if (hasAny) continue;
    const anchor = tokens[1] ?? tokens[0];
    diags.push({
      startOff: anchor?.start ?? 0,
      endOff: anchor?.end ?? 1,
      severity: DiagnosticSeverity.Warning,
      code: "EXEC_CICS_OPTION_MISSING",
      message: `Bei EXEC CICS ${command} fehlt eine der Optionen: ${reqGroup.join(" / ")}.`,
    });
  }
}

function lintHandleCondition(tokens: ExecToken[], conditionStartIdx: number, diags: GenDiag[]) {
  if (conditionStartIdx >= tokens.length) {
    const anchor = tokens[1] ?? tokens[0];
    diags.push({
      startOff: anchor?.start ?? 0,
      endOff: anchor?.end ?? 1,
      severity: DiagnosticSeverity.Warning,
      code: "EXEC_CICS_CONDITION_MISSING",
      message: "EXEC CICS HANDLE CONDITION ohne Condition-Liste.",
    });
    return;
  }

  for (let i = conditionStartIdx; i < tokens.length; i += 2) {
    const cond = tokens[i];
    if (!cond) break;

    if (!CICS_KNOWN_CONDITIONS.has(cond.upper)) {
      diags.push({
        startOff: cond.start,
        endOff: cond.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_CICS_CONDITION_UNKNOWN",
        message: `Unbekannte CICS Condition in HANDLE CONDITION: ${cond.upper}.`,
      });
      continue;
    }

    const target = tokens[i + 1];
    if (!target || CICS_KNOWN_CONDITIONS.has(target.upper)) {
      // No label → valid: cancels previous handling (or empty parens → ignore condition)
      // Step only 1 forward so next iteration picks up the next condition
      i--;
    }
  }
}

function lintIgnoreCondition(tokens: ExecToken[], conditionStartIdx: number, diags: GenDiag[]) {
  if (conditionStartIdx >= tokens.length) {
    const anchor = tokens[1] ?? tokens[0];
    diags.push({
      startOff: anchor?.start ?? 0,
      endOff: anchor?.end ?? 1,
      severity: DiagnosticSeverity.Warning,
      code: "EXEC_CICS_CONDITION_MISSING",
      message: "EXEC CICS IGNORE CONDITION ohne Condition-Liste.",
    });
    return;
  }

  for (let i = conditionStartIdx; i < tokens.length; i++) {
    const cond = tokens[i];
    if (!cond) break;

    if (!CICS_KNOWN_CONDITIONS.has(cond.upper)) {
      diags.push({
        startOff: cond.start,
        endOff: cond.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_CICS_CONDITION_UNKNOWN",
        message: `Unbekannte CICS Condition in IGNORE CONDITION: ${cond.upper}.`,
      });
    }
  }
}

function resolveCicsCommand(tokens: ExecToken[]): CicsCommandMatch | undefined {
  for (const command of CICS_COMMAND_MATCH_ORDER) {
    const parts = command.split(" ");
    let ok = true;

    for (let i = 0; i < parts.length; i++) {
      if (tokens[1 + i]?.upper !== parts[i]) {
        ok = false;
        break;
      }
    }

    if (!ok) continue;
    const canonical = CICS_COMMAND_ALIASES.get(command) ?? command;
    return { command: canonical, commandTokenCount: parts.length };
  }

  return undefined;
}

function suggestCicsPrimaryCommand(token: string): string | undefined {
  if (token.length < 4) return undefined;
  if (!/^[A-Z]+$/.test(token)) return undefined;

  let bestKeyword: string | undefined;
  let bestDistance = Number.POSITIVE_INFINITY;

  for (const kw of CICS_PRIMARY_COMMANDS) {
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

function buildCicsCommandMatchOrder(): string[] {
  const names = new Set<string>([
    ...Object.keys(CICS_COMMAND_SPECS),
    ...CICS_ADDITIONAL_MULTIWORD_COMMANDS,
    ...Array.from(CICS_COMMAND_ALIASES.keys()),
  ]);

  return Array.from(names).sort((a, b) => {
    const aw = a.split(" ").length;
    const bw = b.split(" ").length;
    if (bw !== aw) return bw - aw;
    return b.length - a.length;
  });
}

function buildKnownCicsOptionKeywords(): Set<string> {
  const out = new Set<string>();
  for (const spec of Object.values(CICS_COMMAND_SPECS)) {
    for (const opt of spec.allowedOptions) out.add(opt);
    for (const req of spec.requiredAll ?? []) out.add(req);
    for (const reqGroup of spec.requiredOneOf ?? []) {
      for (const opt of reqGroup) out.add(opt);
    }
  }
  return out;
}

export function resolveDliRequest(tokens: { upper: string; start: number; end: number }[]): { request?: string; requestTokenCount: number } {
  // tokens layout in EXEC block: [DLI, <request...>, <clauses...>]
  const t1 = tokens[1]?.upper;
  const t2 = tokens[2]?.upper;
  const t3 = tokens[3]?.upper;
  const t4 = tokens[4]?.upper;
  const t5 = tokens[5]?.upper;

  if (!t1) return { requestTokenCount: 0 };

  if (t1 === "GET") {
    if (t2 === "HOLD") {
      if (t3 === "UNIQUE") return { request: "GHU", requestTokenCount: 3 };
      if (t3 === "NEXT" && t4 === "IN" && t5 === "PARENT") return { request: "GHNP", requestTokenCount: 5 };
      if (t3 === "NEXT") return { request: "GHN", requestTokenCount: 3 };
    }
    if (t2 === "UNIQUE") return { request: "GU", requestTokenCount: 2 };
    if (t2 === "NEXT" && t3 === "IN" && t4 === "PARENT") return { request: "GNP", requestTokenCount: 4 };
    if (t2 === "NEXT") return { request: "GN", requestTokenCount: 2 };
    return { requestTokenCount: 1 };
  }

  if (t1 === "INSERT") return { request: "ISRT", requestTokenCount: 1 };
  if (t1 === "REPLACE") return { request: "REPL", requestTokenCount: 1 };
  if (t1 === "DELETE") return { request: "DLET", requestTokenCount: 1 };
  if (t1 === "LOAD") return { request: "LOAD", requestTokenCount: 1 };
  if (t1 === "CHECKPOINT") return { request: "CHKP", requestTokenCount: 1 };
  if (t1 === "SCHEDULE") return { request: "SCHD", requestTokenCount: 1 };
  if (t1 === "TERMINATE") return { request: "TERM", requestTokenCount: 1 };
  if (t1 === "SYMBOLIC" && t2 === "CHECKPOINT") return { request: "SYMCHKP", requestTokenCount: 2 };

  return { request: t1, requestTokenCount: 1 };
}

/** IBM DLI HLPI clause-regulation matrix – shared between linter and completion. */
export const DLI_REQUEST_SPECS: Record<string, { allowed: Set<string>; required: Set<string> }> = {
  GU: {
    allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYFEEDBACK", "FEEDBACKLEN", "LOCKED", "OFFSET", "FIRST", "LAST"]),
    required: new Set(["SEGMENT"]),
  },
  GN: {
    allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYFEEDBACK", "FEEDBACKLEN", "LOCKED", "OFFSET", "FIRST", "LAST"]),
    required: new Set<string>(),
  },
  GNP: {
    allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYFEEDBACK", "FEEDBACKLEN", "LOCKED", "OFFSET", "FIRST", "LAST"]),
    required: new Set<string>(),
  },
  GHU: {
    allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYFEEDBACK", "FEEDBACKLEN", "LOCKED", "OFFSET", "FIRST", "LAST"]),
    required: new Set(["SEGMENT"]),
  },
  GHN: {
    allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYFEEDBACK", "FEEDBACKLEN", "LOCKED", "OFFSET", "FIRST", "LAST"]),
    required: new Set<string>(),
  },
  GHNP: {
    allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYFEEDBACK", "FEEDBACKLEN", "LOCKED", "OFFSET", "FIRST", "LAST"]),
    required: new Set<string>(),
  },
  ISRT: {
    allowed: new Set(["USING", "VARIABLE", "SEGMENT", "FROM", "SEGLENGTH", "WHERE", "FIELDLENGTH", "OFFSET", "FIRST", "LAST"]),
    required: new Set(["SEGMENT", "FROM"]),
  },
  REPL: {
    allowed: new Set(["USING", "VARIABLE", "SEGMENT", "FROM", "SEGLENGTH", "OFFSET"]),
    required: new Set<string>(),
  },
  DLET: {
    allowed: new Set(["USING", "VARIABLE", "SEGMENT", "FROM", "SEGLENGTH"]),
    required: new Set(["SEGMENT", "FROM"]),
  },
  LOAD: {
    allowed: new Set(["USING", "VARIABLE", "SEGMENT", "FROM", "SEGLENGTH"]),
    required: new Set(["SEGMENT", "FROM"]),
  },
  CHKP: {
    allowed: new Set(["ID"]),
    required: new Set(["ID"]),
  },
  SCHD: {
    allowed: new Set(["PSB"]),
    required: new Set(["PSB"]),
  },
  TERM: {
    allowed: new Set<string>(),
    required: new Set<string>(),
  },
};

export function validateDliClausesByRequest(
  request: string,
  requestTokenCount: number,
  tokens: { upper: string; start: number; end: number; inParen?: boolean }[],
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
    "KEYFEEDBACK",
    "FEEDBACKLEN",
    "LOCKED",
    "OFFSET",
    "FIRST",
    "LAST",
    "PSB",
    "ID",
  ]);

  // IBM DLI HLPI clause-regulation matrix (from the spec).
  // GET functions: USING PCB opt, KEYFEEDBACK opt, SEGMENT opt(GN/GNP)/required(GU),
  //   INTO opt(parent-seg)/required(object-seg), WHERE opt, FIELDLENGTH opt,
  //   LOCKED opt, OFFSET opt, SEGLENGTH opt.
  // ISRT: USING PCB opt, SEGMENT required, FROM required (object), WHERE opt (parents), FIELDLENGTH opt, OFFSET opt, SEGLENGTH opt.
  // REPL: USING PCB opt, SEGMENT opt, FROM required per segment block, OFFSET opt, SEGLENGTH opt.
  // DLET: USING PCB opt, SEGMENT required, FROM required, SEGLENGTH opt.
  // LOAD: USING PCB opt, SEGMENT required, FROM required, SEGLENGTH opt.
  // CHKP: ID required.
  // SCHD: PSB required.
  // TERM: nothing.
  const requestSpecs = DLI_REQUEST_SPECS;

  const spec = requestSpecs[request];
  if (!spec) return;

  const clauseStart = Math.min(tokens.length, 1 + Math.max(0, requestTokenCount));
  const seen = new Set<string>();
  const clauseKeywordList = Array.from(clauseKeywords);

  for (let i = clauseStart; i < tokens.length; i++) {
    const t = tokens[i];
    // Skip tokens inside parentheses — they are argument values, not clause keywords.
    if (t.inParen) continue;
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

    // Flag keywords take no arguments; all others expect a parenthesised
    // argument — e.g. SEGMENT(name), USING PCB(n).  A token immediately
    // following an argument-taking keyword is an argument, not a clause.
    const flagKeywords = new Set(["VARIABLE", "LOCKED", "FIRST", "LAST"]);
    const prev = i > clauseStart ? tokens[i - 1] : undefined;
    const prevIsArgTaking =
      prev !== undefined && clauseKeywords.has(prev.upper) && !flagKeywords.has(prev.upper);

    const suggestion = suggestDliClauseKeyword(t.upper, clauseKeywordList);
    if (!prevIsArgTaking && isLikelyDliClausePosition(tokens, i, clauseStart, clauseKeywords)) {
      if (suggestion) {
        diags.push({
          startOff: t.start,
          endOff: t.end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_CLAUSE_UNKNOWN",
          message: `Unbekannte DLI-Klausel: ${t.upper}. Meintest du ${suggestion}?`,
        });
      } else if (/^[A-Z]+$/i.test(t.upper) && t.upper.length >= 3) {
        // All-alpha token in clause position that isn't a known keyword nor an
        // obvious argument (data names normally contain hyphens or digits).
        diags.push({
          startOff: t.start,
          endOff: t.end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_CLAUSE_UNKNOWN",
          message: `Unbekannte DLI-Klausel: ${t.upper}. Erlaubte Klauseln fuer ${request}: ${[...spec.allowed].join(", ")}.`,
        });
      }
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

  // Semantic rule: FIRST and LAST are mutually exclusive
  if (seen.has("FIRST") && seen.has("LAST")) {
    for (let i = clauseStart; i < tokens.length; i++) {
      if (tokens[i].upper === "LAST") {
        diags.push({
          startOff: tokens[i].start,
          endOff: tokens[i].end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_FIRST_LAST_EXCLUSIVE",
          message: "FIRST und LAST sind bei EXEC DLI gegenseitig ausschliessend.",
        });
        break;
      }
    }
  }

  // Semantic rule: LOCKED only meaningful with INTO (GET calls)
  if (seen.has("LOCKED") && !seen.has("INTO")) {
    for (let i = clauseStart; i < tokens.length; i++) {
      if (tokens[i].upper === "LOCKED") {
        diags.push({
          startOff: tokens[i].start,
          endOff: tokens[i].end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_LOCKED_WITHOUT_INTO",
          message: "LOCKED erfordert INTO bei EXEC DLI.",
        });
        break;
      }
    }
  }

  // Semantic rule: FEEDBACKLEN requires KEYFEEDBACK
  if (seen.has("FEEDBACKLEN") && !seen.has("KEYFEEDBACK")) {
    for (let i = clauseStart; i < tokens.length; i++) {
      if (tokens[i].upper === "FEEDBACKLEN") {
        diags.push({
          startOff: tokens[i].start,
          endOff: tokens[i].end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_FEEDBACKLEN_WITHOUT_KEYFEEDBACK",
          message: "FEEDBACKLEN erfordert KEYFEEDBACK bei EXEC DLI.",
        });
        break;
      }
    }
  }

  validateDliWhereClause(request, clauseStart, tokens, clauseKeywords, diags);

  // Structural ordering checks per IBM HLPI spec (Section 5.3–5.5)
  validateDliClauseOrdering(request, clauseStart, tokens, diags);
}

/**
 * Validate EXEC DLI clause ordering according to IBM HLPI specification.
 *
 * IBM HLPI structure (Section 5.3–5.5):
 *   EXEC DLI <function>
 *     [USING PCB(n)]                              — must precede first SEGMENT, at most once
 *     [KEYFEEDBACK(ref) [FEEDBACKLEN(len)]]       — before first SEGMENT, GET only, at most once
 *     { [FIRST|LAST] [VARIABLE] SEGMENT(name)     — segment block
 *       [INTO(ref)|FROM(ref)]                     — transfer, after SEGMENT
 *       [LOCKED]                                  — after INTO (GET only)
 *       [OFFSET(exp)]                             — after transfer
 *       [SEGLENGTH(exp)]                          — after transfer
 *       [WHERE(...) [FIELDLENGTH(...)]]           — qualification
 *     }
 *   END-EXEC
 */
function validateDliClauseOrdering(
  request: string,
  clauseStart: number,
  tokens: { upper: string; start: number; end: number }[],
  diags: GenDiag[]
) {
  const isGet = ["GU", "GN", "GNP", "GHU", "GHN", "GHNP"].includes(request);

  // ---- Collect clause keyword positions ----
  const clauseKw = new Set([
    "USING", "VARIABLE", "SEGMENT", "INTO", "FROM", "SEGLENGTH",
    "WHERE", "FIELDLENGTH", "KEYFEEDBACK", "FEEDBACKLEN",
    "LOCKED", "OFFSET", "FIRST", "LAST", "PSB", "ID",
  ]);

  let firstSegmentIdx = -1;
  let usingCount = 0;
  let keyfeedbackCount = 0;

  // Find first SEGMENT and count globals
  for (let i = clauseStart; i < tokens.length; i++) {
    const u = tokens[i].upper;
    if (u === "SEGMENT" && firstSegmentIdx < 0) firstSegmentIdx = i;
    if (u === "USING") usingCount++;
    if (u === "KEYFEEDBACK") keyfeedbackCount++;
  }

  // ---- Rule: Duplicate USING ----
  if (usingCount > 1) {
    let found = 0;
    for (let i = clauseStart; i < tokens.length; i++) {
      if (tokens[i].upper === "USING") {
        found++;
        if (found > 1) {
          diags.push({
            startOff: tokens[i].start, endOff: tokens[i].end,
            severity: DiagnosticSeverity.Warning,
            code: "EXEC_DLI_DUPLICATE_USING",
            message: "USING PCB darf nur einmal pro EXEC DLI erscheinen.",
          });
        }
      }
    }
  }

  // ---- Rule: Duplicate KEYFEEDBACK ----
  if (keyfeedbackCount > 1) {
    let found = 0;
    for (let i = clauseStart; i < tokens.length; i++) {
      if (tokens[i].upper === "KEYFEEDBACK") {
        found++;
        if (found > 1) {
          diags.push({
            startOff: tokens[i].start, endOff: tokens[i].end,
            severity: DiagnosticSeverity.Warning,
            code: "EXEC_DLI_DUPLICATE_KEYFEEDBACK",
            message: "KEYFEEDBACK darf nur einmal pro EXEC DLI erscheinen.",
          });
        }
      }
    }
  }

  // ---- Rule: USING must appear before first SEGMENT ----
  if (firstSegmentIdx >= 0) {
    for (let i = firstSegmentIdx + 1; i < tokens.length; i++) {
      if (tokens[i].upper === "USING") {
        diags.push({
          startOff: tokens[i].start, endOff: tokens[i].end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_USING_AFTER_SEGMENT",
          message: "USING PCB muss vor dem ersten SEGMENT stehen.",
        });
        break;
      }
    }
  }

  // ---- Rule: KEYFEEDBACK must appear before first SEGMENT (GET only) ----
  if (isGet && firstSegmentIdx >= 0) {
    for (let i = firstSegmentIdx + 1; i < tokens.length; i++) {
      if (tokens[i].upper === "KEYFEEDBACK") {
        diags.push({
          startOff: tokens[i].start, endOff: tokens[i].end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_KEYFEEDBACK_AFTER_SEGMENT",
          message: "KEYFEEDBACK muss vor dem ersten SEGMENT stehen.",
        });
        break;
      }
    }
  }

  // ---- Walk segment blocks for structural ordering ----
  // Track: lastSegmentIdx, per-segment state
  let lastSegmentIdx = -1;
  let segHasTransfer = false;     // INTO or FROM seen in current block
  let segTransferIsInto = false;  // was it INTO? (for LOCKED check)
  // Deferred checks: these keywords require INTO/FROM in the same segment block,
  // but INTO/FROM can appear before OR after them.  Collect positions and check
  // when the segment block ends (at next SEGMENT or end of tokens).
  let pendingSeglength: { start: number; end: number } | null = null;
  let pendingOffset: { start: number; end: number } | null = null;
  let pendingLocked: { start: number; end: number } | null = null;

  function flushSegmentBlock() {
    if (pendingSeglength && !segHasTransfer) {
      diags.push({
        startOff: pendingSeglength.start, endOff: pendingSeglength.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_SEGLENGTH_WITHOUT_TRANSFER",
        message: "SEGLENGTH erfordert INTO oder FROM im selben Segment-Block.",
      });
    }
    if (pendingOffset && !segHasTransfer) {
      diags.push({
        startOff: pendingOffset.start, endOff: pendingOffset.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_OFFSET_WITHOUT_TRANSFER",
        message: "OFFSET erfordert INTO oder FROM im selben Segment-Block.",
      });
    }
    if (pendingLocked && !segTransferIsInto) {
      diags.push({
        startOff: pendingLocked.start, endOff: pendingLocked.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_LOCKED_POSITION",
        message: "LOCKED muss nach INTO innerhalb eines Segment-Blocks stehen.",
      });
    }
    pendingSeglength = null;
    pendingOffset = null;
    pendingLocked = null;
  }

  for (let i = clauseStart; i < tokens.length; i++) {
    const t = tokens[i];
    if (!clauseKw.has(t.upper)) continue;

    if (t.upper === "SEGMENT") {
      // Flush the previous segment block before starting a new one
      flushSegmentBlock();
      lastSegmentIdx = i;
      segHasTransfer = false;
      segTransferIsInto = false;
      continue;
    }

    if (t.upper === "INTO" || t.upper === "FROM") {
      segHasTransfer = true;
      segTransferIsInto = t.upper === "INTO";
    }

    // ---- Rule: INTO/FROM must appear after a SEGMENT ----
    // Exception: REPL allows FROM without explicit SEGMENT
    if ((t.upper === "INTO" || t.upper === "FROM") && lastSegmentIdx < 0 && request !== "REPL") {
      diags.push({
        startOff: t.start, endOff: t.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_TRANSFER_BEFORE_SEGMENT",
        message: `${t.upper} muss nach SEGMENT stehen.`,
      });
    }

    // ---- Rule: WHERE must appear within a segment block (after SEGMENT) ----
    if (t.upper === "WHERE" && lastSegmentIdx < 0) {
      diags.push({
        startOff: t.start, endOff: t.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_WHERE_BEFORE_SEGMENT",
        message: "WHERE muss innerhalb eines Segment-Blocks stehen (nach SEGMENT).",
      });
    }

    // ---- Deferred: LOCKED requires INTO in the same segment block ----
    if (t.upper === "LOCKED" && lastSegmentIdx >= 0) {
      pendingLocked = { start: t.start, end: t.end };
    }

    // ---- Deferred: OFFSET requires INTO/FROM in the same segment block ----
    if (t.upper === "OFFSET" && lastSegmentIdx >= 0) {
      pendingOffset = { start: t.start, end: t.end };
    }

    // ---- Deferred: SEGLENGTH requires INTO/FROM in the same segment block ----
    if (t.upper === "SEGLENGTH" && lastSegmentIdx >= 0) {
      pendingSeglength = { start: t.start, end: t.end };
    }

    // ---- Rule: FEEDBACKLEN must follow KEYFEEDBACK (global position) ----
    if (t.upper === "FEEDBACKLEN" && firstSegmentIdx >= 0 && i > firstSegmentIdx) {
      diags.push({
        startOff: t.start, endOff: t.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_FEEDBACKLEN_AFTER_SEGMENT",
        message: "FEEDBACKLEN muss vor dem ersten SEGMENT stehen (zusammen mit KEYFEEDBACK).",
      });
    }
  }

  // Flush the last segment block
  flushSegmentBlock();
}

function isLikelyDliClausePosition(
  tokens: { upper: string; start: number; end: number; inParen?: boolean }[],
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
  tokens: { upper: string; start: number; end: number; inParen?: boolean }[],
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
  tokens: { upper: string; start: number; end: number; inParen?: boolean }[],
  startIdx: number,
  clauseKeywords: Set<string>
): number {
  for (let i = startIdx; i < tokens.length; i++) {
    if (tokens[i].inParen) continue;
    if (clauseKeywords.has(tokens[i].upper)) return i;
  }
  return tokens.length;
}

function hasNonClauseToken(
  tokens: { upper: string; start: number; end: number; inParen?: boolean }[],
  startIdx: number,
  endIdx: number,
  clauseKeywords: Set<string>
): boolean {
  for (let i = startIdx; i < endIdx; i++) {
    // Tokens inside parentheses count as non-clause (they are argument values)
    if (tokens[i].inParen) return true;
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

// ======================= Undefined-Identifier Check =======================

/**
 * COBOL reserved words, verbs, clauses, and figurative constants (COBOL-85 + IBM extensions).
 *
 * Any COBOL word that is NOT in this set AND NOT in the definition index
 * is flagged as an undefined identifier.
 */
const COBOL_RESERVED_WORDS = new Set([
  // --- Divisions / Sections ---
  "IDENTIFICATION", "ID", "ENVIRONMENT", "DATA", "PROCEDURE",
  "DIVISION", "SECTION",
  "CONFIGURATION", "INPUT-OUTPUT", "FILE", "WORKING-STORAGE", "LINKAGE",
  "LOCAL-STORAGE", "COMMUNICATION", "REPORT", "SCREEN",
  "FILE-CONTROL", "I-O-CONTROL", "SPECIAL-NAMES", "SOURCE-COMPUTER", "OBJECT-COMPUTER",
  "REPOSITORY",

  // --- Verbs / Statement leaders ---
  "ACCEPT", "ADD", "ALTER", "CALL", "CANCEL", "CLOSE", "COMPUTE", "CONTINUE",
  "COPY", "DELETE", "DISPLAY", "DIVIDE", "ELSE", "ENTRY", "EVALUATE",
  "EXEC", "EXECUTE", "EXIT", "GENERATE", "GOBACK", "GO", "IF",
  "INITIALIZE", "INITIATE", "INSERT", "INSPECT", "MERGE", "MOVE", "MULTIPLY",
  "NEXT", "NOT", "OPEN", "PERFORM", "READ", "RECEIVE", "RELEASE",
  "REPLACE", "RETURN", "REWRITE", "SEARCH", "SEND", "SET", "SORT",
  "START", "STOP", "STRING", "SUBTRACT", "SUPPRESS", "TERMINATE",
  "UNSTRING", "USE", "WRITE",

  // --- END-xxx ---
  "END-ACCEPT", "END-ADD", "END-CALL", "END-COMPUTE", "END-DELETE",
  "END-DISPLAY", "END-DIVIDE", "END-EVALUATE", "END-IF", "END-MULTIPLY",
  "END-PERFORM", "END-READ", "END-RECEIVE", "END-RETURN", "END-REWRITE",
  "END-SEARCH", "END-START", "END-STRING", "END-SUBTRACT", "END-UNSTRING",
  "END-WRITE", "END-EXEC",

  // --- Clause / modifier keywords ---
  "ADVANCING", "AFTER", "ALL", "ALSO", "ALPHABETIC", "ALPHABETIC-LOWER",
  "ALPHABETIC-UPPER", "ALPHANUMERIC", "ALPHANUMERIC-EDITED", "AND", "ANY",
  "APPLY", "ARE", "ASCENDING", "ASSIGN", "AT",
  "BEFORE", "BEGINNING", "BINARY", "BLANK", "BLOCK", "BOTTOM", "BY",
  "CBL", "CD", "CF", "CH", "CHARACTER", "CHARACTERS", "CLASS", "CLOCK-UNITS",
  "CODE", "CODE-SET", "COLLATING", "COLUMN", "COMMA", "COMMON",
  "COMP", "COMP-1", "COMP-2", "COMP-3", "COMP-4", "COMP-5",
  "COMPUTATIONAL", "COMPUTATIONAL-1", "COMPUTATIONAL-2",
  "COMPUTATIONAL-3", "COMPUTATIONAL-4", "COMPUTATIONAL-5",
  "CONTAINS", "CONTENT", "CONTROL", "CONTROLS", "CONVERTING",
  "COPY", "CORR", "CORRESPONDING", "COUNT", "CURRENCY",
  "DATE", "DATE-COMPILED", "DATE-WRITTEN", "DAY", "DAY-OF-WEEK",
  "DE", "DEBUG-CONTENTS", "DEBUG-ITEM", "DEBUG-LINE", "DEBUG-NAME",
  "DEBUG-SUB-1", "DEBUG-SUB-2", "DEBUG-SUB-3", "DEBUGGING",
  "DECIMAL-POINT", "DECLARATIVES", "DELIMITED", "DELIMITER",
  "DEPENDING", "DESCENDING", "DETAIL", "DOWN", "DUPLICATES", "DYNAMIC",
  "EGI", "EMI", "ENABLE", "ENDING", "ENVIRONMENT",
  "EOP", "EQUAL", "ERROR", "ESI", "EVERY", "EXCEPTION", "EXTEND", "EXTERNAL",
  "FALSE", "FD", "FILLER", "FINAL", "FIRST", "FOOTING", "FOR", "FROM",
  "FUNCTION",
  "GIVING", "GLOBAL", "GREATER", "GROUP",
  "HEADING", "HIGH-VALUE", "HIGH-VALUES",
  "I-O", "IN", "INDEX", "INDEXED", "INDICATE", "INITIAL", "INNER",
  "INPUT", "INSTALLATION", "INTO", "INVALID", "IS",
  "JUST", "JUSTIFIED",
  "KEY",
  "LABEL", "LAST", "LEADING", "LEFT", "LENGTH", "LESS", "LIMIT", "LIMITS",
  "LINAGE", "LINAGE-COUNTER", "LINE", "LINE-COUNTER",
  "LINES", "LOCK", "LOW-VALUE", "LOW-VALUES",
  "MEMORY", "MESSAGE", "MODE", "MODULES",
  "NATIVE", "NEGATIVE", "NO", "NUMERIC", "NUMERIC-EDITED",
  "OCCURS", "OF", "OFF", "OMITTED", "ON", "OPTIONAL", "OR",
  "ORDER", "ORGANIZATION", "OTHER", "OUTPUT", "OVERFLOW",
  "PACKED-DECIMAL", "PADDING", "PAGE", "PAGE-COUNTER",
  "PF", "PH", "PIC", "PICTURE", "PLUS", "POINTER",
  "POSITION", "POSITIVE", "PRINTING", "PROGRAM", "PROGRAM-ID", "PURGE",
  "QUEUE", "QUOTE", "QUOTES",
  "RANDOM", "RD", "RECORD", "RECORDS", "RECORDING",
  "REDEFINES", "REEL", "REFERENCE", "REFERENCES", "RELATIVE", "REMAINDER",
  "REMOVAL", "RENAMES", "REPLACING", "REPORTING",
  "REPORTS", "RERUN", "RESERVE", "RESET", "REVERSED", "REWOUND",
  "RF", "RH", "RIGHT", "ROUNDED", "RUN",
  "SAME", "SD", "SECURITY", "SELECT", "SELF",
  "SENTENCE", "SEPARATE", "SEQUENCE", "SEQUENTIAL",
  "SIGN", "SIZE", "SORT-MERGE", "SORT-RETURN",
  "SOURCE", "SPACE", "SPACES", "SPECIAL-NAMES",
  "STANDARD", "STANDARD-1", "STANDARD-2",
  "STATUS", "SUM", "SYMBOLIC", "SYNC", "SYNCHRONIZED",
  "TABLE", "TALLYING", "TAPE", "TEST", "TEXT", "THAN", "THEN",
  "THROUGH", "THRU", "TIME", "TIMES", "TO", "TOP",
  "TRAILING", "TRUE", "TYPE",
  "UNIT", "UNTIL", "UP", "UPON", "USAGE", "USING",
  "VALUE", "VALUES", "VARYING",
  "WHEN", "WITH", "WORDS",
  "WORKING-STORAGE", "WRITE-ONLY",
  "ZERO", "ZEROES", "ZEROS",
  "READY", "TRACE",

  // --- IBM z/OS & VSE extensions ---
  "SERVICE", "RELOAD", "BASIS", "TRANSFORM", "EXAMINE", "ENTER", "NOTE",
  "EXHIBIT", "READY", "RESET", "TRACE", "EJECT", "SKIP1", "SKIP2", "SKIP3",
  "TITLE", "CBL", "PROCESS",
  "DISPLAY-1", "DBCS", "EGCS", "KANJI",
  "XML", "JSON", "PARSE", "ENCODING",
  "UTF-8",
  "NATIVE",

  // --- Condition / relation ---
  "EQUAL", "EQUALS", "GREATER", "LESS", "THAN",
  "POSITIVE", "NEGATIVE", "NUMERIC",
  "NOT",

  // --- Special registers ---
  "ADDRESS", "DEBUG-ITEM",
  "RETURN-CODE", "SORT-RETURN",
  "TALLY", "SHIFT-OUT", "SHIFT-IN",
  "WHEN-COMPILED",
  "LINAGE-COUNTER", "LINE-COUNTER", "PAGE-COUNTER",
  "JNIENVPTR", "JSON-CODE", "JSON-STATUS", "XML-CODE", "XML-EVENT",
  "XML-NTEXT", "XML-TEXT",

  // --- CICS / DLI (inside EXEC blocks these are skipped, but they appear
  //     as clause keywords in surrounding code too) ---
  "DLI", "CICS", "SQL", "SQLIMS",
  "END-EXEC",

  // --- Figurative constants ---
  "ZERO", "ZEROS", "ZEROES",
  "SPACE", "SPACES",
  "HIGH-VALUE", "HIGH-VALUES",
  "LOW-VALUE", "LOW-VALUES",
  "QUOTE", "QUOTES",
  "NULL", "NULLS",

  // --- Boolean / special literals ---
  "TRUE", "FALSE",

  // --- PERFORM modifiers ---
  "VARYING", "UNTIL", "TIMES", "THROUGH", "THRU", "WITH", "TEST",
  "BEFORE", "AFTER",

  // --- Misc ---
  "CORRESPONDING", "CORR", "REMAINDER", "ROUNDED", "GIVING",
  "ON", "SIZE", "ERROR", "OVERFLOW", "EXCEPTION",
  "INVALID", "KEY", "AT", "END", "EOP", "PAGE",
  "UPON", "CONSOLE", "SYSOUT", "SYSIN",
  "TAB", "NO", "ADVANCING", "DELIMITED",
  "POINTER", "COUNT", "TALLYING",
  "INTO", "FROM", "BY", "TO", "USING", "RETURNING",
  "REFERENCE", "CONTENT",
  "LENGTH", "OF", "IN", "FUNCTION",
  "STANDARD", "CLASS",
  "GLOBAL", "EXTERNAL",
  "VARIABLE",

  // --- I/O ---
  "INPUT", "OUTPUT", "I-O", "EXTEND",
  "SEQUENTIAL", "RANDOM", "DYNAMIC",
  "OPTIONAL", "LOCK",
  "REVERSED",
  "LABEL", "STANDARD", "OMITTED",
  "ACCESS",

  // --- SORT / MERGE ---
  "ASCENDING", "DESCENDING",
  "DUPLICATES", "IN", "ORDER",

  // --- Miscellaneous IBM extensions ---
  "ENTRYLEN", "KEYLEN",
  "ALSO",
  "RECORDING",
  "REMARKS",
  "AUTHOR", "INSTALLATION", "DATE-WRITTEN", "DATE-COMPILED", "SECURITY",

  // --- DL/I Interface Block (DIB) fields ---
  // Automatically provided by the DL/I HLPI translator; not user-defined.
  "DIBSTAT",    // DL/I status code
  "DIBSEGM",   // segment name
  "DIBKFBL",   // key feedback length
  "DIBSEGL",   // segment length
  "DIBSEGNO",  // segment level number
  "DIBDBDNM",  // DBD name (database name)
  "DIBPROCOP", // processing options
  "DIBKFBA",   // key feedback area

  // --- CICS Execute Interface Block (EIB) fields ---
  // Automatically provided in CICS programs via DFHEIBLK; not user-defined.
  "EIBAID",     // attention identifier (AID key)
  "EIBATT",     // attach header received
  "EIBCALEN",   // COMMAREA length
  "EIBCOMPL",   // data set completion status
  "EIBCONF",    // confirmation request
  "EIBCPOSN",   // cursor position
  "EIBDATE",    // date (0CYYDDD packed)
  "EIBDS",      // last data set name
  "EIBEOC",     // end of chain
  "EIBERR",     // error flag
  "EIBERRCD",   // error code
  "EIBFMH",     // FMH received
  "EIBFN",      // function code
  "EIBFREE",    // free facility
  "EIBNODAT",   // no data flag
  "EIBRECV",    // receive required
  "EIBREQID",   // request identifier
  "EIBRESP",    // response code
  "EIBRESP2",   // secondary response code
  "EIBRLDBK",   // rollback indicator
  "EIBRSRCE",   // resource name
  "EIBSIG",     // signal received
  "EIBSYNC",    // syncpoint request
  "EIBSYNRB",   // syncpoint rollback
  "EIBTASKN",   // task number
  "EIBTIME",    // time (0HHMMSS packed)
  "EIBTRMID",   // terminal identifier
  "EIBTRNID",   // transaction identifier
]);

/**
 * Lint for undefined identifiers in PROCEDURE DIVISION.
 *
 * Walks the preprocessed text line-by-line through the PROCEDURE DIVISION.
 * For every COBOL word that is NOT a reserved word, NOT defined in the index,
 * and NOT a numeric/special token, emits a warning diagnostic.
 *
 * @param text  Preprocessed COBOL text (fixed-format, COPY-expanded)
 * @param index Definition index built from the same preprocessed text
 * @returns     Array of GenDiag for each undefined identifier occurrence
 */
export function lintUndefinedIdentifiers(text: string, index: DefinitionIndex): GenDiag[] {
  const diags: GenDiag[] = [];
  const lines = text.split(/\r?\n/);

  // Build lookup set of all defined names (data items, paragraphs, sections)
  const definedNames = new Set<string>();
  for (const d of index.dataItems) definedNames.add(d.name);
  for (const p of index.paragraphs) definedNames.add(p.name);
  for (const s of index.sections) definedNames.add(s.name);

  // Track already-flagged names to limit noise: warn only on first occurrence
  const flaggedNames = new Set<string>();

  let inProcedureDivision = false;
  let inExecBlock = false;
  let off = 0;

  const tokenRe = /[A-Za-z0-9][A-Za-z0-9_-]*/g;

  for (const line of lines) {
    const isFixed = hasFixedColumns(line);
    const indicator = isFixed ? line[6] : " ";
    const isComment = isFixed && isFixedCommentIndicator(indicator);
    const isContinuation = isFixed && indicator === "-";

    if (isComment || isContinuation) {
      off += line.length + 1;
      continue;
    }

    if (isCompilerDirectiveLine(line)) {
      off += line.length + 1;
      continue;
    }

    const langStart = isFixed ? 7 : 0;
    const langEnd = isFixed ? Math.min(line.length, 72) : line.length;
    const lang = line.slice(langStart, langEnd);
    const trimmed = lang.trim();

    // Division tracking
    if (/^(IDENTIFICATION|ID|ENVIRONMENT|DATA)\s+DIVISION\b/i.test(trimmed)) {
      inProcedureDivision = false;
      off += line.length + 1;
      continue;
    }
    if (/^PROCEDURE\s+DIVISION\b/i.test(trimmed)) {
      inProcedureDivision = true;
      off += line.length + 1;
      continue;
    }

    if (!inProcedureDivision) {
      off += line.length + 1;
      continue;
    }

    // EXEC block tracking — check data names in parenthesized arguments
    if (trimmed.length > 0) {
      const hasExec = /\bEXEC\b/i.test(trimmed);
      const hasEndExec = /\bEND-EXEC\b/i.test(trimmed);
      if (inExecBlock || hasExec || hasEndExec) {
        if (hasExec && !hasEndExec) inExecBlock = true;
        if (hasEndExec) inExecBlock = false;

        // Inside EXEC DLI/CICS: validate data-name arguments inside parentheses.
        // Keywords whose parenthesized arguments are COBOL host variables (refs):
        //   INTO(ref), FROM(ref), KEYFEEDBACK(ref), SET(ref) — check as data names
        //   SEGLENGTH(exp), FIELDLENGTH(exp), FEEDBACKLEN(exp), OFFSET(exp), ID(exp)
        //     — expressions: can be data names or numeric literals (skip pure numbers)
        //   WHERE(field = ref) — only the right side (ref) is a host variable
        // NOT checked: SEGMENT(name) = DLI segment name, USING PCB(n) = PCB number,
        //   PSB(name) = PSB name
        const dataArgRe = /\b(INTO|FROM|WHERE|KEYFEEDBACK|SET|SEGLENGTH|FIELDLENGTH|FEEDBACKLEN|OFFSET|ID)\s*\(([^)]*)\)/gi;
        let dm: RegExpExecArray | null;
        while ((dm = dataArgRe.exec(lang)) !== null) {
          const keyword = dm[1].toUpperCase();
          const argsText = dm[2]; // text inside the parens
          const argsOffset = langStart + dm.index + dm[0].indexOf("(") + 1;

          if (keyword === "WHERE") {
            // WHERE( RelCond { AND|OR RelCond } )
            // RelCond = fieldname RelOp ref
            // Only the right-hand side (ref) is a COBOL data name.
            // The left-hand side is a DLI segment field name — do NOT check it.
            // Split by AND/OR to get individual RelCond fragments.
            const relCondRe = /([A-Za-z][A-Za-z0-9_-]*)\s*(?:NOT\s*[><=]|[><=]+|=<|=>)\s*([A-Za-z][A-Za-z0-9_-]*)/g;
            let rm: RegExpExecArray | null;
            while ((rm = relCondRe.exec(argsText)) !== null) {
              // rm[1] = field name (DLI segment field) — skip
              // rm[2] = ref (COBOL host variable) — check
              const ref = rm[2];
              const refUpper = ref.toUpperCase();
              if (COBOL_RESERVED_WORDS.has(refUpper)) continue;
              if (definedNames.has(refUpper)) continue;
              if (flaggedNames.has(refUpper)) continue;
              if (isInsideStringLiteral(argsText, rm.index + rm[0].length - ref.length)) continue;

              flaggedNames.add(refUpper);
              const wordOff = off + argsOffset + rm.index + rm[0].length - ref.length;
              diags.push({
                startOff: wordOff,
                endOff: wordOff + ref.length,
                severity: DiagnosticSeverity.Warning,
                code: "UNDEFINED_IDENTIFIER",
                message: `'${refUpper}' ist nicht definiert (kein Datenname, Paragraph oder Section).`,
              });
            }
          } else {
            // INTO, FROM, KEYFEEDBACK, SET — single data name
            // SEGLENGTH, FIELDLENGTH, FEEDBACKLEN, OFFSET, ID — expression (data name or numeric literal)
            const wordRe2 = /[A-Za-z][A-Za-z0-9_-]*/g;
            let wm: RegExpExecArray | null;
            while ((wm = wordRe2.exec(argsText)) !== null) {
              const w = wm[0];
              const wUpper = w.toUpperCase();
              // Skip PCB keyword (for USING PCB(...))
              if (wUpper === "PCB") continue;
              if (COBOL_RESERVED_WORDS.has(wUpper)) continue;
              if (definedNames.has(wUpper)) continue;
              if (flaggedNames.has(wUpper)) continue;
              if (isInsideStringLiteral(argsText, wm.index)) continue;

              flaggedNames.add(wUpper);
              const wordOff = off + argsOffset + wm.index;
              diags.push({
                startOff: wordOff,
                endOff: wordOff + w.length,
                severity: DiagnosticSeverity.Warning,
                code: "UNDEFINED_IDENTIFIER",
                message: `'${wUpper}' ist nicht definiert (kein Datenname, Paragraph oder Section).`,
              });
            }
          }
        }

        off += line.length + 1;
        continue;
      }
    }

    // Skip paragraph/section label lines
    if (/^[A-Z][A-Z0-9-]*\s*(SECTION)?\s*\.$/i.test(trimmed)) {
      off += line.length + 1;
      continue;
    }

    // Skip inline label prefix: "LABEL. <code>"
    // We don't check the label itself — only the code after it may contain identifiers.

    // Scan every COBOL word in the lang area
    tokenRe.lastIndex = 0;
    let m: RegExpExecArray | null;
    while ((m = tokenRe.exec(lang)) !== null) {
      const word = m[0];
      const upper = word.toUpperCase();

      // Skip pure numbers (level numbers, numeric literals, PIC contents)
      if (/^\d/.test(word)) continue;

      // Skip known words
      if (COBOL_RESERVED_WORDS.has(upper)) continue;
      if (definedNames.has(upper)) continue;

      // Skip FILLER (not in definedNames because buildDefinitionIndex skips it)
      if (upper === "FILLER") continue;

      // Skip words ending with common suffixes that look like section names
      // already handled by definedNames

      // Skip if already flagged (warn once per name)
      if (flaggedNames.has(upper)) continue;

      // Skip words inside string literals — the regex won't match inside quotes
      // because scanLintTokens handles that, but our simpler line scanner does not.
      // Check if the match position is inside a string literal on this line.
      const charPos = langStart + m.index;
      if (isInsideStringLiteral(lang, m.index)) continue;

      flaggedNames.add(upper);
      const wordOff = off + charPos;
      diags.push({
        startOff: wordOff,
        endOff: wordOff + word.length,
        severity: DiagnosticSeverity.Warning,
        code: "UNDEFINED_IDENTIFIER",
        message: `'${upper}' ist nicht definiert (kein Datenname, Paragraph oder Section).`,
      });
    }

    off += line.length + 1;
  }

  return diags;
}

/**
 * Check if a character position in a lang-area string is inside a string literal.
 * This handles both single-quote and double-quote COBOL string literals.
 */
function isInsideStringLiteral(lang: string, pos: number): boolean {
  let inString = false;
  let quoteChar = "";
  for (let i = 0; i < lang.length; i++) {
    const ch = lang[i];
    if (inString) {
      if (ch === quoteChar) {
        // Check for escaped quote (doubled quote)
        if (i + 1 < lang.length && lang[i + 1] === quoteChar) {
          i++; // skip escaped quote
          continue;
        }
        inString = false;
      }
      if (i >= pos) return true; // pos is inside string
    } else {
      if (ch === "'" || ch === '"') {
        inString = true;
        quoteChar = ch;
      }
      if (i === pos) return false; // pos is outside string (at quote start or outside)
    }
  }
  return inString && pos < lang.length;
}
