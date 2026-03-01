/**
 * Semantic Tokens provider for COBOL fixed-format.
 *
 * Provides fine-grained token classification for syntax highlighting:
 *  - Keywords (COBOL verbs, division/section headers)
 *  - Variables (data names)
 *  - Functions (paragraph names)
 *  - Namespaces (section names)
 *  - Strings
 *  - Numbers
 *  - Comments
 *  - Operators
 *
 * Uses the definition index to distinguish paragraphs/sections/data names
 * from generic keywords.
 */
import { Range, SemanticTokensBuilder } from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import {
  hasFixedColumns,
  isFixedCommentIndicator,
  isValidFixedIndicator,
} from "./utils";

import {
  buildDefinitionIndex,
  DefinitionIndex,
} from "./definition";

// ---- Token types & modifiers (indices into legend) ----

export const TOKEN_TYPES = [
  "keyword",      // 0 — COBOL verbs, division/section keywords
  "variable",     // 1 — data names
  "function",     // 2 — paragraph names / CICS command verbs
  "namespace",    // 3 — section names
  "string",       // 4 — string literals
  "number",       // 5 — numeric literals
  "comment",      // 6 — comment lines
  "operator",     // 7 — arithmetic operators
  "macro",        // 8 — COPY statements / compiler directives
  "type",         // 9 — level numbers, PIC clauses, CICS conditions
  "parameter",    // 10 — FILLER, special registers
  "property",     // 11 — CICS option keywords
] as const;

export const TOKEN_MODIFIERS = [
  "declaration",  // 0 — definition site
  "readonly",     // 1 — level 88 condition names
  "definition",   // 2 — alias for declaration
] as const;

// ---- Known COBOL keywords ----

const COBOL_KEYWORDS = new Set([
  "ACCEPT", "ADD", "ADDRESS", "ALTER", "CALL", "CANCEL", "CLOSE", "COMPUTE", "CONTINUE",
  "DELETE", "DISPLAY", "DIVIDE", "ELSE", "END-EVALUATE", "END-IF",
  "END-PERFORM", "END-READ", "END-RETURN", "END-REWRITE", "END-SEARCH",
  "END-START", "END-STRING", "END-SUBTRACT", "END-UNSTRING", "END-WRITE",
  "EVALUATE", "EXIT", "GO", "GOBACK",
  "IF", "INITIALIZE", "INITIATE", "INSPECT", "MERGE", "MOVE", "MULTIPLY",
  "NEXT", "NOT", "OPEN", "PERFORM", "READ", "RELEASE", "RETURN", "REWRITE",
  "SEARCH", "SET", "SORT", "START", "STOP", "STRING", "SUBTRACT",
  "UNSTRING", "WRITE", "WHEN",
  "THEN", "THAN", "TO", "FROM", "INTO", "BY", "GIVING", "REMAINDER",
  "ON", "SIZE", "ERROR", "OVERFLOW", "AT", "END", "INVALID", "KEY",
  "WITH", "POINTER", "TALLYING", "REPLACING", "LEADING", "TRAILING",
  "ALL", "FIRST", "INITIAL", "REFERENCE", "CONTENT", "VALUE",
  "UNTIL", "VARYING", "AFTER", "BEFORE", "THRU", "THROUGH", "TIMES",
  "ASCENDING", "DESCENDING", "DEPENDING", "INDEXED",
  "ALSO", "OTHER", "ANY", "TRUE", "FALSE",
  "CORRESPONDING", "CORR", "ROUNDED",
  "UPON", "ADVANCING", "LINE", "LINES", "PAGE",
  "USING", "RETURNING",
  "RUN", "PROGRAM",
]);

const DIVISION_SECTION_KEYWORDS = new Set([
  "IDENTIFICATION", "ID", "ENVIRONMENT", "DATA", "PROCEDURE",
  "DIVISION", "SECTION",
  "CONFIGURATION", "INPUT-OUTPUT", "FILE",
  "WORKING-STORAGE", "LINKAGE", "LOCAL-STORAGE",
  "COMMUNICATION", "REPORT", "SCREEN",
  "FILE-CONTROL", "I-O-CONTROL", "OBJECT-COMPUTER", "SOURCE-COMPUTER",
  "SPECIAL-NAMES",
]);

const DATA_KEYWORDS = new Set([
  "PIC", "PICTURE", "USAGE", "OCCURS", "REDEFINES",
  "COMP", "COMP-1", "COMP-2", "COMP-3", "COMP-4", "COMP-5",
  "BINARY", "PACKED-DECIMAL", "DISPLAY",
  "JUSTIFIED", "BLANK", "SIGN", "SYNCHRONIZED", "SYNC",
  "IS", "ARE", "OF", "IN",
  "COPY", "EXEC", "END-EXEC",
  "FD", "SD", "RD",
  "SELECT", "ASSIGN", "ORGANIZATION", "ACCESS", "MODE",
  "SEQUENTIAL", "RANDOM", "DYNAMIC", "RELATIVE",
  "STATUS", "RECORD", "CONTAINS", "CHARACTERS",
  "BLOCK", "LABEL", "STANDARD", "OMITTED",
  "PROGRAM-ID", "AUTHOR", "INSTALLATION", "DATE-WRITTEN",
  "DATE-COMPILED", "SECURITY",
]);

const SPECIAL_REGISTERS = new Set([
  "FILLER", "ZERO", "ZEROS", "ZEROES", "SPACE", "SPACES",
  "HIGH-VALUE", "HIGH-VALUES", "LOW-VALUE", "LOW-VALUES",
  "QUOTE", "QUOTES",
]);

// ---- DLI keyword sets (for EXEC DLI semantic classification) ----

/** DLI function verbs (short codes) — classified as function (2). */
const DLI_VERBS = new Set([
  "GU", "GN", "GNP", "GHU", "GHN", "GHNP",
  "ISRT", "REPL", "DLET", "LOAD",
  "CHKP", "SCHD", "TERM",
]);

/**
 * DLI long-form verb words — each word individually classified as function (2).
 * Long forms: GET UNIQUE, GET NEXT, GET NEXT IN PARENT,
 *             GET HOLD UNIQUE, GET HOLD NEXT, GET HOLD NEXT IN PARENT,
 *             INSERT, REPLACE, DELETE, CHECKPOINT, SCHEDULE, TERMINATE.
 */
const DLI_VERB_WORDS = new Set([
  "GET", "UNIQUE", "NEXT", "IN", "PARENT", "HOLD",
  "INSERT", "REPLACE", "DELETE",
  "CHECKPOINT", "SCHEDULE", "TERMINATE",
]);

/** DLI clause/option keywords — classified as property (11). */
const DLI_CLAUSE_KEYWORDS = new Set([
  "USING", "PCB",
  "SEGMENT", "INTO", "FROM",
  "WHERE", "FIELDLENGTH", "SEGLENGTH",
  "KEYFEEDBACK", "FEEDBACKLEN",
  "OFFSET", "LOCKED",
  "FIRST", "LAST", "VARIABLE",
  "PSB", "ID",
  "AND", "OR",
]);

// ---- CICS keyword sets (for EXEC CICS semantic classification) ----

const CICS_VERBS = new Set([
  "READ", "READNEXT", "READPREV", "WRITE", "REWRITE", "DELETE",
  "START", "STARTBR", "RESETBR", "ENDBR", "UNLOCK",
  "SEND", "RECEIVE", "LINK", "XCTL", "RETURN", "LOAD", "RELEASE",
  "CANCEL", "DELAY", "ASKTIME", "FORMATTIME", "RETRIEVE",
  "GETMAIN", "FREEMAIN", "ABEND", "SUSPEND",
  "ENQ", "DEQ", "POST", "READQ", "WRITEQ", "DELETEQ",
  "SYNCPOINT", "HANDLE", "IGNORE", "PUSH", "POP",
  "DUMP", "ISSUE", "PURGE", "ADDRESS", "ASSIGN",
  "ENTER", "ROLLBACK",
]);

const CICS_OPTIONS = new Set([
  "FILE", "DATASET", "INTO", "SET", "RIDFLD", "RBA", "RRN", "XRBA",
  "KEYLENGTH", "GENERIC", "GTEQ", "EQUAL", "TOKEN", "LENGTH",
  "UPDATE", "NOSUSPEND", "CONSISTENT", "SYSID", "REQID",
  "FROM", "DEBKEY", "DEBREC", "MASSINSERT",
  "PROGRAM", "COMMAREA", "DATALENGTH", "CHANNEL",
  "INPUTMSG", "INPUTMSGLEN", "SYNCONRETURN",
  "TRANSID", "FLENGTH", "ENTRY", "HOLD",
  "INTERVAL", "TIME", "AFTER", "AT", "RTRANSID", "RTERMID",
  "QUEUE", "NOCHECK", "FOR", "HOURS", "MINUTES", "SECONDS",
  "MILLISECS", "UNTIL", "ABSTIME",
  "MAP", "MAPSET", "DATAONLY", "MAPONLY", "ERASE", "ALARM",
  "FREEKB", "FRSET", "CURSOR", "WAIT", "LAST", "PAGING",
  "ASIS", "NOTRUNCATE", "DATA", "DATAPOINTER",
  "ABCODE", "NODUMP", "RESOURCE", "UOW", "TASK",
  "MAXLIFETIME", "ECADDR", "BELOW", "INITIMG", "SHARED",
  "ITEM", "NEXT", "NUMITEMS", "MAIN", "AUXILIARY",
  "LABEL", "RESET", "TEXT", "TD", "TS",
  "CONDITION", "AID", "CONTROL", "PAGE", "MESSAGE",
  "TRANSACTION", "OPERATOR", "TRACENUM", "DUMPCODE",
  "COMPLETE", "STORAGE", "TERMINAL", "FROMLENGTH",
  "EXCEPTION", "TEXTLENGTH", "IMMEDIATE", "TERMID",
  "TRAILER", "RETAIN", "AUTOPAGE", "FMHPARM", "OPERPURGE",
  // ASSIGN options
  "ABDUMP", "ABOFFSET", "ABPROGRAM", "ACTIVITY", "ACTIVITYID",
  "ALTSCRNHT", "ALTSCRNWD", "APLKYBD", "APLTEXT", "APPLICATION",
  "APPLID", "ASRAINTRPT", "ASRAKEY", "ASRAPSW", "ASRAREGS",
  "ASRASPC", "ASRASTG", "BRIDGE", "BTRANS", "CMDSEC", "COLOR",
  "CWALENG", "DEFSCRNHT", "DEFSCRNWD", "DELIMITER", "DESTCOUNT",
  "DESTID", "DESTIDLENG", "DSSCS", "DS3270", "ERRORMSG",
  "ERRORMSGLEN", "EWASUPP", "EXTDS", "FACILITY", "FCI",
  "GCHARS", "GCODES", "GMMI", "GMEXITOPT", "HILIGHT",
  "INITPARM", "INITPARMLEN", "INPARTN", "INVOKINGPROG",
  "KATAKANA", "LANGINUSE", "LDCMNEM", "LDCNUM", "LINKLEVEL",
  "LOCALCCSID", "MAPCOLUMN", "MAPHEIGHT", "MAPLINE", "MAPWIDTH",
  "MSRCONTROL", "NATLANGINUSE", "NETNAME", "NEXTTRANSID",
  "NUMTAB", "OPCLASS", "OPERKEYS", "OPID", "OPSECURITY",
  "ORGABCODE", "OUTLINE", "PAGENUM", "PARTNPAGE", "PARTNS",
  "PARTNSET", "PLATFORM", "PRINSYSID", "PROCESSTYPE", "PS",
  "QNAME", "RESSEC", "RESTART", "RETURNPROG", "SCRNHT",
  "SCRNWD", "SIGDATA", "SOSI", "STARTCODE", "STATIONID",
  "TASKPRIORITY", "TCTUALENG", "TELLERID", "TERMCODE",
  "TERMPRIORITY", "TEXTKYBD", "TEXTPRINT", "TNADDR",
  "TNIPFAMILY", "TNPORT", "TRANPRIORITY", "TWALENG",
  "UNATTEND", "USERID", "USERNAME", "USERPRIORITY", "VALIDATION",
  // ADDRESS options
  "ACEE", "CWA", "EIB", "TCTUA", "TWA",
  // WRITE OPERATOR options
  "ROUTECODES", "NUMROUTES", "ACTION", "CRITICAL", "EVENTUAL",
  "REPLY", "MAXLENGTH", "TIMEOUT",
  // RESP/RESP2
  "RESP", "RESP2",
]);

const CICS_CONDITIONS = new Set([
  "CBIDERR", "CHANNELERR", "CINVREQ", "CONTAINERERR", "DUPKEY",
  "DUPREC", "ENDDATA", "ENDFILE", "ENDINPT", "ENDOUTPT",
  "ENQBUSY", "ENVDEFERR", "EOC", "EODS", "ERROR", "EXPIRED",
  "FILENOTFOUND", "ILLOGIC", "INBFMH", "INCOMPLETE", "INQBUSY",
  "INVERRTERM", "INVEST", "INVMPSZ", "INVREQ", "IOERR",
  "ISCINVREQ", "ITEMERR", "LENGERR", "LINKABEND", "MAPFAIL",
  "NATLANGERR", "NOSTART", "NOSPACE", "NOSPOOL", "NOSTG",
  "NOTALLOC", "NOTAUTH", "NOTFND", "NORMAL", "NOTOPEN",
  "PGMIDERR", "QBUSY", "QIDERR", "QZERO", "READONLY",
  "RECORDBUSY", "REQIDERR", "ROLLEDBACK", "SESSIONERR", "SIGNOFF",
  "SPOLBUSY", "SPOLERR", "SUPPRESSED", "SYSBUSY", "SYSIDERR",
  "TASKIDERR", "TERMERR", "TERMIDERR", "TIMERERR", "TRANSIDERR",
  "TSIOERR", "UNEXPIN", "WRBRK",
]);

// ---- Token scanning ----

const WORD_RE = /[A-Za-z][A-Za-z0-9_-]*/g;
const NUMBER_RE = /(?<!\w)[+-]?\d+(?:\.\d+)?(?!\w)/g;
const STRING_RE = /'[^']*'|"[^"]*"/g;

export type SemanticToken = {
  line: number;
  startChar: number;
  length: number;
  tokenType: number;
  tokenModifiers: number;
};

/**
 * Emit semantic tokens for a single line inside an EXEC CICS block.
 * Classifies CICS verbs, options, conditions, and data names.
 * Returns the updated paren depth for multi-line tracking.
 */
function emitCicsTokens(
  lang: string,
  langStart: number,
  lineNo: number,
  tokens: SemanticToken[],
  dataItemNames: Set<string>,
  paragraphNames: Set<string>,
  defSites: Set<string>,
  parenDepth: number,
): number {
  // 1) String literals
  STRING_RE.lastIndex = 0;
  let sm: RegExpExecArray | null;
  const stringRanges: { start: number; end: number }[] = [];
  while ((sm = STRING_RE.exec(lang)) !== null) {
    const start = langStart + sm.index;
    tokens.push({
      line: lineNo,
      startChar: start,
      length: sm[0].length,
      tokenType: 4, // string
      tokenModifiers: 0,
    });
    stringRanges.push({ start: sm.index, end: sm.index + sm[0].length });
  }

  const inStr = (idx: number) =>
    stringRanges.some(r => idx >= r.start && idx < r.end);

  // 2) Numeric literals (outside strings)
  NUMBER_RE.lastIndex = 0;
  let nm: RegExpExecArray | null;
  while ((nm = NUMBER_RE.exec(lang)) !== null) {
    if (inStr(nm.index)) continue;
    tokens.push({
      line: lineNo,
      startChar: langStart + nm.index,
      length: nm[0].length,
      tokenType: 5, // number
      tokenModifiers: 0,
    });
  }

  // 3) Build per-character paren depth map (carrying over from previous lines)
  let depth = parenDepth;
  const depthAt: number[] = new Array(lang.length).fill(0);
  for (let i = 0; i < lang.length; i++) {
    if (lang[i] === '(' && !inStr(i)) depth++;
    depthAt[i] = depth;
    if (lang[i] === ')' && !inStr(i) && depth > 0) {
      depth--;
      depthAt[i] = depth;
    }
  }

  // 4) Classify words
  WORD_RE.lastIndex = 0;
  let wm: RegExpExecArray | null;
  while ((wm = WORD_RE.exec(lang)) !== null) {
    if (inStr(wm.index)) continue;
    const word = wm[0];
    const upper = word.toUpperCase();
    const absChar = langStart + wm.index;
    const length = word.length;
    const isDef = defSites.has(`${lineNo}:${absChar}`);
    const inParens = depthAt[wm.index] > 0;

    if (inParens) {
      // Inside parentheses: data name / paragraph name reference
      if (dataItemNames.has(upper)) {
        tokens.push({
          line: lineNo, startChar: absChar, length,
          tokenType: 1, // variable
          tokenModifiers: isDef ? 1 : 0,
        });
      } else if (paragraphNames.has(upper)) {
        tokens.push({
          line: lineNo, startChar: absChar, length,
          tokenType: 2, // function (paragraph)
          tokenModifiers: isDef ? 1 : 0,
        });
      }
      // else: unknown word in parens — leave unstyled (data name not in index)
      continue;
    }

    // Outside parentheses: CICS keyword classification
    if (upper === "EXEC" || upper === "CICS" || upper === "END-EXEC") {
      tokens.push({
        line: lineNo, startChar: absChar, length,
        tokenType: 8, // macro
        tokenModifiers: 0,
      });
      continue;
    }

    if (CICS_VERBS.has(upper)) {
      tokens.push({
        line: lineNo, startChar: absChar, length,
        tokenType: 2, // function (CICS verb)
        tokenModifiers: 0,
      });
      continue;
    }

    if (CICS_CONDITIONS.has(upper)) {
      tokens.push({
        line: lineNo, startChar: absChar, length,
        tokenType: 9, // type (CICS condition)
        tokenModifiers: 0,
      });
      continue;
    }

    if (CICS_OPTIONS.has(upper)) {
      tokens.push({
        line: lineNo, startChar: absChar, length,
        tokenType: 11, // property (CICS option)
        tokenModifiers: 0,
      });
      continue;
    }

    // Unknown word outside parens — leave unstyled
  }

  return depth; // carry forward paren depth for next line
}

/**
 * Emit semantic tokens for a single line inside an EXEC DLI block.
 * Classifies DLI verbs (short & long), clause keywords, and data names.
 * Returns the updated paren depth for multi-line tracking.
 */
function emitDliTokens(
  lang: string,
  langStart: number,
  lineNo: number,
  tokens: SemanticToken[],
  dataItemNames: Set<string>,
  paragraphNames: Set<string>,
  defSites: Set<string>,
  parenDepth: number,
): number {
  // 1) String literals
  STRING_RE.lastIndex = 0;
  let sm: RegExpExecArray | null;
  const stringRanges: { start: number; end: number }[] = [];
  while ((sm = STRING_RE.exec(lang)) !== null) {
    const start = langStart + sm.index;
    tokens.push({
      line: lineNo,
      startChar: start,
      length: sm[0].length,
      tokenType: 4, // string
      tokenModifiers: 0,
    });
    stringRanges.push({ start: sm.index, end: sm.index + sm[0].length });
  }

  const inStr = (idx: number) =>
    stringRanges.some(r => idx >= r.start && idx < r.end);

  // 2) Numeric literals (outside strings)
  NUMBER_RE.lastIndex = 0;
  let nm: RegExpExecArray | null;
  while ((nm = NUMBER_RE.exec(lang)) !== null) {
    if (inStr(nm.index)) continue;
    tokens.push({
      line: lineNo,
      startChar: langStart + nm.index,
      length: nm[0].length,
      tokenType: 5, // number
      tokenModifiers: 0,
    });
  }

  // 3) Build per-character paren depth map (carrying over from previous lines)
  let depth = parenDepth;
  const depthAt: number[] = new Array(lang.length).fill(0);
  for (let i = 0; i < lang.length; i++) {
    if (lang[i] === '(' && !inStr(i)) depth++;
    depthAt[i] = depth;
    if (lang[i] === ')' && !inStr(i) && depth > 0) {
      depth--;
      depthAt[i] = depth;
    }
  }

  // 4) Classify words
  WORD_RE.lastIndex = 0;
  let wm: RegExpExecArray | null;
  while ((wm = WORD_RE.exec(lang)) !== null) {
    if (inStr(wm.index)) continue;
    const word = wm[0];
    const upper = word.toUpperCase();
    const absChar = langStart + wm.index;
    const length = word.length;
    const isDef = defSites.has(`${lineNo}:${absChar}`);
    const inParens = depthAt[wm.index] > 0;

    if (inParens) {
      // Inside parentheses: data name / paragraph name reference
      if (dataItemNames.has(upper)) {
        tokens.push({
          line: lineNo, startChar: absChar, length,
          tokenType: 1, // variable
          tokenModifiers: isDef ? 1 : 0,
        });
      } else if (paragraphNames.has(upper)) {
        tokens.push({
          line: lineNo, startChar: absChar, length,
          tokenType: 2, // function (paragraph)
          tokenModifiers: isDef ? 1 : 0,
        });
      }
      // else: unknown word in parens — leave unstyled
      continue;
    }

    // Outside parentheses: DLI keyword classification
    if (upper === "EXEC" || upper === "DLI" || upper === "END-EXEC") {
      tokens.push({
        line: lineNo, startChar: absChar, length,
        tokenType: 8, // macro
        tokenModifiers: 0,
      });
      continue;
    }

    if (DLI_VERBS.has(upper)) {
      tokens.push({
        line: lineNo, startChar: absChar, length,
        tokenType: 2, // function (DLI verb)
        tokenModifiers: 0,
      });
      continue;
    }

    if (DLI_VERB_WORDS.has(upper)) {
      tokens.push({
        line: lineNo, startChar: absChar, length,
        tokenType: 2, // function (DLI long-form verb word)
        tokenModifiers: 0,
      });
      continue;
    }

    if (DLI_CLAUSE_KEYWORDS.has(upper)) {
      tokens.push({
        line: lineNo, startChar: absChar, length,
        tokenType: 11, // property (DLI clause)
        tokenModifiers: 0,
      });
      continue;
    }

    // Unknown word outside parens — leave unstyled
  }

  return depth; // carry forward paren depth for next line
}

/**
 * Build semantic tokens for the entire document.
 */
export function buildSemanticTokens(
  doc: TextDocument,
  indexOverride?: DefinitionIndex,
): SemanticToken[] {
  const text = doc.getText();
  const lines = text.split(/\r?\n/);
  const tokens: SemanticToken[] = [];

  const index = indexOverride ?? buildDefinitionIndex(doc);

  // Build lookup sets for fast classification
  const paragraphNames = new Set(index.paragraphs.map(p => p.name));
  const sectionNames = new Set(index.sections.map(s => s.name));
  const dataItemNames = new Set(index.dataItems.map(d => d.name));

  // Build definition site lookup: "line:character" -> true
  const defSites = new Set<string>();
  for (const p of index.paragraphs) defSites.add(`${p.line}:${p.character}`);
  for (const s of index.sections) defSites.add(`${s.line}:${s.character}`);
  for (const d of index.dataItems) defSites.add(`${d.line}:${d.character}`);

  let inDataDivision = false;
  let inProcedureDivision = false;
  let inExecCics = false;
  let cicsParenDepth = 0;
  let inExecDli = false;
  let dliParenDepth = 0;

  for (let lineNo = 0; lineNo < lines.length; lineNo++) {
    const full = lines[lineNo];
    if (!hasFixedColumns(full)) continue;
    const indicator = full[6];

    // Sequence area (cols 1-6): skip — not semantically interesting

    // Comment lines: entire lang area is a comment
    if (isFixedCommentIndicator(indicator)) {
      const langEnd = Math.min(full.length, 72);
      if (langEnd > 7) {
        tokens.push({
          line: lineNo,
          startChar: 6,
          length: langEnd - 6,
          tokenType: 6, // comment
          tokenModifiers: 0,
        });
      }
      continue;
    }

    if (!isValidFixedIndicator(indicator)) continue;

    const langStart = 7;
    const langEnd = Math.min(full.length, 72);
    const lang = full.slice(langStart, langEnd);
    const trimmed = lang.trimStart();

    // Track EXEC CICS blocks — provide CICS-specific semantic tokens
    const hasExecCics = /\bEXEC\s+CICS\b/i.test(lang);
    const hasExecDli = /\bEXEC\s+DLI\b/i.test(lang);
    const hasEndExec = /\bEND-EXEC\b/i.test(lang);

    if (hasExecCics && hasEndExec) {
      // Single-line EXEC CICS ... END-EXEC
      cicsParenDepth = 0;
      cicsParenDepth = emitCicsTokens(lang, langStart, lineNo, tokens,
        dataItemNames, paragraphNames, defSites, cicsParenDepth);
      inExecCics = false;
      cicsParenDepth = 0;
      continue;
    }
    if (hasExecCics) {
      cicsParenDepth = 0;
      cicsParenDepth = emitCicsTokens(lang, langStart, lineNo, tokens,
        dataItemNames, paragraphNames, defSites, cicsParenDepth);
      inExecCics = true;
      continue;
    }
    if (hasEndExec && inExecCics) {
      cicsParenDepth = emitCicsTokens(lang, langStart, lineNo, tokens,
        dataItemNames, paragraphNames, defSites, cicsParenDepth);
      inExecCics = false;
      cicsParenDepth = 0;
      continue;
    }
    if (inExecCics) {
      cicsParenDepth = emitCicsTokens(lang, langStart, lineNo, tokens,
        dataItemNames, paragraphNames, defSites, cicsParenDepth);
      continue;
    }

    // Track EXEC DLI blocks — provide DLI-specific semantic tokens
    if (hasExecDli && hasEndExec) {
      // Single-line EXEC DLI ... END-EXEC
      dliParenDepth = 0;
      dliParenDepth = emitDliTokens(lang, langStart, lineNo, tokens,
        dataItemNames, paragraphNames, defSites, dliParenDepth);
      inExecDli = false;
      dliParenDepth = 0;
      continue;
    }
    if (hasExecDli) {
      dliParenDepth = 0;
      dliParenDepth = emitDliTokens(lang, langStart, lineNo, tokens,
        dataItemNames, paragraphNames, defSites, dliParenDepth);
      inExecDli = true;
      continue;
    }
    if (hasEndExec && inExecDli) {
      dliParenDepth = emitDliTokens(lang, langStart, lineNo, tokens,
        dataItemNames, paragraphNames, defSites, dliParenDepth);
      inExecDli = false;
      dliParenDepth = 0;
      continue;
    }
    if (inExecDli) {
      dliParenDepth = emitDliTokens(lang, langStart, lineNo, tokens,
        dataItemNames, paragraphNames, defSites, dliParenDepth);
      continue;
    }

    // Track division
    if (/^(IDENTIFICATION|ID)\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = false;
      inProcedureDivision = false;
    } else if (/^ENVIRONMENT\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = false;
      inProcedureDivision = false;
    } else if (/^DATA\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = true;
      inProcedureDivision = false;
    } else if (/^PROCEDURE\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = false;
      inProcedureDivision = true;
    }

    // 1) String literals
    STRING_RE.lastIndex = 0;
    let sm: RegExpExecArray | null;
    const stringRanges: { start: number; end: number }[] = [];
    while ((sm = STRING_RE.exec(lang)) !== null) {
      const start = langStart + sm.index;
      const length = sm[0].length;
      tokens.push({
        line: lineNo,
        startChar: start,
        length,
        tokenType: 4, // string
        tokenModifiers: 0,
      });
      stringRanges.push({ start: sm.index, end: sm.index + sm[0].length });
    }

    // Helper: check if an offset in lang is inside a string
    const inString = (idx: number) =>
      stringRanges.some(r => idx >= r.start && idx < r.end);

    // 2) Numeric literals (outside strings)
    NUMBER_RE.lastIndex = 0;
    let nm: RegExpExecArray | null;
    while ((nm = NUMBER_RE.exec(lang)) !== null) {
      if (inString(nm.index)) continue;
      const start = langStart + nm.index;
      tokens.push({
        line: lineNo,
        startChar: start,
        length: nm[0].length,
        tokenType: 5, // number
        tokenModifiers: 0,
      });
    }

    // 3) Words: classify as keyword, variable, function, namespace, etc.
    WORD_RE.lastIndex = 0;
    let wm: RegExpExecArray | null;
    while ((wm = WORD_RE.exec(lang)) !== null) {
      if (inString(wm.index)) continue;

      const word = wm[0];
      const upper = word.toUpperCase();
      const absChar = langStart + wm.index;
      const length = word.length;
      const isDef = defSites.has(`${lineNo}:${absChar}`);

      // Level numbers at start of data items
      if (inDataDivision && /^\d{1,2}$/.test(word)) {
        const level = parseInt(word, 10);
        if ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
          17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
          31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,
          45, 46, 47, 48, 49, 66, 77, 78, 88].includes(level)) {
          tokens.push({
            line: lineNo,
            startChar: absChar,
            length,
            tokenType: 9, // type (level number)
            tokenModifiers: 0,
          });
          continue;
        }
      }

      // Paragraph names
      if (paragraphNames.has(upper)) {
        tokens.push({
          line: lineNo,
          startChar: absChar,
          length,
          tokenType: 2, // function (paragraph)
          tokenModifiers: isDef ? 1 : 0, // declaration modifier
        });
        continue;
      }

      // Section names
      if (sectionNames.has(upper)) {
        tokens.push({
          line: lineNo,
          startChar: absChar,
          length,
          tokenType: 3, // namespace (section)
          tokenModifiers: isDef ? 1 : 0,
        });
        continue;
      }

      // Data item names
      if (dataItemNames.has(upper)) {
        tokens.push({
          line: lineNo,
          startChar: absChar,
          length,
          tokenType: 1, // variable
          tokenModifiers: isDef ? 1 : 0,
        });
        continue;
      }

      // Special registers / figurative constants
      if (SPECIAL_REGISTERS.has(upper)) {
        tokens.push({
          line: lineNo,
          startChar: absChar,
          length,
          tokenType: 10, // parameter
          tokenModifiers: 2, // readonly
        });
        continue;
      }

      // COPY / EXEC keywords
      if (upper === "COPY" || upper === "EXEC" || upper === "END-EXEC") {
        tokens.push({
          line: lineNo,
          startChar: absChar,
          length,
          tokenType: 8, // macro
          tokenModifiers: 0,
        });
        continue;
      }

      // Division/Section structural keywords
      if (DIVISION_SECTION_KEYWORDS.has(upper)) {
        tokens.push({
          line: lineNo,
          startChar: absChar,
          length,
          tokenType: 0, // keyword
          tokenModifiers: 0,
        });
        continue;
      }

      // Data description keywords
      if (DATA_KEYWORDS.has(upper)) {
        tokens.push({
          line: lineNo,
          startChar: absChar,
          length,
          tokenType: 0, // keyword
          tokenModifiers: 0,
        });
        continue;
      }

      // COBOL verbs (in procedure division)
      if (COBOL_KEYWORDS.has(upper)) {
        tokens.push({
          line: lineNo,
          startChar: absChar,
          length,
          tokenType: 0, // keyword
          tokenModifiers: 0,
        });
        continue;
      }

      // PIC clause content (e.g. X(10), 9(5)V99 — leave unclassified or as type)
      // No default token for unknown words — let textmate grammar handle them
    }
  }

  // Sort tokens by line, then by startChar (LSP requires ascending order)
  tokens.sort((a, b) => a.line - b.line || a.startChar - b.startChar);

  return tokens;
}

/**
 * Encode semantic tokens into the LSP delta format.
 * Returns the `data` array: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers, ...]
 */
export function encodeSemanticTokens(tokens: SemanticToken[]): number[] {
  const data: number[] = [];
  let prevLine = 0;
  let prevChar = 0;

  for (const t of tokens) {
    const deltaLine = t.line - prevLine;
    const deltaChar = deltaLine === 0 ? t.startChar - prevChar : t.startChar;

    data.push(deltaLine, deltaChar, t.length, t.tokenType, t.tokenModifiers);

    prevLine = t.line;
    prevChar = t.startChar;
  }

  return data;
}
