/**
 * Completion provider for COBOL fixed-format.
 *
 * Context-aware completions:
 *  - PROCEDURE DIVISION: COBOL verbs, paragraph/section names
 *  - DATA DIVISION:      level numbers, PIC/USAGE clauses
 *  - Any division:       division/section keywords
 *  - After COPY keyword: copybook names from search dirs
 *  - Data names referenced in PROCEDURE DIVISION
 */
import {
  CompletionItem,
  CompletionItemKind,
  InsertTextFormat,
  Position,
  Range,
  TextDocumentPositionParams,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";
import * as fs from "fs";
import * as path from "path";

import {
  hasFixedColumns,
  isFixedCommentIndicator,
  isValidFixedIndicator,
  fsPathFromUri,
} from "./utils";

import { buildDefinitionIndex, DefinitionIndex } from "./definition";
import {
  resolveDliRequest,
  DLI_REQUEST_SPECS,
  CICS_PRIMARY_COMMANDS,
  CICS_COMMAND_SPECS,
  CICS_KNOWN_CONDITIONS,
} from "./lint";

/**
 * CICS options that are flags (Boolean switches) and do NOT take a
 * parenthesized argument.  Every option NOT in this set gets a snippet
 * with `OPTION(${1:placeholder})`.
 */
const CICS_FLAG_OPTIONS = new Set([
  "AFTER", "ALARM", "ASIS", "AT", "AUTOPAGE", "AUXILIARY", "BELOW",
  "CANCEL", "CICSDATAKEY", "COMPLETE", "CONSISTENT", "CRITICAL",
  "DATAONLY", "DEBKEY", "DEBREC", "EQUAL", "ERASE", "EVENTUAL",
  "EXCEPTION", "EXECUTABLE", "FCT", "FOR", "FREEKB", "FRSET",
  "GENERIC", "GTEQ", "HOLD", "IMMEDIATE", "LAST", "MAIN", "MAPONLY",
  "MASSINSERT", "NEXT", "NOAUTOPAGE", "NOCHECK", "NODUMP", "NOSUSPEND",
  "NOTRUNCATE", "OPERPURGE", "PAGING", "PCT", "PPT", "RBA", "RELEASE",
  "RESET", "RETAIN", "REWRITE", "RRN", "SHARED", "SIT", "STORAGE",
  "SYNCONRETURN", "TABLES", "TASK", "TCT", "TERMINAL", "UNTIL", "UOW",
  "UPDATE", "USERDATAKEY", "WAIT", "XRBA",
]);

/**
 * Per-option placeholder text for CICS options that take a specific
 * argument pattern other than a simple data-name.
 * Options not listed here default to `name`.
 */
const CICS_OPTION_PLACEHOLDER: Record<string, string> = {
  // ADDRESS command → pointer references
  ACEE:    "ADDRESS OF name",
  CWA:     "ADDRESS OF name",
  EIB:     "ADDRESS OF name",
  TCTUA:   "ADDRESS OF name",
  TWA:     "ADDRESS OF name",
  // SET / ENTRY / DATAPOINTER → pointer references
  SET:          "ADDRESS OF name",
  ENTRY:        "ADDRESS OF name",
  DATAPOINTER:  "ADDRESS OF name",
  ECADDR:       "ADDRESS OF name",
  // length/size options
  LENGTH:       "len",
  FLENGTH:      "len",
  DATALENGTH:   "len",
  KEYLENGTH:    "len",
  FROMLENGTH:   "len",
  INPUTMSGLEN:  "len",
  TEXTLENGTH:   "len",
  MAXLENGTH:    "len",
  SEGLENGTH:    "len",
  FEEDBACKLEN:  "len",
  NUMROUTES:    "len",
  MAXLIFETIME:  "len",
  // numeric/time values
  INTERVAL:     "hhmmss",
  TIME:         "hhmmss",
  TIMEOUT:      "secs",
  HOURS:        "hh",
  MINUTES:      "mm",
  SECONDS:      "ss",
  MILLISECS:    "ms",
  ABSTIME:      "abstime",
  // identifiers / names
  FILE:         "name",
  DATASET:      "name",
  PROGRAM:      "name",
  TRANSID:      "name",
  TERMID:       "name",
  QUEUE:        "name",
  MAP:          "name",
  MAPSET:       "name",
  CHANNEL:      "name",
  REQID:        "name",
  SYSID:        "name",
  RESOURCE:     "name",
  DUMPCODE:     "name",
  ABCODE:       "name",
  // data areas
  INTO:         "area",
  FROM:         "area",
  COMMAREA:     "area",
  INITIMG:      "char",
  // item / count
  ITEM:         "n",
  NUMITEMS:     "n",
};

export type CompletionSourceLocation = {
  uri: string;
  line: number;
  character: number;
};

// ---- COBOL verb list (COBOL 85 standard) ----

const COBOL_VERBS: string[] = [
  "ACCEPT", "ADD", "ALTER", "CALL", "CANCEL", "CLOSE", "COMPUTE", "CONTINUE",
  "COPY", "DELETE", "DISPLAY", "DIVIDE", "ELSE", "END-EVALUATE", "END-IF",
  "END-PERFORM", "END-READ", "END-RETURN", "END-REWRITE", "END-SEARCH",
  "END-START", "END-STRING", "END-SUBTRACT", "END-UNSTRING", "END-WRITE",
  "EVALUATE", "EXEC", "EXIT", "GO", "GOBACK",
  "IF", "INITIALIZE", "INITIATE", "INSPECT", "MERGE", "MOVE", "MULTIPLY",
  "NEXT", "NOT", "OPEN", "PERFORM", "READ", "RELEASE", "RETURN", "REWRITE",
  "SEARCH", "SET", "SORT", "START", "STOP", "STRING", "SUBTRACT",
  "UNSTRING", "WRITE", "WHEN",
];

const DIVISION_KEYWORDS: string[] = [
  "IDENTIFICATION DIVISION.",
  "ENVIRONMENT DIVISION.",
  "DATA DIVISION.",
  "PROCEDURE DIVISION.",
];

const SECTION_KEYWORDS: string[] = [
  "CONFIGURATION SECTION.",
  "INPUT-OUTPUT SECTION.",
  "FILE SECTION.",
  "WORKING-STORAGE SECTION.",
  "LINKAGE SECTION.",
  "LOCAL-STORAGE SECTION.",
  "COMMUNICATION SECTION.",
  "REPORT SECTION.",
  "SCREEN SECTION.",
];

const DATA_CLAUSES: string[] = [
  "PIC", "PICTURE", "VALUE", "USAGE", "OCCURS", "REDEFINES",
  "COMP", "COMP-1", "COMP-2", "COMP-3", "COMP-4", "COMP-5",
  "BINARY", "PACKED-DECIMAL", "DISPLAY",
  "INDEXED", "DEPENDING", "ASCENDING", "DESCENDING",
  "JUSTIFIED", "BLANK", "SIGN", "SYNCHRONIZED",
];

// ---- EXEC DLI completion data ----

/** DLI HLPI function short-form codes with descriptions. */
const DLI_FUNCTIONS: { label: string; detail: string; snippet?: string }[] = [
  { label: "GU",   detail: "Get Unique – unqualifizierter Zugriff auf ein Segment", snippet: "GU\n      USING PCB(${1:n})\n      SEGMENT(${2:name}) INTO(${3:area})" },
  { label: "GN",   detail: "Get Next – naechstes Segment lesen" ,                   snippet: "GN\n      USING PCB(${1:n})\n      SEGMENT(${2:name}) INTO(${3:area})" },
  { label: "GNP",  detail: "Get Next in Parent – naechstes Segment unter Parent" ,   snippet: "GNP\n      USING PCB(${1:n})\n      SEGMENT(${2:name}) INTO(${3:area})" },
  { label: "GHU",  detail: "Get Hold Unique – fuer Update/Delete sperren" ,          snippet: "GHU\n      USING PCB(${1:n})\n      SEGMENT(${2:name}) INTO(${3:area})" },
  { label: "GHN",  detail: "Get Hold Next – naechstes Segment gesperrt lesen" ,      snippet: "GHN\n      USING PCB(${1:n})\n      SEGMENT(${2:name}) INTO(${3:area})" },
  { label: "GHNP", detail: "Get Hold Next in Parent – gesperrt unter Parent" ,       snippet: "GHNP\n      USING PCB(${1:n})\n      SEGMENT(${2:name}) INTO(${3:area})" },
  { label: "ISRT", detail: "Insert – neues Segment einfuegen" ,                      snippet: "ISRT\n      USING PCB(${1:n})\n      SEGMENT(${2:name}) FROM(${3:area})" },
  { label: "REPL", detail: "Replace – Segment aktualisieren (nach GET HOLD)" ,       snippet: "REPL\n      USING PCB(${1:n})\n      SEGMENT(${2:name}) FROM(${3:area})" },
  { label: "DLET", detail: "Delete – Segment loeschen (nach GET HOLD)" ,             snippet: "DLET\n      USING PCB(${1:n})\n      SEGMENT(${2:name}) FROM(${3:area})" },
  { label: "LOAD", detail: "Load – Segment in leere Datenbank laden" ,               snippet: "LOAD\n      USING PCB(${1:n})\n      SEGMENT(${2:name}) FROM(${3:area})" },
  { label: "CHKP", detail: "Checkpoint – Sicherungspunkt setzen" ,                   snippet: "CHKP\n      ID(${1:area})" },
  { label: "SCHD", detail: "Schedule – PSB zuweisen" ,                               snippet: "SCHD\n      PSB(${1:name})" },
  { label: "TERM", detail: "Terminate – PSB freigeben" },
];

/** DLI long-form function keywords. */
const DLI_LONG_FUNCTIONS: { label: string; detail: string; short: string }[] = [
  { label: "GET UNIQUE",              detail: "Get Unique (= GU)",                short: "GU" },
  { label: "GET NEXT",                detail: "Get Next (= GN)",                  short: "GN" },
  { label: "GET NEXT IN PARENT",      detail: "Get Next in Parent (= GNP)",       short: "GNP" },
  { label: "GET HOLD UNIQUE",         detail: "Get Hold Unique (= GHU)",          short: "GHU" },
  { label: "GET HOLD NEXT",           detail: "Get Hold Next (= GHN)",            short: "GHN" },
  { label: "GET HOLD NEXT IN PARENT", detail: "Get Hold Next in Parent (= GHNP)", short: "GHNP" },
  { label: "INSERT",                  detail: "Insert (= ISRT)",                  short: "ISRT" },
  { label: "REPLACE",                 detail: "Replace (= REPL)",                 short: "REPL" },
  { label: "DELETE",                  detail: "Delete (= DLET)",                  short: "DLET" },
  { label: "LOAD",                    detail: "Load",                             short: "LOAD" },
  { label: "CHECKPOINT",              detail: "Checkpoint (= CHKP)",              short: "CHKP" },
  { label: "SCHEDULE",                detail: "Schedule (= SCHD)",                short: "SCHD" },
  { label: "TERMINATE",               detail: "Terminate (= TERM)",               short: "TERM" },
];

/** DLI clause keywords with descriptions. */
const DLI_CLAUSES: { label: string; detail: string; snippet?: string }[] = [
  { label: "USING PCB",    detail: "PCB-Referenz angeben",                  snippet: "USING PCB(${1:n})" },
  { label: "SEGMENT",      detail: "Segmentname angeben",                   snippet: "SEGMENT(${1:name})" },
  { label: "INTO",          detail: "Zielbereich fuer gelesene Daten",      snippet: "INTO(${1:area})" },
  { label: "FROM",          detail: "Quellbereich fuer Schreibdaten",       snippet: "FROM(${1:area})" },
  { label: "WHERE",         detail: "Suchbedingung (SSA-Qualifikation)",    snippet: "WHERE(${1:field}=${2:value})" },
  { label: "FIELDLENGTH",  detail: "Feld-Laengenangabe fuer WHERE",         snippet: "FIELDLENGTH(${1:len})" },
  { label: "SEGLENGTH",    detail: "Segment-Laengenangabe (variable Laenge)", snippet: "SEGLENGTH(${1:len})" },
  { label: "KEYFEEDBACK",  detail: "Schluessel-Feedback-Bereich",           snippet: "KEYFEEDBACK(${1:area})" },
  { label: "FEEDBACKLEN",  detail: "Laenge des Keyfeedback-Bereichs",       snippet: "FEEDBACKLEN(${1:len})" },
  { label: "OFFSET",        detail: "Start-Offset im Segment",              snippet: "OFFSET(${1:n})" },
  { label: "LOCKED",        detail: "Segment exklusiv sperren" },
  { label: "FIRST",         detail: "Erstes Segment im Parent waehlen" },
  { label: "LAST",          detail: "Letztes Segment im Parent waehlen" },
  { label: "VARIABLE",      detail: "Variable Segment-Laenge" },
  { label: "PSB",            detail: "PSB-Name fuer SCHEDULE",              snippet: "PSB(${1:name})" },
  { label: "ID",             detail: "Checkpoint-Kennung",                  snippet: "ID(${1:area})" },
];

/**
 * Returns the current division context for a given position in the document.
 */
function getDivisionContext(doc: TextDocument, line: number): "identification" | "environment" | "data" | "procedure" | "unknown" {
  const text = doc.getText();
  const lines = text.split(/\r?\n/);

  for (let i = line; i >= 0; i--) {
    const full = lines[i];
    if (!hasFixedColumns(full)) continue;
    const indicator = full[6];
    if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) continue;
    const lang = full.slice(7, Math.min(full.length, 72)).trimStart();

    if (/^PROCEDURE\s+DIVISION\b/i.test(lang)) return "procedure";
    if (/^DATA\s+DIVISION\b/i.test(lang)) return "data";
    if (/^ENVIRONMENT\s+DIVISION\b/i.test(lang)) return "environment";
    if (/^(IDENTIFICATION|ID)\s+DIVISION\b/i.test(lang)) return "identification";
  }
  return "unknown";
}

/**
 * Check if cursor is right after COPY keyword (for copybook name completion).
 */
function isAfterCopyKeyword(lineText: string, character: number): boolean {
  if (!hasFixedColumns(lineText)) return false;
  const lang = lineText.slice(7, Math.min(lineText.length, 72));
  const before = lineText.slice(0, character);
  return /\bCOPY\s+$/i.test(before) || /\bCOPY\s+[A-Z0-9-]*$/i.test(before);
}

/**
 * Collect copybook file names from the given search directories.
 */
function collectCopybookNames(baseDirs: string[]): string[] {
  const names = new Set<string>();
  const extensions = new Set([".cpy", ".cob", ".cbl"]);

  for (const dir of baseDirs) {
    try {
      if (!fs.existsSync(dir) || !fs.statSync(dir).isDirectory()) continue;
      const entries = fs.readdirSync(dir);
      for (const entry of entries) {
        const ext = path.extname(entry).toLowerCase();
        if (extensions.has(ext)) {
          const baseName = path.basename(entry, path.extname(entry)).toUpperCase();
          names.add(baseName);
        }
      }
    } catch {
      // directory not accessible
    }
  }

  return Array.from(names).sort();
}

/**
 * Get the partial word being typed at the cursor position.
 */
function getPartialWord(lineText: string, character: number): { partial: string; start: number } {
  let start = character;
  while (start > 0 && /[A-Za-z0-9_-]/.test(lineText[start - 1])) {
    start--;
  }
  return { partial: lineText.slice(start, character).toUpperCase(), start };
}

/**
 * Detect if cursor is inside an EXEC DLI ... END-EXEC block.
 * Returns:
 *  - { context: "function" }                – directly after "EXEC DLI" (offer function names)
 *  - { context: "clause", request: "GU" }   – further into the block (offer clauses filtered by function)
 *  - undefined                               – not inside EXEC DLI
 */
function getExecDliContext(doc: TextDocument, line: number, character: number): { context: "function" | "clause" | "argument"; request?: string; validNextClauses?: Set<string>; argKeyword?: string } | undefined {
  const text = doc.getText();
  const lines = text.split(/\r?\n/);

  // Walk backwards to find most recent EXEC DLI or END-EXEC
  let foundExecDli = false;
  let execDliLine = -1;
  for (let i = line; i >= 0; i--) {
    const full = lines[i];
    if (!full || full.length < 7) continue;
    const indicator = full[6];
    if (indicator === "*" || indicator === "/") continue;
    const content = full.slice(7, Math.min(full.length, 72));

    // If we find END-EXEC before EXEC DLI, we are outside
    if (/\bEND-EXEC\b/i.test(content) && i < line) return undefined;

    if (/\bEXEC\s+DLI\b/i.test(content)) {
      foundExecDli = true;
      execDliLine = i;
      break;
    }
  }

  if (!foundExecDli) return undefined;

  // Also check that no END-EXEC comes between EXEC DLI line and cursor line
  for (let i = execDliLine + 1; i <= line; i++) {
    const full = lines[i];
    if (!full || full.length < 7) continue;
    const indicator = full[6];
    if (indicator === "*" || indicator === "/") continue;
    const content = full.slice(7, Math.min(full.length, 72));
    if (/\bEND-EXEC\b/i.test(content)) return undefined;
  }

  // Count non-comment tokens between EXEC DLI and cursor to determine context
  // If we're right after EXEC DLI (same line, just 1-2 tokens), offer functions.
  // Otherwise offer clauses — filtered by the resolved DLI function.
  const execLine = lines[execDliLine];
  const execContent = execLine.slice(7, Math.min(execLine.length, 72));
  const afterExecDli = execContent.replace(/^.*?\bEXEC\s+DLI\b\s*/i, "");
  // When cursor is on the EXEC DLI line, compute how much of afterExecDli
  // to keep (truncate at cursor column).
  const execDliMatchOffset = execContent.length - afterExecDli.length;
  const cursorColFromLang = character - 7; // cursor column relative to lang area start

  // If cursor is on the same line as EXEC DLI and nothing after EXEC DLI yet
  if (execDliLine === line) {
    const trimmed = afterExecDli.trim();
    if (!trimmed) return { context: "function" };
    const tokens = trimmed.split(/\s+/);
    if (tokens.length <= 1) return { context: "function" };
  }

  // Collect all text from the EXEC DLI block (from EXEC DLI to cursor)
  // to resolve which DLI function is active.
  // When cursor is on the EXEC DLI line, truncate afterExecDli at cursor column.
  let blockText: string;
  if (execDliLine === line) {
    const endInAfter = cursorColFromLang - execDliMatchOffset;
    blockText = endInAfter > 0 ? afterExecDli.slice(0, endInAfter) : afterExecDli;
  } else {
    blockText = afterExecDli;
  }
  for (let i = execDliLine + 1; i <= line; i++) {
    const full = lines[i];
    if (!full || full.length < 7) continue;
    const indicator = full[6];
    if (indicator === "*" || indicator === "/") continue;
    // On the cursor line, only include text up to the cursor column
    // so that closing parens after the cursor don't affect depth tracking.
    const endCol = i === line ? character : Math.min(full.length, 72);
    blockText += " " + full.slice(7, endCol);
  }

  // Tokenize and resolve the DLI request using the shared resolveDliRequest
  const wordRe = /[A-Za-z0-9]+(-[A-Za-z0-9]+)*/g;
  const blockTokens: { upper: string; start: number; end: number }[] = [
    { upper: "DLI", start: 0, end: 3 },  // synthetic DLI token at index 0
  ];
  let m: RegExpExecArray | null;
  while ((m = wordRe.exec(blockText)) !== null) {
    blockTokens.push({ upper: m[0].toUpperCase(), start: m.index, end: m.index + m[0].length });
  }

  const { request, requestTokenCount } = resolveDliRequest(blockTokens);

  if (!request) {
    // No function recognized yet → still offer function names
    return { context: "function" };
  }

  // ---- Paren-depth check: is cursor inside parentheses? ----
  // Walk blockText to find the last unclosed '(' and the keyword preceding it.
  let parenDepth = 0;
  let lastOpenParenKeyword = "";
  {
    const kwRe = /\b([A-Z][A-Z0-9-]*)\s*\(/gi;
    // Track all paren opens/closes to determine depth at end (= cursor position)
    for (let ci = 0; ci < blockText.length; ci++) {
      if (blockText[ci] === "(") {
        parenDepth++;
        if (parenDepth === 1) {
          // Find the keyword before this '('
          const before = blockText.slice(0, ci).trimEnd();
          const kwMatch = before.match(/([A-Z][A-Z0-9-]*)$/i);
          lastOpenParenKeyword = kwMatch ? kwMatch[1].toUpperCase() : "";
        }
      } else if (blockText[ci] === ")") {
        if (parenDepth > 0) {
          parenDepth--;
          if (parenDepth === 0) lastOpenParenKeyword = "";
        }
      }
    }
  }

  if (parenDepth > 0 && lastOpenParenKeyword) {
    return { context: "argument", request, argKeyword: lastOpenParenKeyword };
  }

  // Compute structurally valid next clauses based on IBM HLPI ordering rules
  const validNextClauses = computeValidNextClauses(blockTokens, request, requestTokenCount);

  return { context: "clause", request, validNextClauses };
}

/**
 * Detect if cursor is inside an EXEC CICS ... END-EXEC block.
 * Returns:
 *  - { context: "verb" }                      – directly after "EXEC CICS" (offer CICS verbs)
 *  - { context: "option", command, spec }      – after verb (offer command-specific options)
 *  - { context: "argument" }                   – inside parentheses (offer data items)
 *  - undefined                                  – not inside EXEC CICS
 *
 * `cicsPartial` is the effective partial word within the EXEC CICS block
 * (independent of the generic getPartialWord which might pick up "CICS" itself).
 */
function getExecCicsContext(
  doc: TextDocument,
  line: number,
  character: number
): { context: "verb" | "subverb" | "option" | "argument"; command?: string; spec?: { allowedOptions: Set<string> }; subVerbs?: string[]; cicsPartial: string } | undefined {
  const text = doc.getText();
  const lines = text.split(/\r?\n/);

  // Walk backwards to find most recent EXEC CICS or END-EXEC
  let foundExecCics = false;
  let execCicsLine = -1;
  for (let i = line; i >= 0; i--) {
    const full = lines[i];
    if (!full || full.length < 7) continue;
    const indicator = full[6];
    if (indicator === "*" || indicator === "/") continue;
    const content = full.slice(7, Math.min(full.length, 72));

    // If we find END-EXEC before EXEC CICS on a previous line, we are outside
    if (/\bEND-EXEC\b/i.test(content) && i < line) return undefined;

    if (/\bEXEC\s+CICS\b/i.test(content)) {
      foundExecCics = true;
      execCicsLine = i;
      break;
    }
  }

  if (!foundExecCics) return undefined;

  // Check that no END-EXEC comes between EXEC CICS line and cursor line
  for (let i = execCicsLine + 1; i <= line; i++) {
    const full = lines[i];
    if (!full || full.length < 7) continue;
    const indicator = full[6];
    if (indicator === "*" || indicator === "/") continue;
    const content = full.slice(7, Math.min(full.length, 72));
    if (/\bEND-EXEC\b/i.test(content)) return undefined;
  }

  // Collect all text from EXEC CICS to cursor
  const execLine = lines[execCicsLine];
  const execContent = execLine.slice(7, Math.min(execLine.length, 72));
  const afterExecCics = execContent.replace(/^.*?\bEXEC\s+CICS\b\s*/i, "");

  let blockText: string;
  if (execCicsLine === line) {
    const execMatchOffset = execContent.length - afterExecCics.length;
    const cursorColFromLang = character - 7;
    const endInAfter = cursorColFromLang - execMatchOffset;
    blockText = endInAfter > 0 ? afterExecCics.slice(0, endInAfter) : "";
  } else {
    blockText = afterExecCics;
  }

  for (let i = execCicsLine + 1; i <= line; i++) {
    const full = lines[i];
    if (!full || full.length < 7) continue;
    const indicator = full[6];
    if (indicator === "*" || indicator === "/") continue;
    const endCol = i === line ? character : Math.min(full.length, 72);
    blockText += " " + full.slice(7, endCol);
  }

  // Compute the effective partial word from block text (not from raw line)
  // If blockText ends with a word character, the last word is a partial being typed
  let cicsPartial = "";
  const partialMatch = blockText.match(/([A-Za-z0-9][-A-Za-z0-9]*)$/);
  if (partialMatch) {
    cicsPartial = partialMatch[1].toUpperCase();
  }

  // Tokenize the block text
  const wordRe = /[A-Za-z0-9]+(-[A-Za-z0-9]+)*/g;
  const tokens: string[] = [];
  let m: RegExpExecArray | null;
  while ((m = wordRe.exec(blockText)) !== null) {
    tokens.push(m[0].toUpperCase());
  }

  // Check paren depth — if inside parens, offer data items
  let parenDepth = 0;
  for (let ci = 0; ci < blockText.length; ci++) {
    if (blockText[ci] === "(") parenDepth++;
    else if (blockText[ci] === ")" && parenDepth > 0) parenDepth--;
  }
  if (parenDepth > 0) {
    return { context: "argument", cicsPartial };
  }

  // No tokens yet after EXEC CICS → offer verbs
  if (tokens.length === 0) {
    return { context: "verb", cicsPartial: "" };
  }

  // Try to match a CICS command (multi-word first, then single-word)
  // Build candidate commands from CICS_COMMAND_SPECS keys
  const specKeys = Object.keys(CICS_COMMAND_SPECS);
  // Sort by word count descending (match longest first)
  specKeys.sort((a, b) => b.split(" ").length - a.split(" ").length);

  let matchedCommand: string | undefined;
  let matchedSpec: { allowedOptions: Set<string> } | undefined;

  for (const cmdName of specKeys) {
    const cmdWords = cmdName.split(" ");
    if (cmdWords.length > tokens.length) continue;
    let matches = true;
    for (let j = 0; j < cmdWords.length; j++) {
      if (tokens[j] !== cmdWords[j]) { matches = false; break; }
    }
    if (matches) {
      matchedCommand = cmdName;
      matchedSpec = CICS_COMMAND_SPECS[cmdName];
      break;
    }
  }

  if (!matchedCommand) {
    // Check if tokens are a prefix of multi-word commands (e.g. HANDLE → CONDITION/AID/ABEND)
    const endsWithSpace = /\s$/.test(blockText);
    if (endsWithSpace && tokens.length >= 1) {
      const nextWords = new Set<string>();
      for (const cmdName of specKeys) {
        const cmdWords = cmdName.split(" ");
        if (cmdWords.length <= tokens.length) continue;
        let prefixMatch = true;
        for (let j = 0; j < tokens.length; j++) {
          if (tokens[j] !== cmdWords[j]) { prefixMatch = false; break; }
        }
        if (prefixMatch) {
          nextWords.add(cmdWords[tokens.length]);
        }
      }
      if (nextWords.size > 0) {
        return { context: "subverb", subVerbs: [...nextWords], cicsPartial: "" };
      }
    }
    // First token might be a partial verb being typed — offer verbs
    if (tokens.length === 1) {
      return { context: "verb", cicsPartial: endsWithSpace ? "" : cicsPartial };
    }
    // Unknown command but we're in EXEC CICS — still offer verbs
    return { context: "verb", cicsPartial: endsWithSpace ? "" : cicsPartial };
  }

  // Compute effective partial for options: if blockText ends with whitespace,
  // no partial is being typed. Otherwise, check if the last token is beyond
  // the command name tokens.
  const cmdWordCount = matchedCommand.split(" ").length;
  let optionPartial = "";
  if (tokens.length > cmdWordCount && !blockText.match(/\s$/)) {
    optionPartial = cicsPartial;
  }

  // We matched a command → offer its options
  return { context: "option", command: matchedCommand, spec: matchedSpec, cicsPartial: optionPartial };
}
/**
 * Compute which DLI clause keywords are structurally valid at the current cursor
 * position, based on IBM HLPI ordering rules (Spec Section 5.3–5.5).
 *
 * IBM HLPI structure:
 *   EXEC DLI <function>
 *     [USING PCB(n)]                              -- global, before 1st SEGMENT
 *     [KEYFEEDBACK(ref) [FEEDBACKLEN(len)]]       -- global, before 1st SEGMENT (GET only)
 *     { [FIRST|LAST] [VARIABLE] SEGMENT(name)     -- segment block
 *       [INTO(ref)|FROM(ref)]                     -- transfer, after SEGMENT
 *       [LOCKED]                                  -- after INTO (GET only)
 *       [OFFSET(exp)]                             -- after transfer
 *       [SEGLENGTH(exp)]                          -- after transfer
 *       [WHERE(...) [FIELDLENGTH(...)]]           -- qualification
 *     }
 *   END-EXEC
 */
function computeValidNextClauses(
  blockTokens: { upper: string }[],
  request: string,
  requestTokenCount: number
): Set<string> {
  const spec = DLI_REQUEST_SPECS[request];
  if (!spec) return new Set();

  const allowed = spec.allowed;
  const GET_FUNCTIONS = new Set(["GU", "GN", "GNP", "GHU", "GHN", "GHNP"]);
  const isGet = GET_FUNCTIONS.has(request);

  const CLAUSE_KW = new Set([
    "USING", "VARIABLE", "SEGMENT", "INTO", "FROM", "SEGLENGTH",
    "WHERE", "FIELDLENGTH", "KEYFEEDBACK", "FEEDBACKLEN",
    "LOCKED", "OFFSET", "FIRST", "LAST", "PSB", "ID",
  ]);

  // ---- Global flags ----
  let hasUsing = false;
  let hasKeyfeedback = false;
  let hasFeedbacklen = false;
  let firstSegmentSeen = false;

  // ---- Per-segment-block flags (reset on each SEGMENT) ----
  let segHasTransfer = false;
  let segTransferIsInto = false;
  let segHasLocked = false;
  let segHasOffset = false;
  let segHasSeglength = false;
  let segHasWhere = false;
  let segHasFieldlength = false;

  // ---- Prefix state ----
  // "none"             – normal state
  // "after-first-last" – FIRST or LAST just seen → expect VARIABLE or SEGMENT
  // "after-variable"   – VARIABLE just seen → expect SEGMENT
  let prefixState: "none" | "after-first-last" | "after-variable" = "none";

  // Walk clause tokens from after the function name
  const clauseStart = 1 + requestTokenCount; // 1 for synthetic DLI token
  for (let i = clauseStart; i < blockTokens.length; i++) {
    const u = blockTokens[i].upper;
    if (!CLAUSE_KW.has(u)) continue;

    switch (u) {
      case "USING":
        hasUsing = true;
        prefixState = "none";
        break;
      case "KEYFEEDBACK":
        hasKeyfeedback = true;
        prefixState = "none";
        break;
      case "FEEDBACKLEN":
        hasFeedbacklen = true;
        prefixState = "none";
        break;
      case "FIRST":
      case "LAST":
        prefixState = "after-first-last";
        break;
      case "VARIABLE":
        prefixState = "after-variable";
        break;
      case "SEGMENT":
        firstSegmentSeen = true;
        prefixState = "none";
        segHasTransfer = false;
        segTransferIsInto = false;
        segHasLocked = false;
        segHasOffset = false;
        segHasSeglength = false;
        segHasWhere = false;
        segHasFieldlength = false;
        break;
      case "INTO":
        segHasTransfer = true;
        segTransferIsInto = true;
        prefixState = "none";
        break;
      case "FROM":
        segHasTransfer = true;
        prefixState = "none";
        break;
      case "LOCKED": segHasLocked = true; prefixState = "none"; break;
      case "OFFSET": segHasOffset = true; prefixState = "none"; break;
      case "SEGLENGTH": segHasSeglength = true; prefixState = "none"; break;
      case "WHERE": segHasWhere = true; prefixState = "none"; break;
      case "FIELDLENGTH": segHasFieldlength = true; prefixState = "none"; break;
      case "PSB": prefixState = "none"; break;
      case "ID": prefixState = "none"; break;
    }
  }

  const valid = new Set<string>();

  // ---- Prefix state: after FIRST/LAST only VARIABLE or SEGMENT ----
  if (prefixState === "after-first-last") {
    if (allowed.has("VARIABLE")) valid.add("VARIABLE");
    if (allowed.has("SEGMENT")) valid.add("SEGMENT");
    return valid;
  }
  if (prefixState === "after-variable") {
    if (allowed.has("SEGMENT")) valid.add("SEGMENT");
    return valid;
  }

  // ---- Simple functions (CHKP/SCHD/TERM) ----
  if (request === "CHKP") {
    if (allowed.has("ID")) valid.add("ID");
    return valid;
  }
  if (request === "SCHD") {
    if (allowed.has("PSB")) valid.add("PSB");
    return valid;
  }
  if (request === "TERM") {
    return valid; // no clauses
  }

  // ---- Global options (only before first SEGMENT) ----
  if (!firstSegmentSeen) {
    if (!hasUsing && allowed.has("USING")) valid.add("USING");
    if (isGet && !hasKeyfeedback && allowed.has("KEYFEEDBACK")) valid.add("KEYFEEDBACK");
    if (isGet && hasKeyfeedback && !hasFeedbacklen && allowed.has("FEEDBACKLEN")) valid.add("FEEDBACKLEN");
  }

  // ---- Segment block starters (always available if function allows SEGMENT) ----
  if (allowed.has("SEGMENT")) {
    valid.add("SEGMENT");
    if (allowed.has("FIRST")) valid.add("FIRST");
    if (allowed.has("LAST")) valid.add("LAST");
    if (allowed.has("VARIABLE")) valid.add("VARIABLE");
  }

  // ---- Within a segment block: offer sub-clauses ----
  if (firstSegmentSeen) {
    if (!segHasTransfer) {
      if (allowed.has("INTO")) valid.add("INTO");
      if (allowed.has("FROM")) valid.add("FROM");
    }
    if (isGet && segTransferIsInto && !segHasLocked && allowed.has("LOCKED")) valid.add("LOCKED");
    if (!segHasOffset && allowed.has("OFFSET")) valid.add("OFFSET");
    if (!segHasSeglength && allowed.has("SEGLENGTH")) valid.add("SEGLENGTH");
    if (!segHasWhere && allowed.has("WHERE")) valid.add("WHERE");
    if (segHasWhere && !segHasFieldlength && allowed.has("FIELDLENGTH")) valid.add("FIELDLENGTH");
  }

  // ---- REPL special: FROM/SEGLENGTH/OFFSET valid without explicit SEGMENT ----
  if (request === "REPL" && !firstSegmentSeen) {
    if (allowed.has("FROM") && !segHasTransfer) valid.add("FROM");
    if (allowed.has("SEGLENGTH") && !segHasSeglength) valid.add("SEGLENGTH");
    if (allowed.has("OFFSET") && !segHasOffset) valid.add("OFFSET");
  }

  return valid;
}

/**
 * Check if the text before the cursor ends with one of the given clause keywords
 * (possibly followed by a partial word the user is typing).
 * Returns the matched keyword or undefined.
 */
function getPrecedingClauseKeyword(before: string, keywords: string[]): string | undefined {
  const upper = before.toUpperCase();
  for (const kw of keywords) {
    // Pattern: keyword followed by optional whitespace at end (cursor right after keyword)
    // or keyword + space + partial word at end
    const r1 = new RegExp(`\\b${kw}\\s+$`);
    const r2 = new RegExp(`\\b${kw}\\s+[A-Z0-9][A-Z0-9-]*$`);
    if (r1.test(upper) || r2.test(upper)) {
      return kw;
    }
  }
  return undefined;
}

/**
 * Build completion items for the given document position.
 */
export function buildCompletionItems(
  doc: TextDocument,
  position: Position,
  baseDirs: string[],
  indexOverride?: DefinitionIndex,
  resolveSourceLocation?: (line: number, character: number) => CompletionSourceLocation | undefined,
): CompletionItem[] {
  const lineText = doc.getText(Range.create(position.line, 0, position.line + 1, 0)).replace(/\r?\n$/, "");
  const items: CompletionItem[] = [];
  const { partial } = getPartialWord(lineText, position.character);

  // 1) After COPY keyword → copybook names
  if (isAfterCopyKeyword(lineText, position.character)) {
    const names = collectCopybookNames(baseDirs);
    for (const name of names) {
      if (partial && !name.startsWith(partial)) continue;
      items.push({
        label: name,
        kind: CompletionItemKind.File,
        detail: "Copybook",
        sortText: sortKey("A", name),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }
    return items;
  }

  // 2) Inside EXEC DLI block → DLI functions, clauses, or argument values
  const dliCtx = getExecDliContext(doc, position.line, position.character);

  // 2a) Inside parenthesized argument → offer data names for data-taking keywords
  if (dliCtx?.context === "argument") {
    // Keywords whose parenthesized args are host variables / expressions (data names):
    //   INTO(ref), FROM(ref), KEYFEEDBACK(ref), WHERE(field=ref) — host variables
    //   SEGLENGTH(exp), FIELDLENGTH(exp), FEEDBACKLEN(exp), OFFSET(exp), ID(exp) — expressions (often data names)
    // NOT: SEGMENT(name) = DLI segment name, PSB(name) = PSB name, USING PCB(n) = PCB number
    const dataKeywords = new Set([
      "WHERE", "INTO", "FROM", "KEYFEEDBACK",
      "SEGLENGTH", "FIELDLENGTH", "FEEDBACKLEN", "OFFSET", "ID",
    ]);
    if (dataKeywords.has(dliCtx.argKeyword ?? "")) {
      const index = indexOverride ?? buildDefinitionIndex(doc);
      for (const d of index.dataItems) {
        if (partial && !d.name.startsWith(partial)) continue;
        const picSuffix = d.pic ? ` PIC ${d.pic}` : "";
        items.push({
          label: d.name,
          kind: CompletionItemKind.Variable,
          detail: `Level ${String(d.level).padStart(2, "0")}${picSuffix}`,
          sortText: sortKey("A", d.name),
          insertTextFormat: InsertTextFormat.PlainText,
        });
      }
    }
    // For keywords not in dataKeywords (e.g., SEGMENT, PSB, PCB) → no specific completions
    return items;
  }

  if (dliCtx?.context === "function") {
    for (const fn of DLI_FUNCTIONS) {
      if (partial && !fn.label.startsWith(partial)) continue;
      items.push({
        label: fn.label,
        kind: CompletionItemKind.Method,
        detail: `DLI ${fn.detail}`,
        sortText: sortKey("A", fn.label),
        insertText: fn.snippet ?? fn.label,
        insertTextFormat: fn.snippet ? InsertTextFormat.Snippet : InsertTextFormat.PlainText,
      });
    }
    for (const fn of DLI_LONG_FUNCTIONS) {
      if (partial && !fn.label.startsWith(partial)) continue;
      items.push({
        label: fn.label,
        kind: CompletionItemKind.Method,
        detail: `DLI ${fn.detail}`,
        sortText: sortKey("B", fn.label),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }
    return items;
  }
  if (dliCtx?.context === "clause") {
    // Determine sub-context: what keyword (if any) immediately precedes the cursor?
    // DLI clause keywords that take a parenthesized argument:
    //   USING → PCB,  SEGMENT → name,  INTO/FROM → area,  WHERE → expr,
    //   FIELDLENGTH/SEGLENGTH/FEEDBACKLEN/OFFSET → length,
    //   KEYFEEDBACK → area,  ID → area,  PSB → name
    // Standalone flags (no argument): LOCKED, FIRST, LAST, VARIABLE
    const before = lineText.slice(0, position.character);

    // Check if cursor is directly after a clause keyword (keyword + optional partial)
    const argKeywords = [
      "USING", "SEGMENT", "INTO", "FROM", "WHERE",
      "FIELDLENGTH", "SEGLENGTH", "KEYFEEDBACK", "FEEDBACKLEN",
      "OFFSET", "ID", "PSB",
    ];
    const precedingKeyword = getPrecedingClauseKeyword(before, argKeywords);

    if (precedingKeyword) {
      // After USING → only PCB
      if (precedingKeyword === "USING") {
        if (!partial || "PCB".startsWith(partial)) {
          items.push({
            label: "PCB",
            kind: CompletionItemKind.Property,
            detail: "DLI PCB-Referenz (USING PCB(n))",
            sortText: sortKey("A", "PCB"),
            insertText: "PCB(${1:n})",
            insertTextFormat: InsertTextFormat.Snippet,
          });
        }
        return items;
      }

      // After SEGMENT/INTO/FROM/WHERE/ID/PSB/KEYFEEDBACK → offer data names from index
      const showDataNames = new Set(["SEGMENT", "INTO", "FROM", "WHERE", "ID", "PSB", "KEYFEEDBACK"]);
      if (showDataNames.has(precedingKeyword)) {
        const index = indexOverride ?? buildDefinitionIndex(doc);
        for (const d of index.dataItems) {
          if (partial && !d.name.startsWith(partial)) continue;
          const picSuffix = d.pic ? ` PIC ${d.pic}` : "";
          items.push({
            label: d.name,
            kind: CompletionItemKind.Variable,
            detail: `Level ${String(d.level).padStart(2, "0")}${picSuffix}`,
            sortText: sortKey("A", d.name),
            insertTextFormat: InsertTextFormat.PlainText,
          });
        }
        return items;
      }

      // After FIELDLENGTH/SEGLENGTH/FEEDBACKLEN/OFFSET → no specific completions
      // (user types numeric value/expression manually)
      return items;
    }

    // No preceding argument-keyword → offer clause keywords valid at cursor position
    const validSet = dliCtx.validNextClauses;
    for (const cl of DLI_CLAUSES) {
      const clauseKey = cl.label.split(/\s+/)[0]; // e.g. "USING" from "USING PCB(n)"
      if (validSet && !validSet.has(clauseKey)) continue;
      if (partial && !clauseKey.startsWith(partial)) continue;
      items.push({
        label: cl.label,
        kind: CompletionItemKind.Property,
        detail: `DLI ${cl.detail}`,
        sortText: sortKey("A", cl.label),
        insertText: cl.snippet ?? cl.label,
        insertTextFormat: cl.snippet ? InsertTextFormat.Snippet : InsertTextFormat.PlainText,
      });
    }
    // Always offer END-EXEC to close the block
    if (!partial || "END-EXEC".startsWith(partial)) {
      items.push({
        label: "END-EXEC",
        kind: CompletionItemKind.Keyword,
        detail: "DLI Block abschliessen",
        sortText: sortKey("Z", "END-EXEC"),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }
    return items;
  }

  // 3) Inside EXEC CICS block → CICS verbs, options, or argument values
  const cicsCtx = getExecCicsContext(doc, position.line, position.character);

  if (cicsCtx?.context === "argument") {
    // Inside parentheses → offer data items
    const cp = cicsCtx.cicsPartial;
    const index = indexOverride ?? buildDefinitionIndex(doc);
    for (const d of index.dataItems) {
      if (cp && !d.name.startsWith(cp)) continue;
      const picSuffix = d.pic ? ` PIC ${d.pic}` : "";
      items.push({
        label: d.name,
        kind: CompletionItemKind.Variable,
        detail: `Level ${String(d.level).padStart(2, "0")}${picSuffix}`,
        sortText: sortKey("A", d.name),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }
    return items;
  }

  if (cicsCtx?.context === "verb") {
    // Offer CICS primary verbs
    const cp = cicsCtx.cicsPartial;
    for (const verb of CICS_PRIMARY_COMMANDS) {
      if (cp && !verb.startsWith(cp)) continue;
      items.push({
        label: verb,
        kind: CompletionItemKind.Keyword,
        detail: "CICS Command",
        sortText: sortKey("A", verb),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }
    return items;
  }

  if (cicsCtx?.context === "subverb" && cicsCtx.subVerbs) {
    // Offer sub-verbs (e.g. CONDITION, AID, ABEND after HANDLE)
    for (const sv of cicsCtx.subVerbs) {
      items.push({
        label: sv,
        kind: CompletionItemKind.Keyword,
        detail: "CICS Sub-Command",
        sortText: sortKey("A", sv),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }
    // Also offer END-EXEC to close the block
    items.push({
      label: "END-EXEC",
      kind: CompletionItemKind.Keyword,
      detail: "CICS Block abschliessen",
      sortText: sortKey("Z", "END-EXEC"),
      insertTextFormat: InsertTextFormat.PlainText,
    });
    return items;
  }

  if (cicsCtx?.context === "option" && cicsCtx.spec) {
    // Offer command-specific options
    const cp = cicsCtx.cicsPartial;
    for (const opt of cicsCtx.spec.allowedOptions) {
      if (cp && !opt.startsWith(cp)) continue;
      const isFlag = CICS_FLAG_OPTIONS.has(opt);
      const placeholder = CICS_OPTION_PLACEHOLDER[opt] ?? "name";
      items.push({
        label: opt,
        kind: CompletionItemKind.Property,
        detail: `CICS ${cicsCtx.command}`,
        sortText: sortKey("A", opt),
        insertText: isFlag ? opt : `${opt}(\${1:${placeholder}})`,
        insertTextFormat: isFlag ? InsertTextFormat.PlainText : InsertTextFormat.Snippet,
      });
    }
    // For HANDLE CONDITION / IGNORE CONDITION, also offer condition names
    if (cicsCtx.command === "HANDLE CONDITION" || cicsCtx.command === "IGNORE CONDITION") {
      for (const cond of CICS_KNOWN_CONDITIONS) {
        if (cp && !cond.startsWith(cp)) continue;
        items.push({
          label: cond,
          kind: CompletionItemKind.EnumMember,
          detail: "CICS Condition",
          sortText: sortKey("B", cond),
          insertText: `${cond}(\${1:label})`,
          insertTextFormat: InsertTextFormat.Snippet,
        });
      }
    }
    // Always offer END-EXEC to close the block
    if (!cp || "END-EXEC".startsWith(cp)) {
      items.push({
        label: "END-EXEC",
        kind: CompletionItemKind.Keyword,
        detail: "CICS Block abschliessen",
        sortText: sortKey("Z", "END-EXEC"),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }
    return items;
  }

  const division = getDivisionContext(doc, position.line);
  const index = indexOverride ?? buildDefinitionIndex(doc);

  // 2) PROCEDURE DIVISION: verbs + paragraph/section names + data names
  if (division === "procedure") {
    for (const verb of COBOL_VERBS) {
      if (partial && !verb.startsWith(partial)) continue;
      items.push({
        label: verb,
        kind: CompletionItemKind.Keyword,
        detail: "COBOL Verb",
        sortText: sortKey("D", verb),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }

    // Paragraph names
    for (const p of index.paragraphs) {
      if (partial && !p.name.startsWith(partial)) continue;
      const source = resolveSourceLocation?.(p.line, p.character);
      const loc = source
        ? formatLocationForDetail(doc.uri, source)
        : `Zeile ${p.line + 1}`;
      items.push({
        label: p.name,
        kind: CompletionItemKind.Function,
        detail: `Paragraph (${loc})`,
        labelDetails: {
          description: loc,
        },
        sortText: sortKey("B", p.name),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }

    // Section names
    for (const s of index.sections) {
      if (partial && !s.name.startsWith(partial)) continue;
      const source = resolveSourceLocation?.(s.line, s.character);
      const loc = source
        ? formatLocationForDetail(doc.uri, source)
        : `Zeile ${s.line + 1}`;
      items.push({
        label: s.name,
        kind: CompletionItemKind.Module,
        detail: `Section (${loc})`,
        labelDetails: {
          description: loc,
        },
        sortText: sortKey("C", s.name),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }

    // Data names (for MOVE, SET, etc.)
    for (const d of index.dataItems) {
      if (partial && !d.name.startsWith(partial)) continue;
      const picSuffix = d.pic ? ` PIC ${d.pic}` : "";
      const usageSuffix = d.usage ? ` USAGE ${d.usage}` : "";
      const compactTypeInfo = `L${String(d.level).padStart(2, "0")}${picSuffix}${usageSuffix}`;
      const source = resolveSourceLocation?.(d.line, d.character);
      const loc = source
        ? formatLocationForDetail(doc.uri, source)
        : `Zeile ${d.line + 1}`;
      items.push({
        label: d.name,
        kind: CompletionItemKind.Variable,
        detail: `Level ${String(d.level).padStart(2, "0")}${picSuffix}${usageSuffix} (${loc})`,
        labelDetails: {
          detail: ` ${compactTypeInfo}`,
          description: loc,
        },
        sortText: sortKey("A", d.name),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }

    return items;
  }

  // 3) DATA DIVISION: data clauses + level numbers
  if (division === "data") {
    for (const clause of DATA_CLAUSES) {
      if (partial && !clause.startsWith(partial)) continue;
      items.push({
        label: clause,
        kind: CompletionItemKind.Keyword,
        detail: "Data Clause",
        sortText: sortKey("D", clause),
        insertTextFormat: InsertTextFormat.PlainText,
      });
    }
    return items;
  }

  // 4) General: division and section keywords
  for (const div of DIVISION_KEYWORDS) {
    const label = div.replace(".", "");
    if (partial && !label.startsWith(partial)) continue;
    items.push({
      label: div,
      kind: CompletionItemKind.Keyword,
      detail: "Division",
      sortText: sortKey("D", div),
      insertTextFormat: InsertTextFormat.PlainText,
    });
  }

  for (const sec of SECTION_KEYWORDS) {
    const label = sec.replace(".", "");
    if (partial && !label.startsWith(partial)) continue;
    items.push({
      label: sec,
      kind: CompletionItemKind.Keyword,
      detail: "Section",
      sortText: sortKey("D", sec),
      insertTextFormat: InsertTextFormat.PlainText,
    });
  }

  return items;
}

function formatLocationForDetail(currentUri: string, source: CompletionSourceLocation): string {
  const sourceLine = source.line + 1;
  if (source.uri === currentUri) return `Zeile ${sourceLine}`;

  const fsPath = fsPathFromUri(source.uri);
  const base = fsPath ? path.basename(fsPath) : source.uri;
  return `${base}:${sourceLine}`;
}

function sortKey(bucket: string, label: string): string {
  return `${bucket}_${label}`;
}
