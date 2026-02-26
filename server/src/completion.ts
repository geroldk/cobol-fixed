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
