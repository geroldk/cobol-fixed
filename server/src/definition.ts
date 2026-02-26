/**
 * Go-to-Definition provider for COBOL fixed-format.
 *
 * Supports:
 *  - COPY book names   → opens the resolved copybook file
 *  - Paragraph names    → jumps to paragraph definition (PERFORM, GO TO, …)
 *  - Section names      → jumps to section definition
 *  - Data names         → jumps to data item definition in DATA DIVISION
 */
import { Location, Position, Range, TextDocumentPositionParams } from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import {
  hasFixedColumns,
  isFixedCommentIndicator,
  isValidFixedIndicator,
  resolveCopybook,
  uriFromFsPath,
} from "./utils";

// ---- Word-at-cursor helpers ----

const COBOL_WORD_RE = /[A-Za-z0-9_-]+/g;

/**
 * Extracts the COBOL word (identifier) at the given cursor position.
 * Returns the word text (uppercased) and its character range in the line.
 */
export function wordAtPosition(
  lineText: string,
  character: number,
): { word: string; start: number; end: number } | undefined {
  COBOL_WORD_RE.lastIndex = 0;
  let m: RegExpExecArray | null;
  while ((m = COBOL_WORD_RE.exec(lineText)) !== null) {
    const start = m.index;
    const end = start + m[0].length;
    if (character >= start && character <= end) {
      return { word: m[0].toUpperCase(), start, end };
    }
  }
  return undefined;
}

// ---- Index builders (lightweight, per-request) ----

export type ParagraphDef = { name: string; line: number; character: number; endCharacter: number };
export type SectionDef = { name: string; line: number; character: number; endCharacter: number };
export type DataItemDef = {
  name: string;
  level: number;
  line: number;
  character: number;
  endCharacter: number;
  pic?: string;
  usage?: string;
};

export type DefinitionIndex = {
  paragraphs: ParagraphDef[];
  sections: SectionDef[];
  dataItems: DataItemDef[];
};

export type SymbolDef = {
  name: string;
  line: number;
  character: number;
  endCharacter: number;
};

/**
 * Scans the document and collects paragraph names, section names, and data items.
 */
export function buildDefinitionIndex(doc: TextDocument): DefinitionIndex {
  const text = doc.getText();
  const lines = text.split(/\r?\n/);
  const paragraphs: ParagraphDef[] = [];
  const sections: SectionDef[] = [];
  const dataItems: DataItemDef[] = [];

  let inDataDivision = false;
  let inProcedureDivision = false;

  for (let lineNo = 0; lineNo < lines.length; lineNo++) {
    const full = lines[lineNo];
    if (!hasFixedColumns(full)) continue;
    const indicator = full[6];
    if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) continue;
    if (indicator === "-") continue; // continuation

    const langStart = 7;
    const langEnd = Math.min(full.length, 72);
    const lang = full.slice(langStart, langEnd);
    const trimmed = lang.trimStart();
    const indent = lang.length - trimmed.length;

    // Division tracking
    if (/^(IDENTIFICATION|ID)\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = false;
      inProcedureDivision = false;
      continue;
    }
    if (/^ENVIRONMENT\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = false;
      inProcedureDivision = false;
      continue;
    }
    if (/^DATA\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = true;
      inProcedureDivision = false;
      continue;
    }
    if (/^PROCEDURE\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = false;
      inProcedureDivision = true;
      continue;
    }

    // Data items (in DATA DIVISION): level number + name
    if (inDataDivision && trimmed.length > 0) {
      const dm = /^(\d{1,2})\s+([A-Z][A-Z0-9-]*)\b/i.exec(trimmed);
      if (dm) {
        const level = parseInt(dm[1], 10);
        const name = dm[2].toUpperCase();
        if (name !== "FILLER") {
          const nameStart = langStart + indent + dm[0].indexOf(dm[2]);
          const nameEnd = nameStart + dm[2].length;

          // Try to extract PIC and USAGE from the same line
          const restOfLine = trimmed.slice(dm[0].length);
          const picMatch = /\bPIC(?:TURE)?\s+IS\s+(\S+)|\bPIC(?:TURE)?\s+(\S+)/i.exec(restOfLine);
          const usageMatch = /\bUSAGE\s+(?:IS\s+)?(\S+)|\b(COMP(?:-[1-5])?|BINARY|PACKED-DECIMAL|DISPLAY|INDEX|POINTER)\b/i.exec(restOfLine);
          const pic = picMatch ? (picMatch[1] || picMatch[2]) : undefined;
          const usage = usageMatch ? (usageMatch[1] || usageMatch[2]) : undefined;

          dataItems.push({ name, level, line: lineNo, character: nameStart, endCharacter: nameEnd, pic, usage });
        }
      }
    }

    // Sections (in PROCEDURE DIVISION)
    if (inProcedureDivision) {
      const sm = /^([A-Z][A-Z0-9-]*)\s+SECTION\.\s*$/i.exec(trimmed);
      if (sm) {
        const name = sm[1].toUpperCase();
        const nameStart = langStart + indent + trimmed.indexOf(sm[1]);
        sections.push({ name, line: lineNo, character: nameStart, endCharacter: nameStart + sm[1].length });
        continue;
      }

      // Paragraphs
      const pm = /^([A-Z][A-Z0-9-]*)\.\s*$/i.exec(trimmed);
      if (pm) {
        const name = pm[1].toUpperCase();
        // Skip END-IF, END-EVALUATE etc. — they are not paragraphs
        if (!name.startsWith("END-")) {
          const nameStart = langStart + indent;
          paragraphs.push({ name, line: lineNo, character: nameStart, endCharacter: nameStart + pm[1].length });
        }
      }
    }
  }

  return { paragraphs, sections, dataItems };
}

/**
 * Resolves a symbol name against the index in precedence order:
 * paragraph -> section -> data item.
 */
export function findSymbolInIndex(
  word: string,
  index: DefinitionIndex,
): SymbolDef | undefined {
  const para = index.paragraphs.find((p) => p.name === word);
  if (para) return para;

  const sec = index.sections.find((s) => s.name === word);
  if (sec) return sec;

  const dataItem = index.dataItems.find((d) => d.name === word);
  if (dataItem) return dataItem;

  return undefined;
}

// ---- COPY-book definition ----

/**
 * Checks whether the cursor is on a COPY book name and resolves it.
 */
export function findCopybookDefinition(
  doc: TextDocument,
  position: Position,
  baseDirs: string[],
): Location | undefined {
  const lineText = doc.getText(Range.create(position.line, 0, position.line + 1, 0)).replace(/\r?\n$/, "");
  if (!hasFixedColumns(lineText)) return undefined;

  const indicator = lineText[6];
  if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) return undefined;

  const lang = lineText.slice(7, Math.min(lineText.length, 72));

  // Check if this line has a COPY keyword
  if (!/\bCOPY\b/i.test(lang)) return undefined;

  const wordInfo = wordAtPosition(lineText, position.character);
  if (!wordInfo) return undefined;
  if (wordInfo.word === "COPY" || wordInfo.word === "REPLACING") return undefined;

  // Verify the word comes after COPY keyword
  const copyIdx = lang.search(/\bCOPY\b/i);
  if (copyIdx < 0) return undefined;
  const copyEnd = 7 + copyIdx + 4; // "COPY" is 4 chars
  if (wordInfo.start < copyEnd) return undefined;

  // Resolve the copybook
  const resolved = resolveCopybook(wordInfo.word, baseDirs);
  if (!resolved) return undefined;

  const targetUri = uriFromFsPath(resolved);
  return Location.create(targetUri, Range.create(0, 0, 0, 0));
}

// ---- Paragraph/Section/Data-Name definition ----

/**
 * Checks whether the cursor is on a paragraph/section/data-name reference
 * and finds its definition.
 */
export function findSymbolDefinition(
  doc: TextDocument,
  position: Position,
  index: DefinitionIndex,
): Location | undefined {
  const lineText = doc.getText(Range.create(position.line, 0, position.line + 1, 0)).replace(/\r?\n$/, "");

  const wordInfo = wordAtPosition(lineText, position.character);
  if (!wordInfo) return undefined;

  const symbolDef = findSymbolInIndex(wordInfo.word, index);
  if (symbolDef) {
    return Location.create(
      doc.uri,
      Range.create(symbolDef.line, symbolDef.character, symbolDef.line, symbolDef.endCharacter),
    );
  }

  return undefined;
}
