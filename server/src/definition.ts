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
  hasSeparatorPeriodOutsideLiterals,
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
  parentNames: string[];
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
 * COBOL clause keywords that terminate an INDEXED BY name list.
 * When scanning names after INDEXED BY, stop when hitting one of these.
 */
const INDEXED_BY_STOP_KEYWORDS = new Set([
  "PIC", "PICTURE", "VALUE", "VALUES", "USAGE", "COMP", "COMP-1", "COMP-2",
  "COMP-3", "COMP-4", "COMP-5", "BINARY", "PACKED-DECIMAL", "DISPLAY",
  "INDEX", "POINTER", "BLANK", "JUSTIFIED", "JUST", "SYNCHRONIZED", "SYNC",
  "REDEFINES", "RENAMES", "OCCURS", "DEPENDING", "ASCENDING", "DESCENDING",
  "EXTERNAL", "GLOBAL", "SIGN", "LEADING", "TRAILING", "SEPARATE",
]);

/**
 * Extract index names from INDEXED BY clauses on a single line.
 * If `isContinuation` is true, we treat the entire lang area as continuation
 * of a previous INDEXED BY clause (names until a stop keyword or period).
 */
function extractIndexedByNames(
  lang: string,
  langStart: number,
  lineNo: number,
  dataItems: DataItemDef[],
  isContinuation: boolean,
): void {
  let searchArea: string;
  let areaOffset: number;

  if (isContinuation) {
    // Continuation of INDEXED BY from previous line — scan from beginning of lang
    searchArea = lang;
    areaOffset = 0;
  } else {
    const ibm = /\bINDEXED\s+BY\b/i.exec(lang);
    if (!ibm) return;
    // Start scanning names after "INDEXED BY"
    areaOffset = ibm.index + ibm[0].length;
    searchArea = lang.slice(areaOffset);
  }

  const nameRe = /[A-Z][A-Z0-9-]*/gi;
  let nm: RegExpExecArray | null;
  while ((nm = nameRe.exec(searchArea)) !== null) {
    const idxName = nm[0].toUpperCase();
    if (INDEXED_BY_STOP_KEYWORDS.has(idxName)) break;
    // Skip if it looks like a level number (pure digits handled by regex requiring alpha start)
    const nameCharStart = langStart + areaOffset + nm.index;
    dataItems.push({
      name: idxName,
      level: 1,
      line: lineNo,
      character: nameCharStart,
      endCharacter: nameCharStart + nm[0].length,
      parentNames: [],
      usage: "INDEX",
    });
  }
}

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
  let inEnvironmentDivision = false;
  let pendingIndexedBy = false;
  const dataStack: { level: number; name: string }[] = [];

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
      inEnvironmentDivision = false;
      continue;
    }
    if (/^ENVIRONMENT\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = false;
      inProcedureDivision = false;
      inEnvironmentDivision = true;
      continue;
    }
    if (/^DATA\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = true;
      inProcedureDivision = false;
      inEnvironmentDivision = false;
      continue;
    }
    if (/^PROCEDURE\s+DIVISION\b/i.test(trimmed)) {
      inDataDivision = false;
      inProcedureDivision = true;
      inEnvironmentDivision = false;
      continue;
    }

    // SELECT file names (in ENVIRONMENT DIVISION / FILE-CONTROL)
    if (inEnvironmentDivision) {
      const selm = /^SELECT\s+([A-Z][A-Z0-9-]*)\b/i.exec(trimmed);
      if (selm) {
        const name = selm[1].toUpperCase();
        const nameStart = langStart + indent + selm[0].indexOf(selm[1]);
        dataItems.push({
          name,
          level: 0,
          line: lineNo,
          character: nameStart,
          endCharacter: nameStart + selm[1].length,
          parentNames: [],
        });
      }
    }

    // FD / SD file descriptions (in DATA DIVISION)
    if (inDataDivision && trimmed.length > 0) {
      const fdm = /^(FD|SD)\s+([A-Z][A-Z0-9-]*)\b/i.exec(trimmed);
      if (fdm) {
        const name = fdm[2].toUpperCase();
        const nameStart = langStart + indent + fdm[0].indexOf(fdm[2]);
        dataItems.push({
          name,
          level: 0,
          line: lineNo,
          character: nameStart,
          endCharacter: nameStart + fdm[2].length,
          parentNames: [],
        });
        dataStack.length = 0; // reset stack for new file
      }
    }

    // Data items (in DATA DIVISION): level number + name
    if (inDataDivision && trimmed.length > 0) {
      const dm = /^(\d{1,2})\s+([A-Z][A-Z0-9-]*)\b/i.exec(trimmed);
      if (dm) {
        const level = parseInt(dm[1], 10);
        const name = dm[2].toUpperCase();

        while (dataStack.length > 0 && dataStack[dataStack.length - 1].level >= level) {
          dataStack.pop();
        }
        const parentNames = dataStack.map((s) => s.name);

        if (name !== "FILLER") {
          const nameStart = langStart + indent + dm[0].indexOf(dm[2]);
          const nameEnd = nameStart + dm[2].length;

          // Try to extract PIC and USAGE from the same line
          const restOfLine = trimmed.slice(dm[0].length);
          const picMatch = /\bPIC(?:TURE)?\s+IS\s+(\S+)|\bPIC(?:TURE)?\s+(\S+)/i.exec(restOfLine);
          const usageMatch = /\bUSAGE\s+(?:IS\s+)?(\S+)|\b(COMP(?:-[1-5])?|BINARY|PACKED-DECIMAL|DISPLAY|INDEX|POINTER)\b/i.exec(restOfLine);
          const pic = picMatch ? (picMatch[1] || picMatch[2]) : undefined;
          const usage = usageMatch ? (usageMatch[1] || usageMatch[2]) : undefined;

          dataItems.push({ name, level, line: lineNo, character: nameStart, endCharacter: nameEnd, pic, usage, parentNames });
        }

        if (level !== 66 && level !== 77 && level !== 88) {
          dataStack.push({ level, name });
        }

        // A new level entry terminates any pending INDEXED BY continuation
        pendingIndexedBy = false;
      }

      // Extract INDEXED BY names from this line or a continuation line
      extractIndexedByNames(lang, langStart, lineNo, dataItems, pendingIndexedBy);
      // Check if INDEXED BY clause continues to the next line:
      // continues if INDEXED BY was found and line has no period
      if (/\bINDEXED\s+BY\b/i.test(lang)) {
        pendingIndexedBy = !hasSeparatorPeriodOutsideLiterals(trimmed);
      } else if (pendingIndexedBy) {
        // Continuation line: stop if it has a period or starts a new level
        pendingIndexedBy = !hasSeparatorPeriodOutsideLiterals(trimmed);
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

export function getQualifiers(doc: TextDocument, startLine: number, startChar: number): string[] {
  const qualifiers: string[] = [];
  let currentLine = startLine;
  let charOffset = startChar;

  let state: "EXPECT_OF" | "EXPECT_NAME" = "EXPECT_OF";

  while (currentLine < doc.lineCount && currentLine <= startLine + 5) {
    const lineText = doc.getText(Range.create(currentLine, 0, currentLine + 1, 0)).replace(/\r?\n$/, "");
    if (!hasFixedColumns(lineText)) {
      currentLine++;
      charOffset = 7;
      continue;
    }
    const indicator = lineText[6];
    if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) {
      currentLine++;
      charOffset = 7;
      continue;
    }

    const lang = lineText.slice(charOffset, Math.min(lineText.length, 72));

    const tokens = [...lang.matchAll(/[A-Za-z0-9_-]+/g)];
    for (const match of tokens) {
      const word = match[0].toUpperCase();
      if (state === "EXPECT_OF") {
        if (word === "OF" || word === "IN") {
          state = "EXPECT_NAME";
        } else {
          return qualifiers;
        }
      } else if (state === "EXPECT_NAME") {
        qualifiers.push(word);
        state = "EXPECT_OF";
      }
    }

    currentLine++;
    charOffset = 7;
  }

  return qualifiers;
}

/**
 * Resolves a symbol name against the index in precedence order:
 * paragraph -> section -> data item.
 */
export function findSymbolInIndex(
  word: string,
  index: DefinitionIndex,
  qualifiers: string[] = [],
): SymbolDef | undefined {
  const para = index.paragraphs.find((p) => p.name === word);
  if (para) return para;

  const sec = index.sections.find((s) => s.name === word);
  if (sec) return sec;

  const candidates = index.dataItems.filter((d) => d.name === word);
  if (candidates.length === 0) return undefined;
  if (candidates.length === 1 && qualifiers.length === 0) return candidates[0];

  if (qualifiers.length > 0) {
    for (const cand of candidates) {
      let match = true;
      let parentIdx = cand.parentNames.length - 1;
      for (const qual of qualifiers) {
        let found = false;
        while (parentIdx >= 0) {
          if (cand.parentNames[parentIdx] === qual) {
            found = true;
            parentIdx--;
            break;
          }
          parentIdx--;
        }
        if (!found) {
          match = false;
          break;
        }
      }
      if (match) return cand;
    }
  }

  return candidates[0];
}

/**
 * Returns a symbol definition exactly at the given source position, if any.
 */
export function findSymbolDefinedAt(
  index: DefinitionIndex,
  line: number,
  character: number,
): SymbolDef | undefined {
  const para = index.paragraphs.find((p) => p.line === line && p.character === character);
  if (para) return para;

  const sec = index.sections.find((s) => s.line === line && s.character === character);
  if (sec) return sec;

  const data = index.dataItems.find((d) => d.line === line && d.character === character);
  if (data) return data;

  return undefined;
}

/**
 * Resolves a symbol occurrence by preferring exact definition sites and explicit qualifiers.
 *
 * For ambiguous unqualified data-item references, returns undefined.
 */
export function resolveSymbolAtOccurrence(
  doc: TextDocument,
  index: DefinitionIndex,
  word: string,
  line: number,
  character: number,
  endCharacter: number,
): SymbolDef | undefined {
  const exactDef = findSymbolDefinedAt(index, line, character);
  if (exactDef && exactDef.name === word) return exactDef;

  const qualifiers = getQualifiers(doc, line, endCharacter);
  if (qualifiers.length > 0) {
    return findSymbolInIndex(word, index, qualifiers);
  }

  const para = index.paragraphs.find((p) => p.name === word);
  if (para) return para;

  const sec = index.sections.find((s) => s.name === word);
  if (sec) return sec;

  const candidates = index.dataItems.filter((d) => d.name === word);
  if (candidates.length === 1) return candidates[0];
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

  const qualifiers = getQualifiers(doc, position.line, wordInfo.end);
  const symbolDef = findSymbolInIndex(wordInfo.word, index, qualifiers);

  if (symbolDef) {
    return Location.create(
      doc.uri,
      Range.create(symbolDef.line, symbolDef.character, symbolDef.line, symbolDef.endCharacter),
    );
  }

  return undefined;
}
