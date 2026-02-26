/**
 * References provider for COBOL fixed-format.
 *
 * Finds all usages (references) of:
 *  - Paragraph names
 *  - Section names
 *  - Data names
 *
 * Optionally includes the declaration itself.
 */
import { Location, Position, Range } from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import {
  hasFixedColumns,
  isFixedCommentIndicator,
  isValidFixedIndicator,
} from "./utils";

import {
  wordAtPosition,
  buildDefinitionIndex,
  DefinitionIndex,
  SymbolDef,
  findSymbolInIndex,
} from "./definition";

// ---- Reference result ----

export type ReferenceLocation = {
  uri: string;
  line: number;
  character: number;
  endCharacter: number;
};

// ---- Helpers ----

const COBOL_WORD_RE = /[A-Za-z][A-Za-z0-9_-]*/g;

/**
 * Scans all code lines in `text` and finds every occurrence of `symbolName`
 * as a standalone COBOL word (case-insensitive).
 *
 * Returns character-level locations (0-based line numbers).
 */
export function findAllOccurrences(
  text: string,
  symbolName: string,
): ReferenceLocation[] {
  const upper = symbolName.toUpperCase();
  const lines = text.split(/\r?\n/);
  const results: ReferenceLocation[] = [];

  for (let lineNo = 0; lineNo < lines.length; lineNo++) {
    const full = lines[lineNo];
    if (!hasFixedColumns(full)) continue;
    const indicator = full[6];
    if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) continue;
    if (indicator === "-") continue; // continuation line

    // Scan lang area only (cols 8-72)
    const langStart = 7;
    const langEnd = Math.min(full.length, 72);
    const lang = full.slice(langStart, langEnd);

    COBOL_WORD_RE.lastIndex = 0;
    let m: RegExpExecArray | null;
    while ((m = COBOL_WORD_RE.exec(lang)) !== null) {
      if (m[0].toUpperCase() === upper) {
        const charStart = langStart + m.index;
        const charEnd = charStart + m[0].length;
        results.push({
          uri: "",
          line: lineNo,
          character: charStart,
          endCharacter: charEnd,
        });
      }
    }
  }

  return results;
}

/**
 * Checks whether a given occurrence is a definition (paragraph/section header
 * or data item declaration) rather than just a reference.
 */
export function isDefinitionSite(
  occ: ReferenceLocation,
  index: DefinitionIndex,
): boolean {
  // Check paragraphs
  for (const p of index.paragraphs) {
    if (p.line === occ.line && p.character === occ.character) return true;
  }
  // Check sections
  for (const s of index.sections) {
    if (s.line === occ.line && s.character === occ.character) return true;
  }
  // Check data items
  for (const d of index.dataItems) {
    if (d.line === occ.line && d.character === occ.character) return true;
  }
  return false;
}

/**
 * Build references for the word at the given position.
 *
 * @param doc              The source document
 * @param position         Cursor position
 * @param includeDeclaration  Whether to include the declaration itself
 * @param preDoc           Optional preprocessed document (COPY expanded)
 * @param preIndex         Optional index built from preprocessed text
 * @param mapToSource      Optional function to map preprocessed locations back to source
 */
export function buildReferences(
  doc: TextDocument,
  position: Position,
  includeDeclaration: boolean,
  preDoc?: TextDocument,
  preIndex?: DefinitionIndex,
  mapToSource?: (startOff: number, endOff: number) => { uri: string; range: Range } | undefined,
): Location[] {
  const lineText = doc
    .getText(Range.create(position.line, 0, position.line + 1, 0))
    .replace(/\r?\n$/, "");

  if (!hasFixedColumns(lineText)) return [];
  const indicator = lineText[6];
  if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) return [];

  const wordInfo = wordAtPosition(lineText, position.character);
  if (!wordInfo) return [];
  const word = wordInfo.word;

  // Decide which document/index to scan
  const scanDoc = preDoc ?? doc;
  const scanIndex = preIndex ?? buildDefinitionIndex(doc);

  // Check that the word is a known symbol
  const sym = findSymbolInIndex(word, scanIndex);
  if (!sym) return [];

  // Find all occurrences in the scan document
  const occurrences = findAllOccurrences(scanDoc.getText(), word);
  const results: Location[] = [];

  for (const occ of occurrences) {
    const isDef = isDefinitionSite(occ, scanIndex);
    if (isDef && !includeDeclaration) continue;

    if (mapToSource) {
      // Map from preprocessed offsets back to source locations
      const startOff = scanDoc.offsetAt({ line: occ.line, character: occ.character });
      const endOff = scanDoc.offsetAt({ line: occ.line, character: occ.endCharacter });
      const mapped = mapToSource(startOff, endOff);
      if (mapped) {
        results.push(Location.create(mapped.uri, mapped.range));
      }
    } else {
      results.push(
        Location.create(
          doc.uri,
          Range.create(occ.line, occ.character, occ.line, occ.endCharacter),
        ),
      );
    }
  }

  return results;
}

/**
 * Collects all references for a known symbol name across the given text.
 * This is used by rename and dead-code analysis which already know the symbol name.
 *
 * Returns raw ReferenceLocation[] without URI — caller must assign URIs.
 */
export function collectReferencesForSymbol(
  text: string,
  symbolName: string,
  index: DefinitionIndex,
  includeDeclaration: boolean,
): ReferenceLocation[] {
  const occurrences = findAllOccurrences(text, symbolName);
  if (includeDeclaration) return occurrences;
  return occurrences.filter((occ) => !isDefinitionSite(occ, index));
}
