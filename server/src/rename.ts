/**
 * Rename provider for COBOL fixed-format.
 *
 * Supports renaming:
 *  - Paragraph names
 *  - Section names
 *  - Data names
 *
 * Handles COPY-expanded text via preprocessor, mapping changes back to source.
 * Validates that new names are valid COBOL identifiers.
 */
import {
  Position,
  Range,
  TextEdit,
  WorkspaceEdit,
} from "vscode-languageserver/node";
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
  findSymbolInIndex,
} from "./definition";

import { findAllOccurrences } from "./references";

// ---- COBOL identifier validation ----

const COBOL_IDENT_RE = /^[A-Za-z][A-Za-z0-9-]*[A-Za-z0-9]$|^[A-Za-z]$/;

/**
 * Validates that a name is a legal COBOL identifier.
 */
export function isValidCobolIdentifier(name: string): boolean {
  if (name.length === 0 || name.length > 30) return false;
  if (!COBOL_IDENT_RE.test(name)) return false;
  // Must not end with a hyphen
  if (name.endsWith("-")) return false;
  // Must not start with a digit
  if (/^\d/.test(name)) return false;
  return true;
}

// ---- Prepare rename ----

export type PrepareRenameResult = {
  range: Range;
  placeholder: string;
} | undefined;

/**
 * Checks if the symbol at the given position can be renamed.
 * Returns the word range and current name, or undefined if rename is not possible.
 */
export function prepareRename(
  doc: TextDocument,
  position: Position,
  index?: DefinitionIndex,
): PrepareRenameResult {
  const lineText = doc
    .getText(Range.create(position.line, 0, position.line + 1, 0))
    .replace(/\r?\n$/, "");

  if (!hasFixedColumns(lineText)) return undefined;
  const indicator = lineText[6];
  if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) return undefined;

  const wordInfo = wordAtPosition(lineText, position.character);
  if (!wordInfo) return undefined;

  // Only allow renaming known symbols (paragraphs, sections, data items)
  const idx = index ?? buildDefinitionIndex(doc);
  const sym = findSymbolInIndex(wordInfo.word, idx);
  if (!sym) return undefined;

  return {
    range: Range.create(position.line, wordInfo.start, position.line, wordInfo.end),
    placeholder: wordInfo.word,
  };
}

// ---- Perform rename ----

/**
 * Performs the rename operation, returning a WorkspaceEdit with all text edits.
 *
 * @param doc           The source document
 * @param position      Cursor position
 * @param newName       The new name for the symbol
 * @param preDoc        Optional preprocessed document (COPY expanded)
 * @param preIndex      Optional index built from preprocessed text
 * @param mapToSource   Optional function to map preprocessed offsets back to source
 */
export function performRename(
  doc: TextDocument,
  position: Position,
  newName: string,
  preDoc?: TextDocument,
  preIndex?: DefinitionIndex,
  mapToSource?: (startOff: number, endOff: number) => { uri: string; range: Range } | undefined,
): WorkspaceEdit | undefined {
  // Validate new name
  if (!isValidCobolIdentifier(newName)) return undefined;

  const lineText = doc
    .getText(Range.create(position.line, 0, position.line + 1, 0))
    .replace(/\r?\n$/, "");

  if (!hasFixedColumns(lineText)) return undefined;
  const indicator = lineText[6];
  if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) return undefined;

  const wordInfo = wordAtPosition(lineText, position.character);
  if (!wordInfo) return undefined;
  const word = wordInfo.word;

  // Decide which document/index to scan
  const scanDoc = preDoc ?? doc;
  const scanIndex = preIndex ?? buildDefinitionIndex(doc);

  // Only rename known symbols
  const sym = findSymbolInIndex(word, scanIndex);
  if (!sym) return undefined;

  // Find all occurrences (including definition)
  const occurrences = findAllOccurrences(scanDoc.getText(), word);
  if (occurrences.length === 0) return undefined;

  // Convert to uppercase since COBOL is case-insensitive
  const upperNewName = newName.toUpperCase();

  // Build workspace edit grouped by URI
  const changes: Map<string, TextEdit[]> = new Map();

  for (const occ of occurrences) {
    if (mapToSource) {
      const startOff = scanDoc.offsetAt({ line: occ.line, character: occ.character });
      const endOff = scanDoc.offsetAt({ line: occ.line, character: occ.endCharacter });
      const mapped = mapToSource(startOff, endOff);
      if (mapped) {
        const edits = changes.get(mapped.uri) ?? [];
        edits.push(TextEdit.replace(mapped.range, upperNewName));
        changes.set(mapped.uri, edits);
      }
    } else {
      const range = Range.create(occ.line, occ.character, occ.line, occ.endCharacter);
      const edits = changes.get(doc.uri) ?? [];
      edits.push(TextEdit.replace(range, upperNewName));
      changes.set(doc.uri, edits);
    }
  }

  // Convert Map to plain object
  const changesObj: { [uri: string]: TextEdit[] } = {};
  for (const [uri, edits] of changes) {
    changesObj[uri] = edits;
  }

  return { changes: changesObj };
}
