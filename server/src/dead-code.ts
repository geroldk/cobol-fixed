/**
 * Dead-Code detection for COBOL fixed-format.
 *
 * Identifies unreferenced paragraphs and sections in the PROCEDURE DIVISION.
 * These are likely dead code that can be safely removed.
 *
 * Entry paragraphs (the first paragraph, and the paragraph containing
 * STOP RUN / GOBACK) are excluded from dead-code warnings.
 *
 * PERFORM ... THRU/THROUGH ranges are respected: all paragraphs between
 * the start and end paragraph of a PERFORM THRU are considered referenced.
 */
import { DiagnosticSeverity } from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import {
  hasFixedColumns,
  isFixedCommentIndicator,
  isValidFixedIndicator,
} from "./utils";

import {
  buildDefinitionIndex,
  DefinitionIndex,
  ParagraphDef,
  SectionDef,
} from "./definition";

import { findAllOccurrences, isDefinitionSite } from "./references";

import { GenDiag } from "./types";

// ---- Dead-code analysis result ----

export type DeadCodeItem = {
  kind: "paragraph" | "section";
  name: string;
  line: number;
  character: number;
  endCharacter: number;
};

// ---- PERFORM THRU detection ----

const PERFORM_THRU_RE = /\bPERFORM\s+([A-Z][A-Z0-9-]*)\s+(?:THRU|THROUGH)\s+([A-Z][A-Z0-9-]*)/gi;

/**
 * Scans fixed-format COBOL text for PERFORM ... THRU/THROUGH patterns.
 * Returns pairs of (fromName, toName) in uppercase.
 */
export function findPerformThruRanges(
  text: string,
): Array<{ from: string; to: string }> {
  const lines = text.split(/\r?\n/);
  const ranges: Array<{ from: string; to: string }> = [];

  for (const line of lines) {
    if (!hasFixedColumns(line)) continue;
    const indicator = line[6];
    if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) continue;

    const lang = line.slice(7, Math.min(line.length, 72));
    PERFORM_THRU_RE.lastIndex = 0;
    let m: RegExpExecArray | null;
    while ((m = PERFORM_THRU_RE.exec(lang)) !== null) {
      ranges.push({ from: m[1].toUpperCase(), to: m[2].toUpperCase() });
    }
  }

  return ranges;
}

/**
 * Given a list of PERFORM THRU pairs and the ordered paragraph list,
 * returns the set of paragraph names that fall within any THRU range.
 */
function collectThruReferencedParagraphs(
  thruRanges: Array<{ from: string; to: string }>,
  paragraphs: ParagraphDef[],
): Set<string> {
  const referenced = new Set<string>();
  for (const { from, to } of thruRanges) {
    const fromIdx = paragraphs.findIndex(p => p.name === from);
    const toIdx = paragraphs.findIndex(p => p.name === to);
    if (fromIdx >= 0 && toIdx >= 0) {
      const startIdx = Math.min(fromIdx, toIdx);
      const endIdx = Math.max(fromIdx, toIdx);
      for (let i = startIdx; i <= endIdx; i++) {
        referenced.add(paragraphs[i].name);
      }
    }
  }
  return referenced;
}

/**
 * Find all unreferenced paragraphs and sections in the document.
 *
 * A paragraph/section is considered "dead" when it is never referenced
 * by any PERFORM, GO TO, or other statement outside its own definition line.
 *
 * Excludes:
 *  - The first paragraph in PROCEDURE DIVISION (entry point)
 *  - Paragraphs/sections whose name matches the PROGRAM-ID
 */
export function findDeadCode(
  doc: TextDocument,
  index?: DefinitionIndex,
): DeadCodeItem[] {
  const idx = index ?? buildDefinitionIndex(doc);
  const text = doc.getText();
  const dead: DeadCodeItem[] = [];

  // Determine entry paragraph (first paragraph — typically the program entry point)
  const entryParagraph = idx.paragraphs.length > 0 ? idx.paragraphs[0] : undefined;

  // Collect paragraph names referenced via PERFORM ... THRU/THROUGH ranges
  const thruRanges = findPerformThruRanges(text);
  const thruReferenced = collectThruReferencedParagraphs(thruRanges, idx.paragraphs);

  // Check each paragraph
  for (const p of idx.paragraphs) {
    // Skip the entry paragraph
    if (entryParagraph && p.line === entryParagraph.line) continue;

    // Skip paragraphs referenced via PERFORM THRU ranges
    if (thruReferenced.has(p.name)) continue;

    const occurrences = findAllOccurrences(text, p.name);
    // Filter out the definition site itself
    const references = occurrences.filter(occ => !isDefinitionSite(occ, idx));

    if (references.length === 0) {
      dead.push({
        kind: "paragraph",
        name: p.name,
        line: p.line,
        character: p.character,
        endCharacter: p.endCharacter,
      });
    }
  }

  // Check each section
  for (const s of idx.sections) {
    const occurrences = findAllOccurrences(text, s.name);
    // Filter out the definition site
    const references = occurrences.filter(occ => !isDefinitionSite(occ, idx));

    // Also filter out the "SECTION." keyword usage on the definition line
    // (buildDefinitionIndex already handles this, so isDefinitionSite catches it)

    if (references.length === 0) {
      dead.push({
        kind: "section",
        name: s.name,
        line: s.line,
        character: s.character,
        endCharacter: s.endCharacter,
      });
    }
  }

  return dead;
}

/**
 * Convert dead-code items into GenDiag diagnostics (hint severity).
 * These are meant to be mapped back via `mapGenRange` if run on preprocessed text,
 * or used directly on the source document.
 *
 * @param doc   The document to analyze (can be source or preprocessed)
 * @param index Optional pre-built definition index
 */
export function lintDeadCode(
  doc: TextDocument,
  index?: DefinitionIndex,
): GenDiag[] {
  const items = findDeadCode(doc, index);
  const diags: GenDiag[] = [];

  for (const item of items) {
    const startOff = doc.offsetAt({ line: item.line, character: item.character });
    const endOff = doc.offsetAt({ line: item.line, character: item.endCharacter });
    const kind = item.kind === "paragraph" ? "Paragraph" : "Section";

    diags.push({
      startOff,
      endOff,
      severity: DiagnosticSeverity.Hint,
      code: "DEAD_CODE",
      message: `${kind} '${item.name}' wird nirgends referenziert (Dead Code).`,
    });
  }

  return diags;
}
