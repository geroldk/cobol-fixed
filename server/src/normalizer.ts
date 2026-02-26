/**
 * Normalization functions for COBOL fixed-format text before parsing.
 * Handles: ID DIVISION alias, listing control statements, compiler directives,
 * parenthesized OR conditions, documentation paragraphs, and fixed-line layout.
 */
import { NormalizationAdjustment, NormalizedParserText } from "./types";
import { hasFixedColumns, isFixedCommentIndicator } from "./utils";

export function normalizeForCobol85Parser(text72: string): NormalizedParserText {
  const sourceLines = text72.split("\n");
  const normalizedLines = [...sourceLines];
  const adjustments: NormalizationAdjustment[] = [];

  // Dokumentationsparagraphen in IDENTIFICATION DIVISION.
  // Einige Altbestaende nutzen statt "." ein "," nach dem Schluesselwort.
  const docParas = /^(AUTHOR|DATE-COMPILED|DATE-WRITTEN|INSTALLATION|REMARKS|SECURITY)\s*[.,]/i;

  let sourceOff = 0;
  let normalizedOff = 0;
  let inIdentificationDivision = false;
  let inDocParagraph = false;

  for (let i = 0; i < sourceLines.length; i++) {
    const sourceLine = sourceLines[i];
    let line = sourceLine;
    const isFixed = hasFixedColumns(line);
    const langTrim = (isFixed ? line.slice(7) : line).trimStart();

    if (/^(IDENTIFICATION|ID)\s+DIVISION\b/i.test(langTrim)) {
      inIdentificationDivision = true;
      inDocParagraph = false;
    } else if (/^(ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b/i.test(langTrim)) {
      inIdentificationDivision = false;
      inDocParagraph = false;
    }

    if (isFixed && line.length >= 8 && inIdentificationDivision) {
      const startsDocParagraph = docParas.test(langTrim);
      if (startsDocParagraph) {
        inDocParagraph = true;
      }

      if (startsDocParagraph || (inDocParagraph && langTrim.length > 0)) {
        // Spalte 7 (Index 6) auf '*' setzen => Kommentarzeile.
        line = line.slice(0, 6) + "*" + line.slice(7);
      }
    }

    if (isListingControlStatement(langTrim) || isCompilerDirectiveStatement(langTrim)) {
      line = neutralizeListingControlLine(line, isFixed);
    }

    // Parser-Toleranz: "A = ('X' OR 'Y')" -> Klammern entfernen (Laenge bleibt identisch)
    // Einige Dialekte erlauben diese Form, tree-sitter-cobol stolpert darueber.
    line = normalizeParenthesizedOrCondition(line);

    // Dialekt-Alias fuer Parser-Normalisierung: ID DIVISION -> IDENTIFICATION DIVISION
    line = normalizeIdDivisionAliasForParser(line, isFixed, sourceOff, normalizedOff, adjustments);
    normalizedLines[i] = line;

    sourceOff += sourceLine.length;
    normalizedOff += line.length;
    if (i < sourceLines.length - 1) {
      sourceOff += 1;
      normalizedOff += 1;
    }
  }

  return { text: normalizedLines.join("\n"), adjustments };
}

export function normalizeParenthesizedOrCondition(line: string): string {
  return line.replace(/=\s*\([^()\n]*\bOR\b[^()\n]*\)/gi, (full) => {
    const open = full.indexOf("(");
    const close = full.lastIndexOf(")");
    if (open < 0 || close <= open) return full;
    return full.slice(0, open) + " " + full.slice(open + 1, close) + " ";
  });
}

export function isListingControlStatement(langTrim: string): boolean {
  if (/^(EJECT|SKIP(?:[123])?)\s*\.?\s*$/i.test(langTrim)) return true;
  if (/^TITLE(?:\s+['"][^'"\r\n]*['"])?\s*\.?\s*$/i.test(langTrim)) return true;
  return false;
}

export function isCompilerDirectiveStatement(langTrim: string): boolean {
  // Compiler options/directives are not executable COBOL syntax for the parser.
  return /^(CBL|PROCESS)\b/i.test(langTrim);
}

export function isCompilerDirectiveLine(full: string): boolean {
  // Tolerate both classic fixed-area placement and left-aligned variants.
  const leftTrimmed = full.trimStart();
  if (isCompilerDirectiveStatement(leftTrimmed) || isListingControlStatement(leftTrimmed)) return true;
  if (!hasFixedColumns(full)) return false;
  const fixedLangTrimmed = full.slice(7).trimStart();
  return isCompilerDirectiveStatement(fixedLangTrimmed) || isListingControlStatement(fixedLangTrimmed);
}

export function neutralizeListingControlLine(line: string, isFixed: boolean): string {
  if (isFixed && line.length >= 7) {
    const tail = line.length > 7 ? line.slice(7) : "";
    return line.slice(0, 6) + "*" + tail;
  }

  if (line.length >= 2) return "*>" + " ".repeat(line.length - 2);
  if (line.length === 1) return "*";
  return "";
}

export function normalizeIdDivisionAliasForParser(
  line: string,
  isFixed: boolean,
  sourceLineStart: number,
  normalizedLineStart: number,
  adjustments: NormalizationAdjustment[]
): string {
  if (isFixed) {
    if (line.length < 7) return line;
    const indicator = line[6];
    if (isFixedCommentIndicator(indicator)) return line;

    const prefix = line.slice(0, 7);
    const lang = line.slice(7);
    const normalizedLang = rewriteIdDivisionAliasSegment(
      lang,
      sourceLineStart + 7,
      normalizedLineStart + 7,
      adjustments
    );
    return prefix + normalizedLang;
  }

  if (/^\s*\*>/.test(line)) return line;
  return rewriteIdDivisionAliasSegment(line, sourceLineStart, normalizedLineStart, adjustments);
}

export function rewriteIdDivisionAliasSegment(
  segment: string,
  sourceBase: number,
  normalizedBase: number,
  adjustments: NormalizationAdjustment[]
): string {
  const re = /\bID\s+DIVISION\b/gi;
  let out = "";
  let sourceCursor = 0;
  let normalizedCursor = 0;

  while (true) {
    const m = re.exec(segment);
    if (!m) break;

    const idx = m.index ?? 0;
    const matched = m[0];
    const replacement = "IDENTIFICATION DIVISION";

    out += segment.slice(sourceCursor, idx);
    normalizedCursor += idx - sourceCursor;

    const sourceStart = sourceBase + idx;
    const sourceEnd = sourceStart + matched.length;
    const normalizedStart = normalizedBase + normalizedCursor;
    const normalizedEnd = normalizedStart + replacement.length;

    adjustments.push({ sourceStart, sourceEnd, normalizedStart, normalizedEnd });

    out += replacement;
    normalizedCursor += replacement.length;
    sourceCursor = idx + matched.length;
  }

  if (sourceCursor === 0) return segment;
  out += segment.slice(sourceCursor);
  return out;
}

export function mapNormalizedOffsetToSource(
  normalizedOff: number,
  adjustments: NormalizationAdjustment[],
  sourceLength: number
): number {
  if (adjustments.length === 0) return Math.max(0, Math.min(normalizedOff, sourceLength));

  let delta = 0;
  for (const a of adjustments) {
    if (normalizedOff < a.normalizedStart) break;

    if (normalizedOff <= a.normalizedEnd) {
      const rel = normalizedOff - a.normalizedStart;
      const sourceLen = a.sourceEnd - a.sourceStart;
      return a.sourceStart + Math.min(rel, sourceLen);
    }

    delta += (a.normalizedEnd - a.normalizedStart) - (a.sourceEnd - a.sourceStart);
  }

  return Math.max(0, Math.min(normalizedOff - delta, sourceLength));
}

export function normalizeFixedLineForParser(line72: string, indicator: string, isComment: boolean): string {
  if (line72.length < 7) return line72;

  // Spalten 1-6 sind im Fixed-Format Sequenzbereich und duerfen den Parser nicht beeinflussen.
  const prefix = " ".repeat(6);
  const languageArea = line72.slice(7);
  if (isComment) {
    return prefix + " " + " ".repeat(Math.max(0, line72.length - 7));
  }

  // Bei fortgesetzten String-Literalen muss '-' in Spalte 7 erhalten bleiben,
  // sonst erkennt der externe tree-sitter Scanner keine mehrzeiligen Literale.
  if (indicator === "-") {
    const continued = languageArea.trimStart();
    if (!continued.startsWith("\"") && !continued.startsWith("'")) {
      return prefix + " " + languageArea;
    }
  }

  const normalizedIndicator = line72[6];

  return prefix + normalizedIndicator + languageArea;
}
