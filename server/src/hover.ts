/**
 * Hover provider for COBOL fixed-format.
 *
 * Provides hover information for:
 *  - Paragraphs / Sections  → name and location
 *  - Data items             → level, PIC, USAGE
 *  - COPY book names        → resolved file path
 */
import { Hover, MarkupContent, MarkupKind, Position, Range } from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import {
  hasFixedColumns,
  isFixedCommentIndicator,
  isValidFixedIndicator,
  resolveCopybook,
} from "./utils";

import {
  wordAtPosition,
  buildDefinitionIndex,
  DefinitionIndex,
  DataItemDef,
  ParagraphDef,
  SectionDef,
  getQualifiers,
  findSymbolInIndex,
} from "./definition";

/**
 * Builds hover information for the word at the given position.
 */
export function buildHover(
  doc: TextDocument,
  position: Position,
  baseDirs: string[],
  preIndex?: DefinitionIndex,
): Hover | undefined {
  const lineText = doc.getText(Range.create(position.line, 0, position.line + 1, 0)).replace(/\r?\n$/, "");

  if (!hasFixedColumns(lineText)) return undefined;
  const indicator = lineText[6];
  if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) return undefined;

  const wordInfo = wordAtPosition(lineText, position.character);
  if (!wordInfo) return undefined;

  const word = wordInfo.word;
  const wordRange = Range.create(position.line, wordInfo.start, position.line, wordInfo.end);

  // 1) COPY book hover: show resolved path
  const copyHover = buildCopybookHover(lineText, word, wordInfo.start, baseDirs, wordRange);
  if (copyHover) return copyHover;

  // 1.5) EXEC CICS context: suppress hover for CICS keywords outside parens
  if (isExecCicsKeyword(doc, position, wordInfo)) return undefined;

  // 1.6) EXEC DLI context: suppress hover for DLI keywords outside parens
  if (isExecDliKeyword(doc, position, wordInfo)) return undefined;

  // 2) Symbol hover: paragraph, section, data item
  const index = preIndex ?? buildDefinitionIndex(doc);
  const qualifiers = getQualifiers(doc, position.line, wordInfo.end);
  const symbolHover = buildSymbolHover(word, index, wordRange, qualifiers);
  if (symbolHover) return symbolHover;

  // 3) EIB / DIB system field hover (fallback when not defined in source)
  const sysHover = buildSystemFieldHover(word, wordRange);
  if (sysHover) return sysHover;

  return undefined;
}

function buildCopybookHover(
  lineText: string,
  word: string,
  wordStart: number,
  baseDirs: string[],
  wordRange: Range,
): Hover | undefined {
  const lang = lineText.slice(7, Math.min(lineText.length, 72));
  if (!/\bCOPY\b/i.test(lang)) return undefined;
  if (word === "COPY" || word === "REPLACING") return undefined;

  const copyIdx = lang.search(/\bCOPY\b/i);
  if (copyIdx < 0) return undefined;
  const copyEnd = 7 + copyIdx + 4;
  if (wordStart < copyEnd) return undefined;

  const resolved = resolveCopybook(word, baseDirs);
  if (!resolved) {
    const content: MarkupContent = {
      kind: MarkupKind.Markdown,
      value: `**COPY Book:** \`${word}\`\n\n_Nicht gefunden_`,
    };
    return { contents: content, range: wordRange };
  }

  const content: MarkupContent = {
    kind: MarkupKind.Markdown,
    value: `**COPY Book:** \`${word}\`\n\n**Pfad:** \`${resolved}\``,
  };
  return { contents: content, range: wordRange };
}

function buildSymbolHover(
  word: string,
  index: ReturnType<typeof buildDefinitionIndex>,
  wordRange: Range,
  qualifiers: string[] = [],
): Hover | undefined {
  const resolved = findSymbolInIndex(word, index, qualifiers);
  
  if (!resolved) return undefined;

  if ("level" in resolved) {
    const d = resolved as DataItemDef;
    let desc = `**Level ${String(d.level).padStart(2, "0")}** \`${d.name}\``;
    if (d.pic) desc += `  PIC ${d.pic}`;
    if (d.usage) desc += `  USAGE ${d.usage}`;
    desc += `  _(Zeile ${d.line + 1})_`;
    
    if (d.parentNames && d.parentNames.length > 0) {
       desc += `\n\n_Qualifiers: ${d.parentNames.slice().reverse().join(' IN ')}_`;
    }

    const content: MarkupContent = {
      kind: MarkupKind.Markdown,
      value: desc,
    };
    return { contents: content, range: wordRange };
  }

  // Paragraph
  const para = index.paragraphs.find((p: ParagraphDef) => p.name === word);
  if (para) {
    const content: MarkupContent = {
      kind: MarkupKind.Markdown,
      value: `**Paragraph:** \`${para.name}\`\n\n_Definiert in Zeile ${para.line + 1}_`,
    };
    return { contents: content, range: wordRange };
  }

  // Section
  const sec = index.sections.find((s: SectionDef) => s.name === word);
  if (sec) {
    const content: MarkupContent = {
      kind: MarkupKind.Markdown,
      value: `**Section:** \`${sec.name}\`\n\n_Definiert in Zeile ${sec.line + 1}_`,
    };
    return { contents: content, range: wordRange };
  }

  return undefined;
}

// ---- EIB / DIB system field definitions ----

/** CICS Execute Interface Block (DFHEIBLK) fields. */
const EIB_FIELDS: Record<string, { pic: string; desc: string }> = {
  // --- Zeitbezogen ---
  "EIBTIME":    { pic: "S9(7) COMP-3",  desc: "Uhrzeit des Task-Starts (0HHMMSS)" },
  "EIBDATE":    { pic: "S9(7) COMP-3",  desc: "Datum des Task-Starts (0CYYDDD, C=Jahrhundert)" },
  // --- Transaktions- und Terminal-Identifikation ---
  "EIBTRNID":   { pic: "X(4)",          desc: "Transaktions-ID der aktuellen Task" },
  "EIBTASKN":   { pic: "S9(7) COMP-3",  desc: "Task-Nummer" },
  "EIBTRMID":   { pic: "X(4)",          desc: "Terminal-ID (Principal Facility)" },
  // --- Dateioperationen ---
  "EIBRSRCE":   { pic: "X(8)",          desc: "Zuletzt angesprochene Ressource (Datei, Queue, Programm)" },
  "EIBDS":      { pic: "X(8)",          desc: "Zuletzt angesprochener Dateiname" },
  "EIBREQID":   { pic: "X(8)",          desc: "Request-Identifier (Interval Control)" },
  // --- Cursorposition ---
  "EIBCPOSN":   { pic: "S9(4) COMP",    desc: "Cursor-Position auf dem Bildschirm" },
  // --- COMMAREA ---
  "EIBCALEN":   { pic: "S9(4) COMP",    desc: "Laenge der empfangenen COMMAREA (0 = kein XCTL/LINK/START)" },
  // --- Attention Identifier ---
  "EIBAID":     { pic: "X(1)",          desc: "Attention-ID (gedrueckte Taste: ENTER, PF1-24, PA1-3, CLEAR)" },
  // --- Funktionscodes ---
  "EIBFN":      { pic: "X(2)",          desc: "Funktionscode des letzten CICS-Befehls" },
  // --- Response ---
  "EIBRCODE":   { pic: "X(6)",          desc: "Response-Code des letzten CICS-Befehls" },
  "EIBRESP":    { pic: "S9(8) COMP",    desc: "RESP-Wert (numerisch, z.B. 0=NORMAL, 13=NOTFND)" },
  "EIBRESP2":   { pic: "S9(8) COMP",    desc: "RESP2-Detailcode" },
  // --- Flags ---
  "EIBATT":     { pic: "X(1)",          desc: "Attach-Header-Flag (LUTYPE6.1)" },
  "EIBEOC":     { pic: "X(1)",          desc: "End-of-Chain-Flag" },
  "EIBFMH":     { pic: "X(1)",          desc: "FMH-Flag (Function Management Header empfangen)" },
  "EIBCOMPL":   { pic: "X(1)",          desc: "Complete-Flag (Datenempfang vollstaendig)" },
  "EIBSIG":     { pic: "X(1)",          desc: "SIGNAL empfangen" },
  "EIBCONF":    { pic: "X(1)",          desc: "Confirmation-Request empfangen" },
  "EIBERR":     { pic: "X(1)",          desc: "Error empfangen" },
  "EIBSYNC":    { pic: "X(1)",          desc: "Syncpoint-Request vom Partner" },
  "EIBFREE":    { pic: "X(1)",          desc: "Free empfangen (Session freigeben)" },
  "EIBRECV":    { pic: "X(1)",          desc: "Receive-Richtung aktiv" },
  "EIBNODAT":   { pic: "X(1)",          desc: "No-Data-Flag (RECEIVE ohne Daten)" },
  "EIBERRCD":   { pic: "X(4)",          desc: "Error-Code bei EIBERR" },
  "EIBRLDBK":   { pic: "X(1)",          desc: "Rolled-Back-Flag (Syncpoint-Rollback erfolgt)" },
  "EIBSYNRB":   { pic: "X(1)",          desc: "Syncpoint-Rollback angefordert" },
  // --- Spezialfelder ---
  "EIBSTRT":    { pic: "X(1)",          desc: "Task wurde per START gestartet (nicht Terminal-Eingabe)" },
};

/** DL/I Interface Block (DIB) fields — set by EXEC DLI calls. */
const DIB_FIELDS: Record<string, { pic: string; desc: string }> = {
  "DIBSTAT":    { pic: "X(2)",          desc: "DL/I-Statuscode ('  '=OK, 'GE'=Segment not found, 'GB'=End of DB, 'II'=Insert-Fehler, 'AI'=Integrity-Fehler)" },
  "DIBSEGM":   { pic: "X(8)",          desc: "Name des zuletzt verarbeiteten Segments" },
  "DIBSEGL":   { pic: "S9(4) COMP",    desc: "Laenge des zuletzt verarbeiteten Segments" },
  "DIBKFBL":   { pic: "S9(4) COMP",    desc: "Key-Feedback-Bereich-Laenge" },
  "DIBDBORG":  { pic: "X(1)",          desc: "Datenbankorganisation (H=HDAM, I=HIDAM, S=HSAM, G=GSAM, X=INDEX)" },
};

/**
 * Build hover for EIB/DIB system fields.
 * Returns hover with PIC and description if word is a known system field.
 */
function buildSystemFieldHover(
  word: string,
  wordRange: Range,
): Hover | undefined {
  const upper = word.toUpperCase();

  const eib = EIB_FIELDS[upper];
  if (eib) {
    const content: MarkupContent = {
      kind: MarkupKind.Markdown,
      value: `**EIB-Feld:** \`${upper}\`  PIC ${eib.pic}\n\n${eib.desc}`,
    };
    return { contents: content, range: wordRange };
  }

  const dib = DIB_FIELDS[upper];
  if (dib) {
    const content: MarkupContent = {
      kind: MarkupKind.Markdown,
      value: `**DIB-Feld:** \`${upper}\`  PIC ${dib.pic}\n\n${dib.desc}`,
    };
    return { contents: content, range: wordRange };
  }

  return undefined;
}

// ---- EXEC CICS context detection ----

const EXEC_CICS_RE = /\bEXEC\s+CICS\b/i;
const END_EXEC_RE  = /\bEND-EXEC\b/i;

/**
 * Determines whether the hovered word is a CICS keyword (verb, option, or
 * condition) that appears *outside* parentheses inside an EXEC CICS block.
 * Words inside parentheses are data references and should still get hover.
 *
 * Returns true to suppress the hover.
 */
function isExecCicsKeyword(
  doc: TextDocument,
  position: Position,
  wordInfo: { word: string; start: number; end: number },
): boolean {
  // ---- 1) Determine if position is inside an EXEC CICS block ----
  // Scan backwards from the current line looking for EXEC CICS or END-EXEC.
  let insideExecCics = false;
  for (let i = position.line; i >= 0; i--) {
    const rawLine = doc.getText(Range.create(i, 0, i + 1, 0)).replace(/\r?\n$/, "");
    if (!hasFixedColumns(rawLine)) continue;
    const ind = rawLine[6];
    if (isFixedCommentIndicator(ind)) continue;
    const lang = rawLine.slice(7, Math.min(rawLine.length, 72));

    if (i === position.line) {
      // On the hover line itself, check if EXEC CICS appears *before* our word
      const beforeWord = lang.slice(0, Math.max(0, wordInfo.start - 7));
      if (EXEC_CICS_RE.test(beforeWord)) {
        insideExecCics = true;
        break;
      }
      // Also check if END-EXEC comes before our word on this line
      if (END_EXEC_RE.test(beforeWord)) return false; // we're after END-EXEC
    } else {
      // On a prior line, the most recent EXEC CICS or END-EXEC wins
      if (END_EXEC_RE.test(lang)) return false; // block closed before us
      if (EXEC_CICS_RE.test(lang)) {
        insideExecCics = true;
        break;
      }
    }
  }

  if (!insideExecCics) return false;

  // ---- 2) Determine paren depth at our position ----
  // Walk from the EXEC CICS line to our position tracking paren depth.
  // If depth > 0 at our word → it's a data reference → allow hover.
  // If depth == 0 → it's a CICS keyword → suppress hover.
  let parenDepth = 0;
  // Find the EXEC CICS line
  let execLine = position.line;
  for (let i = position.line; i >= 0; i--) {
    const rawLine = doc.getText(Range.create(i, 0, i + 1, 0)).replace(/\r?\n$/, "");
    if (!hasFixedColumns(rawLine)) continue;
    const lang = rawLine.slice(7, Math.min(rawLine.length, 72));
    if (EXEC_CICS_RE.test(lang)) {
      execLine = i;
      break;
    }
  }

  // Walk from execLine to the hover position, tracking parens
  for (let i = execLine; i <= position.line; i++) {
    const rawLine = doc.getText(Range.create(i, 0, i + 1, 0)).replace(/\r?\n$/, "");
    if (!hasFixedColumns(rawLine)) continue;
    const ind = rawLine[6];
    if (isFixedCommentIndicator(ind)) continue;

    const langStart = 7;
    const langEnd = Math.min(rawLine.length, 72);

    // On hover line, only scan up to the word start
    const stopCol = (i === position.line) ? wordInfo.start : langEnd;

    for (let c = langStart; c < stopCol; c++) {
      if (rawLine[c] === "(") parenDepth++;
      else if (rawLine[c] === ")") parenDepth = Math.max(0, parenDepth - 1);
    }
  }

  // Inside parens → data reference → don't suppress
  if (parenDepth > 0) return false;

  // Outside parens → it's a CICS keyword → suppress hover
  return true;
}

// ---- EXEC DLI context detection ----

const EXEC_DLI_RE = /\bEXEC\s+DLI\b/i;

/**
 * Determines whether the hovered word is a DLI keyword (verb, clause, or
 * long-form verb word) that appears *outside* parentheses inside an EXEC DLI
 * block.  Words inside parentheses are data references and should still get
 * hover.
 *
 * Returns true to suppress the hover.
 */
function isExecDliKeyword(
  doc: TextDocument,
  position: Position,
  wordInfo: { word: string; start: number; end: number },
): boolean {
  // ---- 1) Determine if position is inside an EXEC DLI block ----
  let insideExecDli = false;
  for (let i = position.line; i >= 0; i--) {
    const rawLine = doc.getText(Range.create(i, 0, i + 1, 0)).replace(/\r?\n$/, "");
    if (!hasFixedColumns(rawLine)) continue;
    const ind = rawLine[6];
    if (isFixedCommentIndicator(ind)) continue;
    const lang = rawLine.slice(7, Math.min(rawLine.length, 72));

    if (i === position.line) {
      const beforeWord = lang.slice(0, Math.max(0, wordInfo.start - 7));
      if (EXEC_DLI_RE.test(beforeWord)) {
        insideExecDli = true;
        break;
      }
      if (END_EXEC_RE.test(beforeWord)) return false;
    } else {
      if (END_EXEC_RE.test(lang)) return false;
      if (EXEC_DLI_RE.test(lang)) {
        insideExecDli = true;
        break;
      }
    }
  }

  if (!insideExecDli) return false;

  // ---- 2) Determine paren depth at our position ----
  let parenDepth = 0;
  let execLine = position.line;
  for (let i = position.line; i >= 0; i--) {
    const rawLine = doc.getText(Range.create(i, 0, i + 1, 0)).replace(/\r?\n$/, "");
    if (!hasFixedColumns(rawLine)) continue;
    const lang = rawLine.slice(7, Math.min(rawLine.length, 72));
    if (EXEC_DLI_RE.test(lang)) {
      execLine = i;
      break;
    }
  }

  for (let i = execLine; i <= position.line; i++) {
    const rawLine = doc.getText(Range.create(i, 0, i + 1, 0)).replace(/\r?\n$/, "");
    if (!hasFixedColumns(rawLine)) continue;
    const ind = rawLine[6];
    if (isFixedCommentIndicator(ind)) continue;

    const langStart = 7;
    const langEnd = Math.min(rawLine.length, 72);
    const stopCol = (i === position.line) ? wordInfo.start : langEnd;

    for (let c = langStart; c < stopCol; c++) {
      if (rawLine[c] === "(") parenDepth++;
      else if (rawLine[c] === ")") parenDepth = Math.max(0, parenDepth - 1);
    }
  }

  if (parenDepth > 0) return false;
  return true;
}