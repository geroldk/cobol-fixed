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

  // 2) Symbol hover: paragraph, section, data item
  const index = preIndex ?? buildDefinitionIndex(doc);
  const symbolHover = buildSymbolHover(word, index, wordRange);
  if (symbolHover) return symbolHover;

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
): Hover | undefined {
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

  // Data items — could have multiple definitions (same name at different levels).
  // Show all of them.
  const matches = index.dataItems.filter((d: DataItemDef) => d.name === word);
  if (matches.length > 0) {
    const parts = matches.map((d: DataItemDef) => {
      let desc = `**Level ${String(d.level).padStart(2, "0")}** \`${d.name}\``;
      if (d.pic) desc += `  PIC ${d.pic}`;
      if (d.usage) desc += `  USAGE ${d.usage}`;
      desc += `  _(Zeile ${d.line + 1})_`;
      return desc;
    });
    const content: MarkupContent = {
      kind: MarkupKind.Markdown,
      value: parts.join("\n\n"),
    };
    return { contents: content, range: wordRange };
  }

  return undefined;
}
