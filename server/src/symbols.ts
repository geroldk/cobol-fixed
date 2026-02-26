/**
 * Document symbol / outline provider for COBOL fixed-format.
 */
import { DocumentSymbol, Range, SymbolKind } from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import {
  hasFixedColumns,
  isValidFixedIndicator,
  isFixedCommentIndicator,
} from "./utils";

export function buildDocumentSymbols(doc: TextDocument): DocumentSymbol[] {
  const text = doc.getText();
  const lines = text.split(/\r?\n/);
  const totalLines = lines.length;

  const root: DocumentSymbol[] = [];
  let currentProgram: DocumentSymbol | undefined;
  let currentDivision: DocumentSymbol | undefined;
  let currentSection: DocumentSymbol | undefined;

  for (let lineNo = 0; lineNo < lines.length; lineNo++) {
    const full = lines[lineNo];

    if (!hasFixedColumns(full)) continue;
    const indicator = full[6];
    if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) continue;

    const langStart = 7;
    const langEnd = Math.min(full.length, 72);
    const lang = full.slice(langStart, langEnd);

    {
      const m = /PROGRAM-ID\.\s*([A-Z0-9-]+)\s*\./i.exec(lang);
      if (m) {
        const sym = makeSymbol(
          `PROGRAM-ID ${m[1]}`,
          SymbolKind.Module,
          lineNo,
          langStart + (m.index ?? 0),
          lineNo,
          langStart + (m.index ?? 0) + m[0].length
        );
        root.push(sym);
        currentProgram = sym;
        currentDivision = undefined;
        currentSection = undefined;
        continue;
      }
    }

    {
      const m = /^\s*(IDENTIFICATION|ID|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b.*$/i.exec(lang);
      if (m) {
        const divisionName = m[1].toUpperCase() === "ID" ? "IDENTIFICATION" : m[1].toUpperCase();
        const label = `${divisionName} DIVISION`;
        const sym = makeSymbol(
          label,
          SymbolKind.Namespace,
          lineNo,
          langStart + (m.index ?? 0),
          lineNo,
          langStart + (m.index ?? 0) + m[0].length
        );
        attachSymbol(root, currentProgram, sym);
        currentDivision = sym;
        currentSection = undefined;
        continue;
      }
    }

    {
      const m = /^\s*([A-Z0-9-]+)\s+SECTION\.\s*$/i.exec(lang);
      if (m) {
        const sym = makeSymbol(
          `${m[1].toUpperCase()} SECTION`,
          SymbolKind.Namespace,
          lineNo,
          langStart + (m.index ?? 0),
          lineNo,
          langStart + (m.index ?? 0) + m[0].length
        );
        attachSymbol(root, currentDivision ?? currentProgram, sym);
        currentSection = sym;
        continue;
      }
    }

    {
      const m = /^\s*([A-Z][A-Z0-9-]*)\.\s*$/i.exec(lang);
      if (m) {
        const name = m[1].toUpperCase();
        if (!["END-IF", "END-EVALUATE"].includes(name)) {
          const sym = makeSymbol(
            `${name}.`,
            SymbolKind.Function,
            lineNo,
            langStart + (m.index ?? 0),
            lineNo,
            langStart + (m.index ?? 0) + m[0].length
          );
          attachSymbol(root, currentSection ?? currentDivision ?? currentProgram, sym);
        }
      }
    }
  }

  // Fix ranges so that each symbol's range covers all its children
  // and extends to the start of the next sibling symbol.
  fixSymbolRanges(root, totalLines);

  return root;
}

function attachSymbol(root: DocumentSymbol[], parent: DocumentSymbol | undefined, child: DocumentSymbol) {
  if (parent) parent.children = [...(parent.children ?? []), child];
  else root.push(child);
}

/**
 * Post-processes the symbol tree so that each symbol's `range` extends from
 * its definition line to just before the next sibling symbol (or to the
 * provided `endLine` for the last sibling). This makes folding and outline
 * highlighting work correctly per LSP specification.
 *
 * `selectionRange` remains the original label-only range.
 */
function fixSymbolRanges(symbols: DocumentSymbol[], endLine: number): void {
  for (let i = 0; i < symbols.length; i++) {
    const sym = symbols[i];
    const nextStartLine = i + 1 < symbols.length
      ? symbols[i + 1].range.start.line
      : endLine;

    // Extend range from symbol start to just before next sibling
    sym.range = Range.create(
      sym.range.start.line,
      0,
      Math.max(sym.range.start.line, nextStartLine - 1),
      99999,
    );

    // Recursively fix children, bounded by this symbol's end
    if (sym.children && sym.children.length > 0) {
      fixSymbolRanges(sym.children, nextStartLine);
    }
  }
}

function makeSymbol(
  name: string,
  kind: SymbolKind,
  startLine: number,
  startChar: number,
  endLine: number,
  endChar: number
): DocumentSymbol {
  const r = Range.create(startLine, startChar, endLine, endChar);
  return { name, kind, range: r, selectionRange: r, children: [] };
}
