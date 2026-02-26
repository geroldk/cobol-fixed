/**
 * Tests for symbols.ts — Document symbols / outline provider.
 */
import { describe, it, expect } from "vitest";
import { TextDocument } from "vscode-languageserver-textdocument";
import { SymbolKind } from "vscode-languageserver/node";
import { buildDocumentSymbols } from "./symbols";

// ---- Helpers ----

function makeDoc(lines: string[]): TextDocument {
  return TextDocument.create("file:///test.cob", "cobol85", 1, lines.join("\n"));
}

function fixedLine(lang: string, indicator: string = " "): string {
  const seq = "000000";
  const content = lang.padEnd(65, " ");
  return seq + indicator + content;
}

// ---- buildDocumentSymbols ----

describe("buildDocumentSymbols", () => {
  it("returns PROGRAM-ID symbol", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
    ]);

    const symbols = buildDocumentSymbols(doc);
    const programSym = symbols.find(s => s.name.includes("TEST1"));
    expect(programSym).toBeDefined();
    expect(programSym!.kind).toBe(SymbolKind.Module);
  });

  it("returns DIVISION symbols as children of PROGRAM-ID", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-X PIC X."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
    ]);

    const symbols = buildDocumentSymbols(doc);
    const programSym = symbols.find(s => s.name.includes("TEST1"));
    expect(programSym).toBeDefined();

    // IDENTIFICATION DIVISION appears before PROGRAM-ID, so it is a root symbol
    const idDiv = symbols.find(s => s.name === "IDENTIFICATION DIVISION");
    expect(idDiv).toBeDefined();

    // DATA and PROCEDURE are after PROGRAM-ID, so they are children
    const divNames = (programSym!.children ?? []).map(c => c.name);
    expect(divNames).toContain("DATA DIVISION");
    expect(divNames).toContain("PROCEDURE DIVISION");
  });

  it("returns SECTION symbols as children of DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-SECTION SECTION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
    ]);

    const symbols = buildDocumentSymbols(doc);
    const programSym = symbols.find(s => s.name.includes("TEST1"));
    const procDiv = (programSym!.children ?? []).find(c => c.name === "PROCEDURE DIVISION");
    expect(procDiv).toBeDefined();

    const sectionNames = (procDiv!.children ?? []).map(c => c.name);
    expect(sectionNames).toContain("MAIN-SECTION SECTION");
  });

  it("returns paragraph symbols as children of SECTION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-SECTION SECTION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("SUB-PARA."),
      fixedLine("    DISPLAY 'HI'."),
    ]);

    const symbols = buildDocumentSymbols(doc);
    const programSym = symbols.find(s => s.name.includes("TEST1"));
    const procDiv = (programSym!.children ?? []).find(c => c.name === "PROCEDURE DIVISION");
    const mainSection = (procDiv!.children ?? []).find(c => c.name.includes("MAIN-SECTION"));
    expect(mainSection).toBeDefined();

    const paraNames = (mainSection!.children ?? []).map(c => c.name);
    expect(paraNames).toContain("MAIN-PARA.");
    expect(paraNames).toContain("SUB-PARA.");
  });

  it("does not include END-IF or END-EVALUATE as paragraphs", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    IF 1 = 1"),
      fixedLine("      DISPLAY 'YES'"),
      fixedLine("    END-IF."),
      fixedLine("    STOP RUN."),
    ]);

    const symbols = buildDocumentSymbols(doc);
    const allNames = flattenSymbolNames(symbols);
    expect(allNames).not.toContain("END-IF.");
    expect(allNames).toContain("MAIN-PARA.");
  });

  it("handles ID DIVISION alias", () => {
    const doc = makeDoc([
      fixedLine("ID DIVISION."),
      fixedLine("PROGRAM-ID. MYPROG."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("ENTRY-PARA."),
      fixedLine("    STOP RUN."),
    ]);

    const symbols = buildDocumentSymbols(doc);
    const programSym = symbols.find(s => s.name.includes("MYPROG"));
    expect(programSym).toBeDefined();

    // ID DIVISION is normalized to IDENTIFICATION DIVISION and appears
    // before PROGRAM-ID, so it is a root-level symbol
    const idDiv = symbols.find(s => s.name === "IDENTIFICATION DIVISION");
    expect(idDiv).toBeDefined();
  });

  it("returns empty for blank document", () => {
    const doc = makeDoc([""]);
    const symbols = buildDocumentSymbols(doc);
    expect(symbols).toEqual([]);
  });

  it("symbol ranges cover children (not just the definition line)", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY 'A'."),
      fixedLine("    STOP RUN."),
      fixedLine("OTHER-PARA."),
      fixedLine("    DISPLAY 'B'."),
    ]);

    const symbols = buildDocumentSymbols(doc);
    const programSym = symbols.find(s => s.name.includes("TEST1"));
    expect(programSym).toBeDefined();
    // PROGRAM-ID range should extend beyond its definition line
    expect(programSym!.range.end.line).toBeGreaterThan(programSym!.range.start.line);

    const procDiv = (programSym!.children ?? []).find(c => c.name === "PROCEDURE DIVISION");
    expect(procDiv).toBeDefined();
    // PROCEDURE DIVISION range should cover all its paragraphs
    expect(procDiv!.range.end.line).toBeGreaterThan(procDiv!.range.start.line);

    // Paragraphs should have ranges covering their body
    const paras = procDiv!.children ?? [];
    expect(paras.length).toBe(2);
    const mainPara = paras.find(p => p.name === "MAIN-PARA.");
    const otherPara = paras.find(p => p.name === "OTHER-PARA.");
    expect(mainPara).toBeDefined();
    expect(otherPara).toBeDefined();

    // MAIN-PARA range should end before OTHER-PARA starts
    expect(mainPara!.range.end.line).toBeLessThan(otherPara!.range.start.line);
  });

  it("selectionRange is the label, range covers the body", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
      fixedLine("    STOP RUN."),
    ]);

    const symbols = buildDocumentSymbols(doc);
    const programSym = symbols.find(s => s.name.includes("TEST1"));
    expect(programSym).toBeDefined();

    // selectionRange should be the narrow label range
    expect(programSym!.selectionRange.start.line).toBe(programSym!.selectionRange.end.line);
    // range should be wider than selectionRange
    expect(programSym!.range.end.line).toBeGreaterThanOrEqual(programSym!.selectionRange.end.line);
  });

  it("handles multiple sections with their own paragraphs", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("SEC-A SECTION."),
      fixedLine("PARA-A1."),
      fixedLine("    DISPLAY 'A1'."),
      fixedLine("SEC-B SECTION."),
      fixedLine("PARA-B1."),
      fixedLine("    DISPLAY 'B1'."),
      fixedLine("PARA-B2."),
      fixedLine("    DISPLAY 'B2'."),
    ]);

    const symbols = buildDocumentSymbols(doc);
    const programSym = symbols.find(s => s.name.includes("TEST1"));
    const procDiv = (programSym!.children ?? []).find(c => c.name === "PROCEDURE DIVISION");
    expect(procDiv).toBeDefined();

    const sections = procDiv!.children ?? [];
    expect(sections.length).toBe(2);

    const secA = sections.find(s => s.name.includes("SEC-A"));
    const secB = sections.find(s => s.name.includes("SEC-B"));
    expect(secA).toBeDefined();
    expect(secB).toBeDefined();

    expect((secA!.children ?? []).length).toBe(1); // PARA-A1
    expect((secB!.children ?? []).length).toBe(2); // PARA-B1, PARA-B2
  });
});

// ---- Helper to flatten all symbol names ----

function flattenSymbolNames(symbols: { name: string; children?: any[] }[]): string[] {
  const result: string[] = [];
  for (const s of symbols) {
    result.push(s.name);
    if (s.children) result.push(...flattenSymbolNames(s.children));
  }
  return result;
}
