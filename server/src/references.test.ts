/**
 * Tests for references.ts — Find All References functionality.
 */
import { describe, it, expect } from "vitest";
import { TextDocument } from "vscode-languageserver-textdocument";
import { Position } from "vscode-languageserver/node";
import {
  findAllOccurrences,
  isDefinitionSite,
  buildReferences,
  collectReferencesForSymbol,
} from "./references";
import { buildDefinitionIndex } from "./definition";

// ---- Helpers ----

function makeDoc(lines: string[]): TextDocument {
  return TextDocument.create("file:///test.cob", "cobol85", 1, lines.join("\n"));
}

function fixedLine(lang: string, indicator: string = " "): string {
  const seq = "000000";
  const content = lang.padEnd(65, " ");
  return seq + indicator + content;
}

// ---- findAllOccurrences ----

describe("findAllOccurrences", () => {
  it("finds all occurrences of a paragraph name", () => {
    const text = [
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM SUB-PARA"),
      fixedLine("    PERFORM SUB-PARA"),
      fixedLine("    STOP RUN."),
      fixedLine("SUB-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ].join("\n");

    const results = findAllOccurrences(text, "SUB-PARA");
    // line 4 PERFORM SUB-PARA, line 5 PERFORM SUB-PARA, line 7 SUB-PARA.
    expect(results.length).toBe(3);
  });

  it("is case-insensitive", () => {
    const text = [
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-COUNTER PIC 9(5)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("    MOVE 1 TO ws-counter."),
    ].join("\n");

    const results = findAllOccurrences(text, "WS-COUNTER");
    expect(results.length).toBe(2);
  });

  it("skips comment lines", () => {
    const text = [
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MY-PARA.", " "),
      fixedLine("MY-PARA is called here", "*"), // comment
      fixedLine("    PERFORM MY-PARA."),
    ].join("\n");

    const results = findAllOccurrences(text, "MY-PARA");
    expect(results.length).toBe(2); // definition + PERFORM, not the comment
  });

  it("returns empty for unknown symbol", () => {
    const text = [
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
    ].join("\n");

    const results = findAllOccurrences(text, "NONEXISTENT");
    expect(results.length).toBe(0);
  });
});

// ---- isDefinitionSite ----

describe("isDefinitionSite", () => {
  it("identifies paragraph definition", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
    ]);
    const index = buildDefinitionIndex(doc);
    const occs = findAllOccurrences(doc.getText(), "MAIN-PARA");
    expect(occs.length).toBe(1);
    expect(isDefinitionSite(occs[0], index)).toBe(true);
  });

  it("identifies non-definition usage", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM SUB-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("SUB-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);
    const index = buildDefinitionIndex(doc);
    const occs = findAllOccurrences(doc.getText(), "SUB-PARA");

    // First occurrence is in PERFORM (usage), second is definition
    const usageOcc = occs.find(o => o.line === 4);
    const defOcc = occs.find(o => o.line === 6);
    expect(usageOcc).toBeDefined();
    expect(defOcc).toBeDefined();
    expect(isDefinitionSite(usageOcc!, index)).toBe(false);
    expect(isDefinitionSite(defOcc!, index)).toBe(true);
  });
});

// ---- buildReferences ----

describe("buildReferences", () => {
  it("finds references to a paragraph including declaration", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM SUB-PARA"),
      fixedLine("    GO TO SUB-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("SUB-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);

    // Cursor on first PERFORM SUB-PARA (SUB-PARA at ~col 19)
    const refs = buildReferences(doc, Position.create(4, 19), true);
    expect(refs.length).toBe(3); // 2 usages + 1 definition
  });

  it("finds references excluding declaration", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM SUB-PARA"),
      fixedLine("    GO TO SUB-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("SUB-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);

    const refs = buildReferences(doc, Position.create(4, 19), false);
    expect(refs.length).toBe(2); // 2 usages only
  });

  it("finds references to a data item", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-COUNT PIC 9(5)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE 1 TO WS-COUNT"),
      fixedLine("    ADD 1 TO WS-COUNT"),
      fixedLine("    DISPLAY WS-COUNT."),
    ]);

    // Cursor on WS-COUNT in MOVE statement
    const refs = buildReferences(doc, Position.create(7, 21), true);
    expect(refs.length).toBe(4); // 1 definition + 3 usages
  });

  it("returns empty for unknown word", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
    ]);

    // Cursor on STOP (not a defined symbol)
    const refs = buildReferences(doc, Position.create(4, 11), true);
    expect(refs.length).toBe(0);
  });
});

// ---- collectReferencesForSymbol ----

describe("collectReferencesForSymbol", () => {
  it("collects usage-only references", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM SUB-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("SUB-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);
    const index = buildDefinitionIndex(doc);

    const refs = collectReferencesForSymbol(doc.getText(), "SUB-PARA", index, false);
    // Only the PERFORM usage, not the definition
    expect(refs.length).toBe(1);
    expect(refs[0].line).toBe(4);
  });

  it("collects including declaration", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM SUB-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("SUB-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);
    const index = buildDefinitionIndex(doc);

    const refs = collectReferencesForSymbol(doc.getText(), "SUB-PARA", index, true);
    expect(refs.length).toBe(2); // PERFORM + definition
  });
});
