/**
 * Tests for rename.ts — Rename provider.
 */
import { describe, it, expect } from "vitest";
import { TextDocument } from "vscode-languageserver-textdocument";
import { Position } from "vscode-languageserver/node";
import { isValidCobolIdentifier, prepareRename, performRename } from "./rename";
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

// ---- isValidCobolIdentifier ----

describe("isValidCobolIdentifier", () => {
  it("accepts valid identifiers", () => {
    expect(isValidCobolIdentifier("WS-COUNTER")).toBe(true);
    expect(isValidCobolIdentifier("A")).toBe(true);
    expect(isValidCobolIdentifier("MAIN-PARA")).toBe(true);
    expect(isValidCobolIdentifier("X1")).toBe(true);
  });

  it("rejects invalid identifiers", () => {
    expect(isValidCobolIdentifier("")).toBe(false);
    expect(isValidCobolIdentifier("1ABC")).toBe(false);
    expect(isValidCobolIdentifier("ABC-")).toBe(false);
    expect(isValidCobolIdentifier("A".repeat(31))).toBe(false);
  });
});

// ---- prepareRename ----

describe("prepareRename", () => {
  it("allows rename for a paragraph name", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM MAIN-PARA."),
    ]);
    const index = buildDefinitionIndex(doc);

    // Cursor on MAIN-PARA definition
    const result = prepareRename(doc, Position.create(3, 9), index);
    expect(result).toBeDefined();
    expect(result!.placeholder).toBe("MAIN-PARA");
  });

  it("allows rename for a data item", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-COUNT PIC 9(5)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE 1 TO WS-COUNT."),
    ]);
    const index = buildDefinitionIndex(doc);

    // Cursor on WS-COUNT usage
    const result = prepareRename(doc, Position.create(7, 21), index);
    expect(result).toBeDefined();
    expect(result!.placeholder).toBe("WS-COUNT");
  });

  it("rejects rename on keyword", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);
    const index = buildDefinitionIndex(doc);

    // Cursor on DISPLAY (not a user symbol)
    const result = prepareRename(doc, Position.create(4, 11), index);
    expect(result).toBeUndefined();
  });

  it("rejects rename on comment line", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("This is a comment MAIN-PARA", "*"),
    ]);
    const index = buildDefinitionIndex(doc);

    const result = prepareRename(doc, Position.create(2, 20), index);
    expect(result).toBeUndefined();
  });
});

// ---- performRename ----

describe("performRename", () => {
  it("renames a paragraph everywhere", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM SUB-PARA."),
      fixedLine("    GO TO SUB-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("SUB-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);

    const result = performRename(doc, Position.create(4, 19), "NEW-PARA");
    expect(result).toBeDefined();
    expect(result!.changes).toBeDefined();

    const edits = result!.changes!["file:///test.cob"];
    expect(edits).toBeDefined();
    expect(edits.length).toBe(3); // 2 usages + 1 definition
    // All edits should use the uppercase form
    for (const edit of edits) {
      expect(edit.newText).toBe("NEW-PARA");
    }
  });

  it("renames a data item everywhere", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-OLD PIC 9(5)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE 1 TO WS-OLD"),
      fixedLine("    DISPLAY WS-OLD."),
    ]);

    // Cursor on WS-OLD in "MOVE 1 TO WS-OLD" — col 7+4+9 = 20
    const result = performRename(doc, Position.create(7, 21), "WS-NEW");
    expect(result).toBeDefined();
    const edits = result!.changes!["file:///test.cob"];
    expect(edits.length).toBe(3); // 1 definition + 2 usages
  });

  it("rejects invalid new name", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
    ]);

    const result = performRename(doc, Position.create(3, 9), "123-INVALID");
    expect(result).toBeUndefined();
  });

  it("returns undefined for unknown symbol", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);

    // Cursor on DISPLAY
    const result = performRename(doc, Position.create(4, 11), "NEW-NAME");
    expect(result).toBeUndefined();
  });
});
