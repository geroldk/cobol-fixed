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

  it("rejects ambiguous unqualified data-name usage", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 REC-A."),
      fixedLine("   05 FIELD-1 PIC X."),
      fixedLine("01 REC-B."),
      fixedLine("   05 FIELD-1 PIC X."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE FIELD-1 TO X."),
    ]);
    const index = buildDefinitionIndex(doc);
    const line10 = doc.getText().split("\n")[10];
    const pos = line10.indexOf("FIELD-1");

    const result = prepareRename(doc, Position.create(10, pos + 1), index);
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

  it("renames only the qualified data-name branch", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 REC-A."),
      fixedLine("   05 FIELD-1 PIC X."),
      fixedLine("01 REC-B."),
      fixedLine("   05 FIELD-1 PIC X."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE FIELD-1 OF REC-A TO FIELD-1 OF REC-B."),
      fixedLine("    DISPLAY FIELD-1 OF REC-A."),
      fixedLine("    DISPLAY FIELD-1 OF REC-B."),
    ]);

    const line10 = doc.getText().split("\n")[10];
    const firstField = line10.indexOf("FIELD-1");
    const result = performRename(doc, Position.create(10, firstField + 1), "FIELD-A");

    expect(result).toBeDefined();
    const edits = result!.changes!["file:///test.cob"];
    const lines = edits.map((e) => e.range.start.line).sort((a, b) => a - b);
    expect(lines).toEqual([5, 10, 11]);
    expect(edits.every((e) => e.newText === "FIELD-A")).toBe(true);
  });

  it("uses exact definition site for duplicate data-name rename", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 REC-A."),
      fixedLine("   05 FIELD-1 PIC X."),
      fixedLine("01 REC-B."),
      fixedLine("   05 FIELD-1 PIC X."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE FIELD-1 OF REC-A TO FIELD-1 OF REC-B."),
      fixedLine("    DISPLAY FIELD-1 OF REC-A."),
      fixedLine("    DISPLAY FIELD-1 OF REC-B."),
    ]);

    const defLine = doc.getText().split("\n")[7];
    const defPos = defLine.indexOf("FIELD-1");
    const result = performRename(doc, Position.create(7, defPos + 1), "FIELD-B");

    expect(result).toBeDefined();
    const edits = result!.changes!["file:///test.cob"];
    const lines = edits.map((e) => e.range.start.line).sort((a, b) => a - b);
    expect(lines).toEqual([7, 10, 12]);
    expect(edits.every((e) => e.newText === "FIELD-B")).toBe(true);
  });
});
