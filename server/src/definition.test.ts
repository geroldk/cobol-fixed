/**
 * Tests for definition.ts — Go-to-Definition functionality.
 */
import { describe, it, expect } from "vitest";
import { wordAtPosition, buildDefinitionIndex, findSymbolDefinition, findSymbolInIndex } from "./definition";
import { TextDocument } from "vscode-languageserver-textdocument";
import { Position } from "vscode-languageserver/node";
import { mapGenRange, PreBuilder } from "./preprocessor";

// ---- wordAtPosition ----

describe("wordAtPosition", () => {
  it("finds word at cursor position", () => {
    const line = "       MOVE WS-COUNTER TO WS-OUTPUT.";
    // Cursor on "WS-COUNTER" (position 12)
    const result = wordAtPosition(line, 12);
    expect(result).toBeDefined();
    expect(result!.word).toBe("WS-COUNTER");
  });

  it("returns undefined for whitespace", () => {
    const line = "       MOVE WS-COUNTER TO WS-OUTPUT.";
    const result = wordAtPosition(line, 5);
    expect(result).toBeUndefined();
  });

  it("finds word at start boundary", () => {
    const line = "       PERFORM MAIN-PROCESS.";
    const result = wordAtPosition(line, 7);
    expect(result).toBeDefined();
    expect(result!.word).toBe("PERFORM");
  });

  it("finds word at end boundary", () => {
    const line = "       PERFORM MAIN-PROCESS.";
    const result = wordAtPosition(line, 27);
    expect(result).toBeDefined();
    expect(result!.word).toBe("MAIN-PROCESS");
  });
});

// ---- buildDefinitionIndex ----

function makeDoc(lines: string[]): TextDocument {
  return TextDocument.create("file:///test.cob", "cobol85", 1, lines.join("\n"));
}

function fixedLine(lang: string, indicator: string = " "): string {
  const seq = "000000";
  const content = lang.padEnd(65, " ");
  return seq + indicator + content;
}

describe("buildDefinitionIndex", () => {
  it("finds paragraphs in PROCEDURE DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
      fixedLine("SUB-PARA."),
      fixedLine("    STOP RUN."),
    ]);

    const index = buildDefinitionIndex(doc);
    expect(index.paragraphs).toHaveLength(2);
    expect(index.paragraphs[0].name).toBe("MAIN-PARA");
    expect(index.paragraphs[1].name).toBe("SUB-PARA");
  });

  it("finds sections in PROCEDURE DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-SECTION SECTION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);

    const index = buildDefinitionIndex(doc);
    expect(index.sections).toHaveLength(1);
    expect(index.sections[0].name).toBe("MAIN-SECTION");
    expect(index.paragraphs).toHaveLength(1);
    expect(index.paragraphs[0].name).toBe("MAIN-PARA");
  });

  it("finds data items in DATA DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-RECORD."),
      fixedLine("   05 WS-NAME PIC X(30)."),
      fixedLine("   05 WS-AGE  PIC 99."),
      fixedLine("PROCEDURE DIVISION."),
    ]);

    const index = buildDefinitionIndex(doc);
    expect(index.dataItems).toHaveLength(3);
    expect(index.dataItems[0].name).toBe("WS-RECORD");
    expect(index.dataItems[0].level).toBe(1);
    expect(index.dataItems[1].name).toBe("WS-NAME");
    expect(index.dataItems[1].level).toBe(5);
    expect(index.dataItems[1].pic).toBeDefined();
    expect(index.dataItems[2].name).toBe("WS-AGE");
  });

  it("skips FILLER data items", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-RECORD."),
      fixedLine("   05 FILLER PIC X(10)."),
      fixedLine("   05 WS-DATA PIC X(20)."),
    ]);

    const index = buildDefinitionIndex(doc);
    const names = index.dataItems.map((d) => d.name);
    expect(names).not.toContain("FILLER");
    expect(names).toContain("WS-DATA");
  });

  it("does not treat END-IF as paragraph", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    IF WS-X = 1"),
      fixedLine("        DISPLAY 'YES'"),
      fixedLine("    END-IF."),
    ]);

    const index = buildDefinitionIndex(doc);
    const names = index.paragraphs.map((p) => p.name);
    expect(names).toContain("MAIN-PARA");
    expect(names).not.toContain("END-IF");
  });
});

// ---- findSymbolDefinition ----

describe("findSymbolDefinition", () => {
  it("finds paragraph definition from PERFORM reference", () => {
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
    // Position on "SUB-PARA" in the PERFORM line (line 4, character ~15)
    const performLine = doc.getText().split("\n")[4];
    const subParaIdx = performLine.indexOf("SUB-PARA");
    const result = findSymbolDefinition(doc, Position.create(4, subParaIdx + 2), index);

    expect(result).toBeDefined();
    expect(result!.range.start.line).toBe(6); // line where SUB-PARA. is defined
  });

  it("finds data item definition from PROCEDURE DIVISION reference", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-COUNTER PIC 9(5)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE 0 TO WS-COUNTER."),
    ]);

    const index = buildDefinitionIndex(doc);
    const moveLine = doc.getText().split("\n")[7];
    const wsIdx = moveLine.indexOf("WS-COUNTER");
    const result = findSymbolDefinition(doc, Position.create(7, wsIdx + 3), index);

    expect(result).toBeDefined();
    expect(result!.range.start.line).toBe(4); // DATA DIVISION line with WS-COUNTER
  });
});

describe("copybook-mapped definition resolution", () => {
  it("maps a data-name definition to copybook URI via preprocess segments", () => {
    const rootUri = "file:///main.cob";
    const copyUri = "file:///COPY/GD209F.cpy";

    const rootLine0 = fixedLine("IDENTIFICATION DIVISION.");
    const rootLine1 = fixedLine("PROGRAM-ID. TEST1.");
    const rootLine2 = fixedLine("DATA DIVISION.");
    const copyLine0 = fixedLine("01 GD209F PIC X(10).");
    const rootLine3 = fixedLine("PROCEDURE DIVISION.");
    const rootLine4 = fixedLine("MAIN-PARA.");
    const rootLine5 = fixedLine("    DISPLAY GD209F.");

    const builder = new PreBuilder();
    builder.appendSourceLine(rootUri, 0, 0, rootLine0, rootLine0.length);
    builder.appendSourceLine(rootUri, 1, 0, rootLine1, rootLine1.length);
    builder.appendSourceLine(rootUri, 2, 0, rootLine2, rootLine2.length);
    builder.appendSourceLine(copyUri, 0, 0, copyLine0, copyLine0.length);
    builder.appendSourceLine(rootUri, 3, 0, rootLine3, rootLine3.length);
    builder.appendSourceLine(rootUri, 4, 0, rootLine4, rootLine4.length);
    builder.appendSourceLine(rootUri, 5, 0, rootLine5, rootLine5.length);

    const pre = builder.build();
    const preDoc = TextDocument.create(rootUri, "cobol85", 1, pre.text);
    const index = buildDefinitionIndex(preDoc);
    const target = findSymbolInIndex("GD209F", index);

    expect(target).toBeDefined();

    const startOff = preDoc.offsetAt(Position.create(target!.line, target!.character));
    const endOff = preDoc.offsetAt(Position.create(target!.line, target!.endCharacter));
    const mapped = mapGenRange(pre, startOff, endOff);

    expect(mapped).toBeDefined();
    expect(mapped!.uri).toBe(copyUri);
    expect(mapped!.range.start.line).toBe(0);
  });
});
