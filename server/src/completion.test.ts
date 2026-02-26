/**
 * Tests for completion.ts — Completion/IntelliSense functionality.
 */
import { describe, it, expect } from "vitest";
import { buildCompletionItems } from "./completion";
import { buildDefinitionIndex } from "./definition";
import { TextDocument } from "vscode-languageserver-textdocument";
import { Position, CompletionItemKind } from "vscode-languageserver/node";

function makeDoc(lines: string[]): TextDocument {
  return TextDocument.create("file:///test.cob", "cobol85", 1, lines.join("\n"));
}

function fixedLine(lang: string, indicator: string = " "): string {
  const seq = "000000";
  const content = lang.padEnd(65, " ");
  return seq + indicator + content;
}

describe("buildCompletionItems", () => {
  it("returns COBOL verbs in PROCEDURE DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    "),
    ]);

    const items = buildCompletionItems(doc, Position.create(4, 11), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("PERFORM");
    expect(labels).toContain("MOVE");
    expect(labels).toContain("DISPLAY");
    expect(labels).toContain("IF");
  });

  it("filters by partial word in PROCEDURE DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PER"),
    ]);

    const items = buildCompletionItems(doc, Position.create(4, 14), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("PERFORM");
    expect(labels).not.toContain("MOVE");
  });

  it("includes paragraph names in PROCEDURE DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY 'HI'."),
      fixedLine("SUB-PARA."),
      fixedLine("    PERFORM "),
    ]);

    const items = buildCompletionItems(doc, Position.create(6, 19), []);
    const paraItems = items.filter((i) => i.kind === CompletionItemKind.Function);
    const labels = paraItems.map((i) => i.label);
    expect(labels).toContain("MAIN-PARA");
    expect(labels).toContain("SUB-PARA");
  });

  it("includes section names in PROCEDURE DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-SECTION SECTION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM "),
    ]);

    const items = buildCompletionItems(doc, Position.create(5, 19), []);
    const secItems = items.filter((i) => i.kind === CompletionItemKind.Module);
    const labels = secItems.map((i) => i.label);
    expect(labels).toContain("MAIN-SECTION");
  });

  it("includes data names in PROCEDURE DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-COUNTER PIC 9(5)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE 0 TO "),
    ]);

    const items = buildCompletionItems(doc, Position.create(7, 21), []);
    const dataItems = items.filter((i) => i.kind === CompletionItemKind.Variable);
    const labels = dataItems.map((i) => i.label);
    expect(labels).toContain("WS-COUNTER");
    const wsCounter = dataItems.find((i) => i.label === "WS-COUNTER");
    expect(wsCounter?.sortText).toBeDefined();
    expect(wsCounter?.labelDetails?.detail).toBe(" L01 PIC 9(5).");
    expect(wsCounter?.labelDetails?.description).toContain("Zeile 5");
  });

  it("includes copybook data names when an expanded index is provided", () => {
    const rootDoc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("COPY GD209F."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE GD209 TO WS-X."),
    ]);

    const expandedDoc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 GD209F PIC X(10)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE GD209 TO WS-X."),
    ]);
    const expandedIndex = buildDefinitionIndex(expandedDoc);

    const items = buildCompletionItems(rootDoc, Position.create(7, 16), [], expandedIndex);
    const dataItems = items.filter((i) => i.kind === CompletionItemKind.Variable);
    const labels = dataItems.map((i) => i.label);
    expect(labels).toContain("GD209F");
  });

  it("formats completion detail with mapped source location for local and copybook symbols", () => {
    const rootDoc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 LOCAL-FIELD PIC X(01)."),
      fixedLine("COPY MYCOPY."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE "),
    ]);

    const expandedDoc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 LOCAL-FIELD PIC X(01)."),
      fixedLine("01 COPY-FIELD PIC X(02)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE "),
    ]);
    const expandedIndex = buildDefinitionIndex(expandedDoc);

    const copyUri = "file:///copybooks/MYCOPY.cpy";
    const resolver = (line: number, character: number) => {
      if (line === 5) return { uri: copyUri, line: 12, character };
      return { uri: rootDoc.uri, line, character };
    };

    const items = buildCompletionItems(rootDoc, Position.create(8, 72), [], expandedIndex, resolver);
    const local = items.find((i) => i.label === "LOCAL-FIELD");
    const copy = items.find((i) => i.label === "COPY-FIELD");

    expect(local).toBeDefined();
    expect(local!.detail).toContain("Zeile 5");
    expect(local!.labelDetails?.description).toContain("Zeile 5");
    expect(copy).toBeDefined();
    expect(copy!.detail).toContain("MYCOPY.cpy:13");
    expect(copy!.labelDetails?.description).toContain("MYCOPY.cpy:13");
  });

  it("returns data clauses in DATA DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-RECORD "),
    ]);

    const items = buildCompletionItems(doc, Position.create(4, 20), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("PIC");
    expect(labels).toContain("VALUE");
    expect(labels).toContain("OCCURS");
    expect(labels).toContain("REDEFINES");
  });

  it("returns division keywords in unknown context", () => {
    const doc = makeDoc([
      fixedLine(""),
    ]);

    const items = buildCompletionItems(doc, Position.create(0, 7), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("IDENTIFICATION DIVISION.");
    expect(labels).toContain("DATA DIVISION.");
  });
});
