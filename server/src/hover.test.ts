/**
 * Tests for hover.ts — Hover information.
 */
import { describe, it, expect } from "vitest";
import { buildHover } from "./hover";
import { TextDocument } from "vscode-languageserver-textdocument";
import { Position } from "vscode-languageserver/node";

function makeDoc(lines: string[]): TextDocument {
  return TextDocument.create("file:///test.cob", "cobol85", 1, lines.join("\n"));
}

function fixedLine(lang: string, indicator: string = " "): string {
  const seq = "000000";
  const content = lang.padEnd(65, " ");
  return seq + indicator + content;
}

describe("buildHover", () => {
  it("returns paragraph hover info", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM MAIN-PARA."),
    ]);

    // Hover over MAIN-PARA on the PERFORM line (line 4)
    const line4 = doc.getText().split("\n")[4];
    const idx = line4.indexOf("MAIN-PARA");
    const hover = buildHover(doc, Position.create(4, idx + 2), []);

    expect(hover).toBeDefined();
    expect(hover!.contents).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("Paragraph");
    expect(md.value).toContain("MAIN-PARA");
  });

  it("returns data item hover with PIC info", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-NAME PIC X(30)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY WS-NAME."),
    ]);

    const line7 = doc.getText().split("\n")[7];
    const idx = line7.indexOf("WS-NAME");
    const hover = buildHover(doc, Position.create(7, idx + 2), []);

    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("WS-NAME");
    expect(md.value).toContain("PIC");
  });

  it("returns section hover info", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-SECTION SECTION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM MAIN-SECTION."),
    ]);

    const line5 = doc.getText().split("\n")[5];
    const idx = line5.indexOf("MAIN-SECTION");
    const hover = buildHover(doc, Position.create(5, idx + 2), []);

    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("Section");
    expect(md.value).toContain("MAIN-SECTION");
  });

  it("returns undefined for non-COBOL word positions", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
    ]);

    // Hover on whitespace area
    const hover = buildHover(doc, Position.create(3, 2), []);
    expect(hover).toBeUndefined();
  });
});
