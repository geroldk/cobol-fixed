/**
 * Tests for dead-code.ts — Dead-Code detection.
 */
import { describe, it, expect } from "vitest";
import { TextDocument } from "vscode-languageserver-textdocument";
import { findDeadCode, lintDeadCode } from "./dead-code";
import { buildDefinitionIndex } from "./definition";
import { DiagnosticSeverity } from "vscode-languageserver/node";

// ---- Helpers ----

function makeDoc(lines: string[]): TextDocument {
  return TextDocument.create("file:///test.cob", "cobol85", 1, lines.join("\n"));
}

function fixedLine(lang: string, indicator: string = " "): string {
  const seq = "000000";
  const content = lang.padEnd(65, " ");
  return seq + indicator + content;
}

// ---- findDeadCode ----

describe("findDeadCode", () => {
  it("detects unreferenced paragraphs", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("DEAD-PARA."),
      fixedLine("    DISPLAY 'NEVER CALLED'."),
    ]);

    const dead = findDeadCode(doc);
    expect(dead.length).toBe(1);
    expect(dead[0].name).toBe("DEAD-PARA");
    expect(dead[0].kind).toBe("paragraph");
  });

  it("does not flag referenced paragraphs", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM SUB-PARA"),
      fixedLine("    STOP RUN."),
      fixedLine("SUB-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);

    const dead = findDeadCode(doc);
    expect(dead.length).toBe(0);
  });

  it("does not flag the entry paragraph", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
    ]);

    const dead = findDeadCode(doc);
    expect(dead.length).toBe(0);
  });

  it("detects unreferenced sections", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-SECTION SECTION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("DEAD-SECTION SECTION."),
      fixedLine("DEAD-PARA."),
      fixedLine("    DISPLAY 'NEVER CALLED'."),
    ]);

    const dead = findDeadCode(doc);
    // DEAD-SECTION and DEAD-PARA are both unreferenced
    const deadNames = dead.map(d => d.name);
    expect(deadNames).toContain("DEAD-SECTION");
    expect(deadNames).toContain("DEAD-PARA");
  });

  it("returns empty when no paragraphs exist", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-X PIC X."),
    ]);

    const dead = findDeadCode(doc);
    expect(dead.length).toBe(0);
  });

  it("detects multiple dead paragraphs", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("DEAD-1."),
      fixedLine("    DISPLAY 'A'."),
      fixedLine("DEAD-2."),
      fixedLine("    DISPLAY 'B'."),
    ]);

    const dead = findDeadCode(doc);
    expect(dead.length).toBe(2);
    const names = dead.map(d => d.name).sort();
    expect(names).toEqual(["DEAD-1", "DEAD-2"]);
  });

  it("does not flag paragraphs covered by PERFORM THRU range", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM STEP-A THRU STEP-C"),
      fixedLine("    STOP RUN."),
      fixedLine("STEP-A."),
      fixedLine("    DISPLAY 'A'."),
      fixedLine("STEP-B."),
      fixedLine("    DISPLAY 'B'."),
      fixedLine("STEP-C."),
      fixedLine("    DISPLAY 'C'."),
    ]);

    const dead = findDeadCode(doc);
    // STEP-A, STEP-B, STEP-C are all covered by PERFORM THRU
    expect(dead.length).toBe(0);
  });

  it("does not flag paragraphs covered by PERFORM THROUGH range", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM STEP-A THROUGH STEP-B"),
      fixedLine("    STOP RUN."),
      fixedLine("STEP-A."),
      fixedLine("    DISPLAY 'A'."),
      fixedLine("STEP-B."),
      fixedLine("    DISPLAY 'B'."),
      fixedLine("DEAD-PARA."),
      fixedLine("    DISPLAY 'DEAD'."),
    ]);

    const dead = findDeadCode(doc);
    // STEP-A and STEP-B are covered, DEAD-PARA is not
    expect(dead.length).toBe(1);
    expect(dead[0].name).toBe("DEAD-PARA");
  });
});

// ---- lintDeadCode ----

describe("lintDeadCode", () => {
  it("produces hint-severity diagnostics for dead code", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
      fixedLine("DEAD-PARA."),
      fixedLine("    DISPLAY 'NEVER'."),
    ]);

    const diags = lintDeadCode(doc);
    expect(diags.length).toBe(1);
    expect(diags[0].code).toBe("DEAD_CODE");
    expect(diags[0].severity).toBe(DiagnosticSeverity.Hint);
    expect(diags[0].message).toContain("DEAD-PARA");
  });

  it("returns no diagnostics when all paragraphs are used", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM SUB-PARA"),
      fixedLine("    STOP RUN."),
      fixedLine("SUB-PARA."),
      fixedLine("    DISPLAY 'OK'."),
    ]);

    const diags = lintDeadCode(doc);
    expect(diags.length).toBe(0);
  });
});
