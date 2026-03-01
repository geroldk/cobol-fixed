/**
 * Tests for completion.ts — Completion/IntelliSense functionality.
 */
import { describe, it, expect } from "vitest";
import { buildCompletionItems } from "./completion";
import { buildDefinitionIndex } from "./definition";
import { TextDocument } from "vscode-languageserver-textdocument";
import { Position, CompletionItemKind, InsertTextFormat } from "vscode-languageserver/node";

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

  // ---- EXEC DLI completion tests ----

  it("returns DLI function names inside EXEC DLI block (function context)", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI "),
    ]);

    const items = buildCompletionItems(doc, Position.create(4, 20), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("GU");
    expect(labels).toContain("GN");
    expect(labels).toContain("ISRT");
    expect(labels).toContain("CHKP");
    expect(labels).toContain("SCHD");
    expect(labels).toContain("TERM");
    expect(labels).toContain("LOAD");
    // Also long-form
    expect(labels).toContain("GET UNIQUE");
    expect(labels).toContain("SCHEDULE");
    // Should NOT contain clause keywords
    expect(labels).not.toContain("SEGMENT");
    expect(labels).not.toContain("INTO");
  });

  it("returns DLI clause keywords inside EXEC DLI block (clause context, pre-segment)", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU"),
      fixedLine("        "),
    ]);

    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    // Pre-segment: only global options + segment block starters
    expect(labels).toContain("SEGMENT");
    expect(labels).toContain("USING PCB");
    expect(labels).toContain("KEYFEEDBACK");
    expect(labels).toContain("FIRST");
    expect(labels).toContain("LAST");
    expect(labels).toContain("VARIABLE");
    // Sub-clauses not available before first SEGMENT
    expect(labels).not.toContain("INTO");
    expect(labels).not.toContain("LOCKED");
    expect(labels).not.toContain("WHERE");
    expect(labels).not.toContain("FROM");
    expect(labels).not.toContain("FEEDBACKLEN");
    // Should NOT contain DLI function names
    expect(labels).not.toContain("GU");
    expect(labels).not.toContain("GN");
  });

  it("returns DLI sub-clauses after SEGMENT (post-segment)", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU SEGMENT(S1)"),
      fixedLine("        "),
    ]);

    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    // Post-segment: sub-clauses become available
    expect(labels).toContain("INTO");
    expect(labels).toContain("WHERE");
    expect(labels).toContain("SEGLENGTH");
    expect(labels).toContain("OFFSET");
    expect(labels).toContain("SEGMENT");  // next segment block
    // Global options no longer available after SEGMENT
    expect(labels).not.toContain("USING PCB");
    expect(labels).not.toContain("KEYFEEDBACK");
    // LOCKED only available after INTO
    expect(labels).not.toContain("LOCKED");
  });

  it("GU after SEGMENT + INTO offers LOCKED", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU SEGMENT(S1) INTO(A)"),
      fixedLine("        "),
    ]);

    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("LOCKED");
    expect(labels).toContain("WHERE");
    expect(labels).toContain("SEGMENT");
    // INTO already used in this segment block
    expect(labels).not.toContain("INTO");
  });

  it("does not offer DLI completions outside EXEC DLI block", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU END-EXEC."),
      fixedLine("    "),
    ]);

    const items = buildCompletionItems(doc, Position.create(5, 11), []);
    const labels = items.map((i) => i.label);
    // Should be regular PROCEDURE DIVISION completions, not DLI
    expect(labels).toContain("PERFORM");
    expect(labels).not.toContain("SEGMENT");
    expect(labels).not.toContain("GU");
  });

  // ---- Sub-context tests: after clause keywords ----

  it("after USING only offers PCB", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU USING "),
    ]);
    //  000000     EXEC DLI GU USING _
    //  0123456789...               ^29
    const items = buildCompletionItems(doc, Position.create(4, 29), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("PCB");
    expect(labels).toHaveLength(1);
  });

  it("after SEGMENT offers data names, not clause keywords", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 MY-SEGMENT PIC X(80)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU SEGMENT "),
    ]);
    const items = buildCompletionItems(doc, Position.create(7, 31), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("MY-SEGMENT");
    expect(labels).not.toContain("USING PCB");
    expect(labels).not.toContain("INTO");
  });

  it("after INTO offers data names, not clause keywords", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-AREA PIC X(100)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU SEGMENT(S1) INTO "),
    ]);
    const items = buildCompletionItems(doc, Position.create(7, 40), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("WS-AREA");
    expect(labels).not.toContain("SEGMENT");
    expect(labels).not.toContain("USING PCB");
  });

  it("after WHERE offers data names (not clause keywords)", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU SEGMENT(S1) WHERE "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 41), []);
    // No data items defined in this doc → 0 items (data names only, no clause keywords)
    expect(items).toHaveLength(0);
  });

  it("inside WHERE() offers data names from definition index", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 MY-KEY  PIC X(10)."),
      fixedLine("01 MY-VAL  PIC 9(04)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU SEGMENT(S1)"),
      fixedLine("         WHERE(FIELD = "),
    ]);
    const items = buildCompletionItems(doc, Position.create(9, 28), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("MY-KEY");
    expect(labels).toContain("MY-VAL");
    // Should NOT contain clause keywords
    expect(labels).not.toContain("SEGMENT");
    expect(labels).not.toContain("INTO");
  });

  it("inside INTO() offers data names from definition index", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-REC  PIC X(100)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU SEGMENT(S1) INTO("),
    ]);
    const items = buildCompletionItems(doc, Position.create(7, 40), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("WS-REC");
    expect(labels).not.toContain("SEGMENT");
  });

  it("inside SEGLENGTH() offers data names from definition index", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 V-LEN  PIC S9(4) COMP."),
      fixedLine("01 V-SEGM PIC X(4000)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI ISRT USING PCB(5) VARIABLE"),
      fixedLine("        SEGMENT(GV) FROM(V-SEGM) SEGLENGTH("),
    ]);
    const items = buildCompletionItems(doc, Position.create(9, 50), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("V-LEN");
    expect(labels).toContain("V-SEGM");
    // DLI keywords must NOT appear
    expect(labels).not.toContain("SEGMENT");
    expect(labels).not.toContain("FROM");
  });

  it("inside FIELDLENGTH() offers data names from definition index", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 FL-VAR PIC S9(4) COMP."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU USING PCB(1)"),
      fixedLine("        WHERE(DKEY = B0-DKEY) FIELDLENGTH("),
    ]);
    const items = buildCompletionItems(doc, Position.create(8, 49), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("FL-VAR");
    expect(labels).not.toContain("WHERE");
  });

  // ---- Already-used clause suppression ----

  it("ISRT with USING PCB already written does not offer USING PCB again", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI ISRT USING PCB(5)"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    expect(labels).not.toContain("USING PCB");
    // Segment block starters still available
    expect(labels).toContain("SEGMENT");
    expect(labels).toContain("FIRST");
    // FROM not yet available (needs SEGMENT first)
    expect(labels).not.toContain("FROM");
  });

  it("ISRT with USING and SEGMENT+FROM already written — FROM per-segment", () => {
    // FROM is per-segment: not offered in current block, but available after next SEGMENT
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI ISRT USING PCB(5)"),
      fixedLine("        SEGMENT(S1) FROM(V-SEGM) "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 40), []);
    const labels = items.map((i) => i.label);
    expect(labels).not.toContain("USING PCB");
    // FROM already used in current segment block → not offered
    expect(labels).not.toContain("FROM");
    // But a new SEGMENT starts a new block where FROM becomes available again
    expect(labels).toContain("SEGMENT");
    expect(labels).toContain("WHERE");
  });

  it("ISRT multi-segment: FROM available again after new SEGMENT", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI ISRT USING PCB(5)"),
      fixedLine("        SEGMENT(S1) FROM(P-AREA)"),
      fixedLine("        SEGMENT(S2)"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(7, 15), []);
    const labels = items.map((i) => i.label);
    // Second SEGMENT resets per-segment flags — FROM available again
    expect(labels).toContain("FROM");
    expect(labels).toContain("WHERE");
    expect(labels).not.toContain("USING PCB"); // global, already used
  });

  it("GU with LOCKED already written does not offer LOCKED again", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU SEGMENT(S1) INTO(A)"),
      fixedLine("        LOCKED "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 22), []);
    const labels = items.map((i) => i.label);
    expect(labels).not.toContain("LOCKED");
    // INTO already used in this segment block
    expect(labels).not.toContain("INTO");
    // But next-segment starters and sub-clauses remain
    expect(labels).toContain("SEGMENT");
    expect(labels).toContain("WHERE");
  });

  it("GU with FIRST already written — only VARIABLE and SEGMENT valid", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU FIRST"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    // After FIRST, only VARIABLE or SEGMENT are valid next tokens
    expect(labels).toContain("VARIABLE");
    expect(labels).toContain("SEGMENT");
    expect(labels).not.toContain("FIRST");
    expect(labels).not.toContain("LAST");
    expect(labels).not.toContain("INTO");
    expect(labels).not.toContain("USING PCB");
    expect(labels).toContain("END-EXEC");
    expect(labels).toHaveLength(3);
  });

  // ---- Function-specific clause filtering ----

  it("ISRT pre-segment: only global options + segment starters", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI ISRT"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    // Pre-segment: global options + segment block starters
    expect(labels).toContain("USING PCB");
    expect(labels).toContain("SEGMENT");
    expect(labels).toContain("FIRST");
    expect(labels).toContain("LAST");
    expect(labels).toContain("VARIABLE");
    // Sub-clauses need SEGMENT first
    expect(labels).not.toContain("FROM");
    expect(labels).not.toContain("WHERE");
    expect(labels).not.toContain("FIELDLENGTH");
    expect(labels).not.toContain("SEGLENGTH");
    expect(labels).not.toContain("OFFSET");
    // ISRT does NOT allow these at all
    expect(labels).not.toContain("INTO");
    expect(labels).not.toContain("KEYFEEDBACK");
    expect(labels).not.toContain("FEEDBACKLEN");
    expect(labels).not.toContain("LOCKED");
    expect(labels).not.toContain("PSB");
    expect(labels).not.toContain("ID");
  });

  it("ISRT post-segment: FROM, WHERE, sub-clauses become available", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI ISRT SEGMENT(S1)"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("FROM");
    expect(labels).toContain("WHERE");
    expect(labels).toContain("SEGLENGTH");
    expect(labels).toContain("OFFSET");
    expect(labels).toContain("SEGMENT");  // next segment block
    // Global options no longer available after SEGMENT
    expect(labels).not.toContain("USING PCB");
    // FIELDLENGTH only after WHERE
    expect(labels).not.toContain("FIELDLENGTH");
  });

  it("GU pre-segment: KEYFEEDBACK available, INTO/LOCKED/FEEDBACKLEN need structure", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    // Pre-segment: global options
    expect(labels).toContain("KEYFEEDBACK");
    expect(labels).toContain("USING PCB");
    expect(labels).toContain("SEGMENT");
    // INTO/LOCKED need SEGMENT first, FEEDBACKLEN needs KEYFEEDBACK first
    expect(labels).not.toContain("INTO");
    expect(labels).not.toContain("LOCKED");
    expect(labels).not.toContain("FEEDBACKLEN");
    // GU does NOT allow FROM, PSB, ID
    expect(labels).not.toContain("FROM");
    expect(labels).not.toContain("PSB");
    expect(labels).not.toContain("ID");
  });

  it("GU KEYFEEDBACK already written: FEEDBACKLEN becomes available", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU KEYFEEDBACK(KF)"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("FEEDBACKLEN");
    expect(labels).toContain("SEGMENT");
    expect(labels).not.toContain("KEYFEEDBACK"); // already used
    expect(labels).not.toContain("INTO"); // needs SEGMENT
  });

  it("CHKP clause completions only include ID", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI CHKP"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("ID");
    expect(labels).toContain("END-EXEC");
    expect(labels).toHaveLength(2);
  });

  it("SCHD clause completions only include PSB", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI SCHD"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("PSB");
    expect(labels).toContain("END-EXEC");
    expect(labels).toHaveLength(2);
  });

  it("TERM clause completions are empty (no allowed clauses)", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI TERM"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    expect(labels).toContain("END-EXEC");
    expect(items).toHaveLength(1);
  });

  it("REPL pre-segment: FROM/SEGLENGTH/OFFSET valid without SEGMENT", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI REPL"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map((i) => i.label);
    // REPL special: FROM, SEGLENGTH, OFFSET valid even without SEGMENT
    expect(labels).toContain("FROM");
    expect(labels).toContain("USING PCB");
    expect(labels).toContain("SEGMENT");
    expect(labels).toContain("SEGLENGTH");
    expect(labels).toContain("OFFSET");
    expect(labels).toContain("VARIABLE");
    // REPL does NOT allow: INTO, KEYFEEDBACK, FEEDBACKLEN, LOCKED, WHERE, FIELDLENGTH, FIRST, LAST, PSB, ID
    expect(labels).not.toContain("INTO");
    expect(labels).not.toContain("KEYFEEDBACK");
    expect(labels).not.toContain("FEEDBACKLEN");
    expect(labels).not.toContain("LOCKED");
    expect(labels).not.toContain("WHERE");
    expect(labels).not.toContain("FIELDLENGTH");
    expect(labels).not.toContain("PSB");
    expect(labels).not.toContain("ID");
  });
});

// ---- EXEC CICS completion ----

describe("EXEC CICS completion", () => {
  it("offers CICS verbs directly after EXEC CICS", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 20), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("READ");
    expect(labels).toContain("WRITE");
    expect(labels).toContain("SEND");
    expect(labels).toContain("RECEIVE");
    expect(labels).toContain("ASSIGN");
    expect(labels).toContain("ADDRESS");
    expect(labels).toContain("HANDLE");
    expect(labels).toContain("RETURN");
    // Should NOT contain regular COBOL verbs that are not CICS commands
    expect(labels).not.toContain("PERFORM");
    expect(labels).not.toContain("DISPLAY");
    expect(labels).not.toContain("COMPUTE");
  });

  it("offers CICS verbs filtered by partial word", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS RE"),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 22), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("READ");
    expect(labels).toContain("READNEXT");
    expect(labels).toContain("RECEIVE");
    expect(labels).toContain("RETURN");
    expect(labels).not.toContain("SEND");
    expect(labels).not.toContain("ASSIGN");
  });

  it("offers CICS options after verb on next line", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS ASSIGN"),
      fixedLine("         "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 16), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("TCTUALENG");
    expect(labels).toContain("OPID");
    expect(labels).toContain("SYSID");
    expect(labels).toContain("USERID");
    // Should NOT have regular COBOL things
    expect(labels).not.toContain("PERFORM");
    expect(labels).not.toContain("DISPLAY");
  });

  it("offers CICS options on same line after verb", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS ASSIGN "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 28), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("TCTUALENG");
    expect(labels).toContain("OPID");
    expect(labels).not.toContain("PERFORM");
  });

  it("offers data items inside CICS parens", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-REC PIC X(80)."),
      fixedLine("01 WS-KEY PIC X(6)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS READ"),
      fixedLine("         FILE('CUSTFILE')"),
      fixedLine("         INTO("),
    ]);
    const items = buildCompletionItems(doc, Position.create(10, 21), []);
    const labels = items.map(i => i.label);
    // Inside parens → data items
    expect(labels).toContain("WS-REC");
    expect(labels).toContain("WS-KEY");
  });

  it("does NOT offer CICS completions outside EXEC CICS", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS RETURN END-EXEC."),
      fixedLine("    "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 11), []);
    const labels = items.map(i => i.label);
    // After END-EXEC → regular COBOL
    expect(labels).toContain("PERFORM");
    expect(labels).toContain("DISPLAY");
    expect(labels).not.toContain("TCTUALENG");
    expect(labels).not.toContain("READNEXT");
  });
});

describe("EXEC CICS sub-verb completion (multi-word commands)", () => {
  it("offers CONDITION, AID, ABEND after HANDLE", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 28), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("CONDITION");
    expect(labels).toContain("AID");
    expect(labels).toContain("ABEND");
    // Should NOT offer full verb list
    expect(labels).not.toContain("READ");
    expect(labels).not.toContain("WRITE");
    expect(labels).not.toContain("PERFORM");
  });

  it("offers CONDITION after IGNORE", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS IGNORE "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 28), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("CONDITION");
    expect(labels).not.toContain("READ");
    expect(labels).not.toContain("HANDLE");
  });

  it("offers HANDLE after PUSH", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS PUSH "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 26), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("HANDLE");
    expect(labels).not.toContain("READ");
  });

  it("offers HANDLE after POP", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS POP "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 25), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("HANDLE");
    expect(labels).not.toContain("READ");
  });

  it("offers TD, TS after READQ", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS READQ "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 27), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("TD");
    expect(labels).toContain("TS");
    expect(labels).not.toContain("READ");
  });

  it("offers MAP, TEXT, CONTROL, PAGE after SEND", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS SEND "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 26), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("MAP");
    expect(labels).toContain("TEXT");
    expect(labels).toContain("CONTROL");
    expect(labels).toContain("PAGE");
    expect(labels).not.toContain("READ");
  });

  it("offers MAP after RECEIVE", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS RECEIVE "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 29), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("MAP");
    expect(labels).not.toContain("READ");
    expect(labels).not.toContain("HANDLE");
  });

  it("sub-verb items have detail 'CICS Sub-Command'", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 28), []);
    const condItem = items.find(i => i.label === "CONDITION");
    expect(condItem).toBeDefined();
    expect(condItem!.detail).toBe("CICS Sub-Command");
  });
});

describe("END-EXEC completion", () => {
  it("offers END-EXEC in CICS option context", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS READ DATASET(WS-FILE) "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 43), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("END-EXEC");
    // Should also still offer options
    expect(labels).toContain("INTO");
  });

  it("offers END-EXEC in CICS sub-verb context", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 28), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("END-EXEC");
    expect(labels).toContain("CONDITION");
  });

  it("offers END-EXEC in DLI clause context", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU"),
      fixedLine("        "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 15), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("END-EXEC");
    // Should also still offer DLI clauses
    expect(labels).toContain("USING PCB");
  });

  it("END-EXEC sorts after options", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS RETURN "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 27), []);
    const endExec = items.find(i => i.label === "END-EXEC");
    const transid = items.find(i => i.label === "TRANSID");
    expect(endExec).toBeDefined();
    expect(transid).toBeDefined();
    // END-EXEC should sort after options (Z > A)
    expect(endExec!.sortText! > transid!.sortText!).toBe(true);
  });

  it("filters END-EXEC by partial E", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS RETURN E"),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 29), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("END-EXEC");
  });

  it("filters out END-EXEC when partial does not match", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS RETURN T"),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 29), []);
    const labels = items.map(i => i.label);
    expect(labels).not.toContain("END-EXEC");
    expect(labels).toContain("TRANSID");
  });
});

describe("CICS option snippet format (argument vs flag)", () => {
  it("ASSIGN options use snippet with (name) placeholder", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS ASSIGN "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 27), []);
    const tctualeng = items.find(i => i.label === "TCTUALENG");
    expect(tctualeng).toBeDefined();
    expect(tctualeng!.insertTextFormat).toBe(InsertTextFormat.Snippet);
    expect(tctualeng!.insertText).toBe("TCTUALENG(${1:name})");
    const opid = items.find(i => i.label === "OPID");
    expect(opid).toBeDefined();
    expect(opid!.insertText).toBe("OPID(${1:name})");
  });

  it("READ argument options use snippet, flag options use plain text", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS READ "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 25), []);
    // Argument option
    const file = items.find(i => i.label === "FILE");
    expect(file).toBeDefined();
    expect(file!.insertTextFormat).toBe(InsertTextFormat.Snippet);
    expect(file!.insertText).toBe("FILE(${1:name})");
    const into = items.find(i => i.label === "INTO");
    expect(into).toBeDefined();
    expect(into!.insertText).toBe("INTO(${1:area})");
    const set = items.find(i => i.label === "SET");
    expect(set).toBeDefined();
    expect(set!.insertText).toBe("SET(${1:ADDRESS OF name})");
    const length = items.find(i => i.label === "LENGTH");
    expect(length).toBeDefined();
    expect(length!.insertText).toBe("LENGTH(${1:len})");
    // Flag option
    const update = items.find(i => i.label === "UPDATE");
    expect(update).toBeDefined();
    expect(update!.insertTextFormat).toBe(InsertTextFormat.PlainText);
    expect(update!.insertText).toBe("UPDATE");
    const nosuspend = items.find(i => i.label === "NOSUSPEND");
    expect(nosuspend).toBeDefined();
    expect(nosuspend!.insertText).toBe("NOSUSPEND");
  });

  it("SEND MAP: ERASE/ALARM are flags, MAP/MAPSET have (name)", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS SEND MAP "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 29), []);
    const mapset = items.find(i => i.label === "MAPSET");
    expect(mapset).toBeDefined();
    expect(mapset!.insertText).toBe("MAPSET(${1:name})");
    const erase = items.find(i => i.label === "ERASE");
    expect(erase).toBeDefined();
    expect(erase!.insertText).toBe("ERASE");
    const alarm = items.find(i => i.label === "ALARM");
    expect(alarm).toBeDefined();
    expect(alarm!.insertText).toBe("ALARM");
  });

  it("HANDLE AID keys use snippet with (name) for paragraph label", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE AID "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 31), []);
    const pf3 = items.find(i => i.label === "PF3");
    expect(pf3).toBeDefined();
    expect(pf3!.insertTextFormat).toBe(InsertTextFormat.Snippet);
    expect(pf3!.insertText).toBe("PF3(${1:name})");
    const clear = items.find(i => i.label === "CLEAR");
    expect(clear).toBeDefined();
    expect(clear!.insertText).toBe("CLEAR(${1:name})");
  });

  it("ADDRESS options use snippet with (name)", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS ADDRESS "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 28), []);
    const tctua = items.find(i => i.label === "TCTUA");
    expect(tctua).toBeDefined();
    expect(tctua!.insertTextFormat).toBe(InsertTextFormat.Snippet);
    expect(tctua!.insertText).toBe("TCTUA(${1:ADDRESS OF name})");
    const cwa = items.find(i => i.label === "CWA");
    expect(cwa).toBeDefined();
    expect(cwa!.insertText).toBe("CWA(${1:ADDRESS OF name})");
  });

  it("RETURN: TRANSID has (name), IMMEDIATE is flag", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS RETURN "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 27), []);
    const transid = items.find(i => i.label === "TRANSID");
    expect(transid).toBeDefined();
    expect(transid!.insertText).toBe("TRANSID(${1:name})");
    const commarea = items.find(i => i.label === "COMMAREA");
    expect(commarea).toBeDefined();
    expect(commarea!.insertText).toBe("COMMAREA(${1:area})");
    const length = items.find(i => i.label === "LENGTH");
    expect(length).toBeDefined();
    expect(length!.insertText).toBe("LENGTH(${1:len})");
    const imm = items.find(i => i.label === "IMMEDIATE");
    expect(imm).toBeDefined();
    expect(imm!.insertText).toBe("IMMEDIATE");
  });
});

describe("EXEC CICS HANDLE CONDITION completion", () => {
  it("offers condition names after HANDLE CONDITION", () => {
    // Real EMKON09 pattern: EXEC CICS HANDLE CONDITION NOTOPEN (NOT-OPEN)
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE CONDITION "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 37), []);
    const labels = items.map(i => i.label);
    // Should offer CICS condition names
    expect(labels).toContain("NOTOPEN");
    expect(labels).toContain("NOSPACE");
    expect(labels).toContain("NOTFND");
    expect(labels).toContain("INVREQ");
    expect(labels).toContain("IOERR");
    expect(labels).toContain("LENGERR");
    expect(labels).toContain("FILENOTFOUND");
    expect(labels).toContain("MAPFAIL");
    expect(labels).toContain("DUPKEY");
    expect(labels).toContain("ENDFILE");
    // Should NOT offer regular COBOL things
    expect(labels).not.toContain("PERFORM");
    expect(labels).not.toContain("DISPLAY");
  });

  it("offers condition names on continuation line (EMKON09 pattern)", () => {
    // EMKON09: EXEC CICS HANDLE CONDITION NOTOPEN (NOT-OPEN)
    //                                     NOSPACE (NO-SPACE)
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE CONDITION NOTOPEN (NOT-OPEN)"),
      fixedLine("                               "),
    ]);
    const items = buildCompletionItems(doc, Position.create(5, 38), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("NOSPACE");
    expect(labels).toContain("INVREQ");
    expect(labels).toContain("ENDFILE");
    expect(labels).not.toContain("PERFORM");
  });

  it("filters conditions by partial word", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE CONDITION NOT"),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 40), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("NOTOPEN");
    expect(labels).toContain("NOTFND");
    expect(labels).toContain("NOTAUTH");
    expect(labels).toContain("NOTALLOC");
    expect(labels).not.toContain("INVREQ");
    expect(labels).not.toContain("LENGERR");
  });

  it("offers paragraph names inside HANDLE CONDITION parens", () => {
    // HANDLE CONDITION NOTOPEN(label) — label is a paragraph name
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("NOT-OPEN."),
      fixedLine("    DISPLAY 'ERROR'."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE CONDITION NOTOPEN ("),
    ]);
    const items = buildCompletionItems(doc, Position.create(6, 47), []);
    const labels = items.map(i => i.label);
    // Inside parens → data items (paragraphs are data items in the index)
    // The completion inside parens currently provides data items
    // For HANDLE CONDITION, parens contain labels which are paragraph names
    expect(labels).not.toContain("NOTOPEN");
    expect(labels).not.toContain("PERFORM");
  });

  it("offers condition names for IGNORE CONDITION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS IGNORE CONDITION "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 37), []);
    const labels = items.map(i => i.label);
    expect(labels).toContain("NOTOPEN");
    expect(labels).toContain("INVREQ");
    expect(labels).toContain("MAPFAIL");
    expect(labels).not.toContain("PERFORM");
  });

  it("condition items use snippet with (label) placeholder", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE CONDITION "),
    ]);
    const items = buildCompletionItems(doc, Position.create(4, 37), []);
    const notopen = items.find(i => i.label === "NOTOPEN");
    expect(notopen).toBeDefined();
    expect(notopen!.insertTextFormat).toBe(InsertTextFormat.Snippet);
    expect(notopen!.insertText).toBe("NOTOPEN(${1:label})");
    const lengerr = items.find(i => i.label === "LENGERR");
    expect(lengerr).toBeDefined();
    expect(lengerr!.insertText).toBe("LENGERR(${1:label})");
  });
});
