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

  // ---- EXEC CICS hover suppression ----
  // Tests model the exact pattern from EMKON09.cbl:
  //   EXEC CICS ASSIGN TCTUALENG (TCTUALENG)
  //                    OPID      (OPER-ID)
  //                    SYSID     (SYS-ID)
  //                    END-EXEC.

  // Shared document matching real EMKON09.cbl structure
  function cicsHoverDoc() {
    return makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),           // 0
      fixedLine("PROGRAM-ID. TEST1."),                 // 1
      fixedLine("DATA DIVISION."),                     // 2
      fixedLine("WORKING-STORAGE SECTION."),           // 3
      fixedLine("01 HILFSFELDER."),                    // 4
      fixedLine("   03 TCTUALENG PIC S9(4) COMP."),   // 5
      fixedLine("   03 OPER-ID   PIC X(3)."),         // 6
      fixedLine("   03 SYS-ID    PIC X(4)."),         // 7
      fixedLine("PROCEDURE DIVISION."),                // 8
      fixedLine("MAIN-PARA."),                         // 9
      // Real pattern: option on same line as EXEC CICS, space before paren
      fixedLine("    EXEC CICS ASSIGN TCTUALENG (TCTUALENG)"), // 10
      fixedLine("                     OPID      (OPER-ID)"),   // 11
      fixedLine("                     SYSID     (SYS-ID)"),    // 12
      fixedLine("                     END-EXEC."),             // 13
      fixedLine("    DISPLAY TCTUALENG."),                      // 14
      fixedLine("    STOP RUN."),                               // 15
    ]);
  }

  it("suppresses hover for TCTUALENG option on same line as EXEC CICS ASSIGN", () => {
    const doc = cicsHoverDoc();
    const dl = doc.getText().split("\n");
    // TCTUALENG appears twice on line 10: option keyword + data name in parens
    // Hover over the FIRST TCTUALENG (the option — outside parens)
    const line10 = dl[10];
    const tctIdx = line10.indexOf("TCTUALENG");
    const hover = buildHover(doc, Position.create(10, tctIdx + 2), []);
    expect(hover).toBeUndefined();
  });

  it("shows data-item hover for TCTUALENG inside parens (data reference)", () => {
    const doc = cicsHoverDoc();
    const dl = doc.getText().split("\n");
    // Hover over the SECOND TCTUALENG (data name inside parens)
    const line10 = dl[10];
    const firstIdx = line10.indexOf("TCTUALENG");
    const dataIdx = line10.indexOf("TCTUALENG", firstIdx + 9);
    const hover = buildHover(doc, Position.create(10, dataIdx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("TCTUALENG");
    expect(md.value).toContain("PIC");
    expect(md.value).toContain("HILFSFELDER");
  });

  it("suppresses hover for ASSIGN verb on same line as EXEC CICS", () => {
    // Add ASSIGN as data item to test suppression
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 ASSIGN PIC X(10)."),
      fixedLine("01 TCTUALENG PIC S9(4) COMP."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS ASSIGN TCTUALENG (TCTUALENG)"),
      fixedLine("                     END-EXEC."),
    ]);
    const dl = doc.getText().split("\n");
    const line8 = dl[8];
    const assignIdx = line8.indexOf("ASSIGN");
    const hover = buildHover(doc, Position.create(8, assignIdx + 2), []);
    expect(hover).toBeUndefined();
  });

  it("suppresses hover for OPID option on continuation line", () => {
    const doc = cicsHoverDoc();
    const dl = doc.getText().split("\n");
    // OPID is on line 11 — continuation of EXEC CICS block
    const line11 = dl[11];
    const opidIdx = line11.indexOf("OPID");
    const hover = buildHover(doc, Position.create(11, opidIdx + 2), []);
    expect(hover).toBeUndefined();
  });

  it("shows data-item hover for OPER-ID inside parens on continuation line", () => {
    const doc = cicsHoverDoc();
    const dl = doc.getText().split("\n");
    const line11 = dl[11];
    const operIdx = line11.indexOf("OPER-ID");
    const hover = buildHover(doc, Position.create(11, operIdx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("OPER-ID");
    expect(md.value).toContain("PIC");
  });

  it("suppresses hover for SYSID option on continuation line", () => {
    const doc = cicsHoverDoc();
    const dl = doc.getText().split("\n");
    const line12 = dl[12];
    const sysidIdx = line12.indexOf("SYSID");
    const hover = buildHover(doc, Position.create(12, sysidIdx + 2), []);
    expect(hover).toBeUndefined();
  });

  it("shows data-item hover for SYS-ID inside parens on continuation line", () => {
    const doc = cicsHoverDoc();
    const dl = doc.getText().split("\n");
    const line12 = dl[12];
    const sysIdx = line12.indexOf("SYS-ID");
    const hover = buildHover(doc, Position.create(12, sysIdx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("SYS-ID");
  });

  it("suppresses hover for CICS condition (NOTFND) in HANDLE CONDITION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 NOTFND PIC X."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE CONDITION"),
      fixedLine("         NOTFND(ERR-PARA)"),
      fixedLine("    END-EXEC."),
    ]);
    const dl = doc.getText().split("\n");
    const line8 = dl[8];
    const idx = line8.indexOf("NOTFND");
    const hover = buildHover(doc, Position.create(8, idx + 2), []);
    expect(hover).toBeUndefined();
  });

  it("shows normal hover for TCTUALENG OUTSIDE EXEC CICS block", () => {
    const doc = cicsHoverDoc();
    const dl = doc.getText().split("\n");
    // Line 14: DISPLAY TCTUALENG — outside any EXEC CICS block
    const line14 = dl[14];
    const idx = line14.indexOf("TCTUALENG");
    const hover = buildHover(doc, Position.create(14, idx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("TCTUALENG");
    expect(md.value).toContain("PIC");
  });

  it("does not suppress hover AFTER END-EXEC on same line", () => {
    // After END-EXEC, normal COBOL hover should resume
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 TCTUALENG PIC S9(4) COMP."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS RETURN END-EXEC."),
      fixedLine("    MOVE 1 TO TCTUALENG."),
    ]);
    const dl = doc.getText().split("\n");
    const line8 = dl[8];
    const idx = line8.indexOf("TCTUALENG");
    const hover = buildHover(doc, Position.create(8, idx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("TCTUALENG");
  });

  // ---- EXEC DLI hover suppression ----
  // Shared document for DLI hover tests
  function dliHoverDoc() {
    return makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),           // 0
      fixedLine("PROGRAM-ID. TEST1."),                 // 1
      fixedLine("DATA DIVISION."),                     // 2
      fixedLine("WORKING-STORAGE SECTION."),           // 3
      fixedLine("01 DLI-DATA."),                       // 4
      fixedLine("   03 SEGMENT   PIC X(8)."),          // 5
      fixedLine("   03 INTO      PIC X(80)."),         // 6
      fixedLine("   03 FROM      PIC X(80)."),         // 7
      fixedLine("   03 WS-AREA   PIC X(200)."),       // 8
      fixedLine("   03 WS-PCB    PIC S9(4) COMP."),   // 9
      fixedLine("PROCEDURE DIVISION."),                // 10
      fixedLine("MAIN-PARA."),                         // 11
      fixedLine("    EXEC DLI GU"),                    // 12
      fixedLine("         USING PCB(WS-PCB)"),         // 13
      fixedLine("         SEGMENT(SEGMENT)"),          // 14
      fixedLine("         INTO(WS-AREA)"),             // 15
      fixedLine("    END-EXEC."),                      // 16
      fixedLine("    DISPLAY SEGMENT."),               // 17
      fixedLine("    STOP RUN."),                      // 18
    ]);
  }

  it("suppresses hover for SEGMENT clause keyword on EXEC DLI continuation line", () => {
    const doc = dliHoverDoc();
    const dl = doc.getText().split("\n");
    // Line 14: SEGMENT(SEGMENT) — first SEGMENT is the clause keyword
    const line14 = dl[14];
    const segIdx = line14.indexOf("SEGMENT");
    const hover = buildHover(doc, Position.create(14, segIdx + 2), []);
    expect(hover).toBeUndefined();
  });

  it("shows data-item hover for SEGMENT inside parens in EXEC DLI", () => {
    const doc = dliHoverDoc();
    const dl = doc.getText().split("\n");
    // Line 14: SEGMENT(SEGMENT) — second SEGMENT is data reference in parens
    const line14 = dl[14];
    const firstIdx = line14.indexOf("SEGMENT");
    const dataIdx = line14.indexOf("SEGMENT", firstIdx + 7);
    const hover = buildHover(doc, Position.create(14, dataIdx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("SEGMENT");
    expect(md.value).toContain("PIC");
  });

  it("suppresses hover for INTO clause keyword in EXEC DLI", () => {
    const doc = dliHoverDoc();
    const dl = doc.getText().split("\n");
    // Line 15: INTO(WS-AREA) — INTO is a clause keyword
    const line15 = dl[15];
    const intoIdx = line15.indexOf("INTO");
    const hover = buildHover(doc, Position.create(15, intoIdx + 2), []);
    expect(hover).toBeUndefined();
  });

  it("shows data-item hover for WS-AREA inside parens in EXEC DLI", () => {
    const doc = dliHoverDoc();
    const dl = doc.getText().split("\n");
    const line15 = dl[15];
    const areaIdx = line15.indexOf("WS-AREA");
    const hover = buildHover(doc, Position.create(15, areaIdx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("WS-AREA");
    expect(md.value).toContain("PIC");
  });

  it("suppresses hover for USING clause keyword on EXEC DLI line", () => {
    const doc = dliHoverDoc();
    const dl = doc.getText().split("\n");
    const line13 = dl[13];
    const usingIdx = line13.indexOf("USING");
    const hover = buildHover(doc, Position.create(13, usingIdx + 2), []);
    expect(hover).toBeUndefined();
  });

  it("shows normal hover for SEGMENT OUTSIDE EXEC DLI block", () => {
    const doc = dliHoverDoc();
    const dl = doc.getText().split("\n");
    // Line 17: DISPLAY SEGMENT — outside any EXEC DLI block
    const line17 = dl[17];
    const segIdx = line17.indexOf("SEGMENT");
    const hover = buildHover(doc, Position.create(17, segIdx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("SEGMENT");
    expect(md.value).toContain("PIC");
  });

  it("does not suppress hover AFTER END-EXEC of DLI block", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 INTO PIC X(80)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI TERM END-EXEC."),
      fixedLine("    MOVE 1 TO INTO."),
    ]);
    const dl = doc.getText().split("\n");
    const line8 = dl[8];
    const intoIdx = line8.indexOf("INTO");
    const hover = buildHover(doc, Position.create(8, intoIdx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("INTO");
  });

  it("suppresses hover for DLI verb GU on same line as EXEC DLI", () => {
    // Define a data item named GU to test suppression
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 GU PIC X(10)."),
      fixedLine("01 WS-AREA PIC X(80)."),
      fixedLine("01 WS-PCB  PIC S9(4) COMP."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC DLI GU"),
      fixedLine("         USING PCB(WS-PCB)"),
      fixedLine("         SEGMENT(GU) INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);
    const dl = doc.getText().split("\n");
    // GU on line 9 (the verb) → should be suppressed
    const line9 = dl[9];
    const guIdx = line9.indexOf("GU");
    const hover = buildHover(doc, Position.create(9, guIdx), []);
    expect(hover).toBeUndefined();
  });

  // ---- EIB / DIB system field hover ----

  it("shows EIB hover for EIBCALEN (not defined in source)", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    IF EIBCALEN > 0"),
      fixedLine("       DISPLAY 'OK'"),
      fixedLine("    END-IF."),
    ]);
    const dl = doc.getText().split("\n");
    const line4 = dl[4];
    const idx = line4.indexOf("EIBCALEN");
    const hover = buildHover(doc, Position.create(4, idx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("EIB-Feld");
    expect(md.value).toContain("EIBCALEN");
    expect(md.value).toContain("S9(4) COMP");
    expect(md.value).toContain("COMMAREA");
  });

  it("shows EIB hover for EIBAID", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    IF EIBAID = DFHENTER"),
      fixedLine("       DISPLAY 'ENTER'"),
      fixedLine("    END-IF."),
    ]);
    const dl = doc.getText().split("\n");
    const line4 = dl[4];
    const idx = line4.indexOf("EIBAID");
    const hover = buildHover(doc, Position.create(4, idx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("EIB-Feld");
    expect(md.value).toContain("EIBAID");
    expect(md.value).toContain("X(1)");
    expect(md.value).toContain("Attention-ID");
  });

  it("shows EIB hover for EIBRESP", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    IF EIBRESP NOT = 0"),
      fixedLine("       DISPLAY 'ERROR'"),
      fixedLine("    END-IF."),
    ]);
    const dl = doc.getText().split("\n");
    const line4 = dl[4];
    const idx = line4.indexOf("EIBRESP");
    const hover = buildHover(doc, Position.create(4, idx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("EIB-Feld");
    expect(md.value).toContain("EIBRESP");
    expect(md.value).toContain("S9(8) COMP");
  });

  it("shows EIB hover for EIBTRNID and EIBTRMID", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY EIBTRNID."),
      fixedLine("    DISPLAY EIBTRMID."),
    ]);
    const dl = doc.getText().split("\n");

    const line4 = dl[4];
    const trnIdx = line4.indexOf("EIBTRNID");
    const hoverTrn = buildHover(doc, Position.create(4, trnIdx + 2), []);
    expect(hoverTrn).toBeDefined();
    const mdTrn = hoverTrn!.contents as { value: string };
    expect(mdTrn.value).toContain("EIBTRNID");
    expect(mdTrn.value).toContain("X(4)");

    const line5 = dl[5];
    const trmIdx = line5.indexOf("EIBTRMID");
    const hoverTrm = buildHover(doc, Position.create(5, trmIdx + 2), []);
    expect(hoverTrm).toBeDefined();
    const mdTrm = hoverTrm!.contents as { value: string };
    expect(mdTrm.value).toContain("EIBTRMID");
    expect(mdTrm.value).toContain("Terminal-ID");
  });

  it("shows EIB hover for EIBTIME and EIBDATE", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY EIBTIME."),
      fixedLine("    DISPLAY EIBDATE."),
    ]);
    const dl = doc.getText().split("\n");

    const line4 = dl[4];
    const timeIdx = line4.indexOf("EIBTIME");
    const hoverTime = buildHover(doc, Position.create(4, timeIdx + 2), []);
    expect(hoverTime).toBeDefined();
    const mdTime = hoverTime!.contents as { value: string };
    expect(mdTime.value).toContain("EIBTIME");
    expect(mdTime.value).toContain("S9(7) COMP-3");

    const line5 = dl[5];
    const dateIdx = line5.indexOf("EIBDATE");
    const hoverDate = buildHover(doc, Position.create(5, dateIdx + 2), []);
    expect(hoverDate).toBeDefined();
    const mdDate = hoverDate!.contents as { value: string };
    expect(mdDate.value).toContain("EIBDATE");
    expect(mdDate.value).toContain("Datum");
  });

  it("shows DIB hover for DIBSTAT", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    IF DIBSTAT NOT = SPACE"),
      fixedLine("       DISPLAY 'DLI ERROR'"),
      fixedLine("    END-IF."),
    ]);
    const dl = doc.getText().split("\n");
    const line4 = dl[4];
    const idx = line4.indexOf("DIBSTAT");
    const hover = buildHover(doc, Position.create(4, idx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("DIB-Feld");
    expect(md.value).toContain("DIBSTAT");
    expect(md.value).toContain("X(2)");
    expect(md.value).toContain("Statuscode");
  });

  it("shows DIB hover for DIBSEGM", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY DIBSEGM."),
    ]);
    const dl = doc.getText().split("\n");
    const line4 = dl[4];
    const idx = line4.indexOf("DIBSEGM");
    const hover = buildHover(doc, Position.create(4, idx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("DIB-Feld");
    expect(md.value).toContain("DIBSEGM");
    expect(md.value).toContain("Segment");
  });

  it("shows DIB hover for DIBSEGL and DIBKFBL", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY DIBSEGL."),
      fixedLine("    DISPLAY DIBKFBL."),
    ]);
    const dl = doc.getText().split("\n");

    const line4 = dl[4];
    const seglIdx = line4.indexOf("DIBSEGL");
    const hSegl = buildHover(doc, Position.create(4, seglIdx + 2), []);
    expect(hSegl).toBeDefined();

    const line5 = dl[5];
    const kfblIdx = line5.indexOf("DIBKFBL");
    const hKfbl = buildHover(doc, Position.create(5, kfblIdx + 2), []);
    expect(hKfbl).toBeDefined();
    const mdKfbl = hKfbl!.contents as { value: string };
    expect(mdKfbl.value).toContain("Key-Feedback");
  });

  it("prefers source-defined data item over EIB fallback", () => {
    // If EIBCALEN is explicitly in WORKING-STORAGE, show the real definition
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 EIBCALEN PIC S9(4) COMP."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    IF EIBCALEN > 0"),
      fixedLine("       DISPLAY 'OK'"),
      fixedLine("    END-IF."),
    ]);
    const dl = doc.getText().split("\n");
    const line7 = dl[7];
    const idx = line7.indexOf("EIBCALEN");
    const hover = buildHover(doc, Position.create(7, idx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    // Should show Level 01 data item definition, NOT the EIB-Feld
    expect(md.value).toContain("Level 01");
    expect(md.value).not.toContain("EIB-Feld");
  });

  it("shows EIB hover for EIBCPOSN", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    IF EIBCPOSN = 456"),
      fixedLine("       DISPLAY 'CURSOR HIT'"),
      fixedLine("    END-IF."),
    ]);
    const dl = doc.getText().split("\n");
    const line4 = dl[4];
    const idx = line4.indexOf("EIBCPOSN");
    const hover = buildHover(doc, Position.create(4, idx + 2), []);
    expect(hover).toBeDefined();
    const md = hover!.contents as { value: string };
    expect(md.value).toContain("EIB-Feld");
    expect(md.value).toContain("Cursor-Position");
  });

  it("does not show EIB/DIB hover for unrelated words", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
    ]);
    // DISPLAY is not an EIB/DIB field — should get keyword hover, not system field hover
    const dl = doc.getText().split("\n");
    const line4 = dl[4];
    const idx = line4.indexOf("DISPLAY");
    const hover = buildHover(doc, Position.create(4, idx + 2), []);
    // DISPLAY is a COBOL keyword, not in the definition index
    // and not an EIB/DIB field → undefined is fine
    expect(hover === undefined || !(hover!.contents as { value: string }).value.includes("EIB-Feld")).toBe(true);
  });
});
