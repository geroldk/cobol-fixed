/**
 * Tests for definition.ts — Go-to-Definition functionality.
 */
import { describe, it, expect } from "vitest";
import { wordAtPosition, buildDefinitionIndex, findSymbolDefinition, findSymbolInIndex, getQualifiers } from "./definition";
import { TextDocument } from "vscode-languageserver-textdocument";
import { Position } from "vscode-languageserver/node";
import { mapGenRange, PreBuilder } from "./preprocessor";
import { lintUndefinedIdentifiers } from "./lint";

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

  it("extracts INDEXED BY names as data items", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-TABLE."),
      fixedLine("   05 WS-ITEM OCCURS 10 INDEXED BY X-IDX."),
    ]);

    const index = buildDefinitionIndex(doc);
    const names = index.dataItems.map((d) => d.name);
    expect(names).toContain("X-IDX");
  });

  it("extracts multiple INDEXED BY names on one line", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-TABLE."),
      fixedLine("   05 WS-ITEM OCCURS 25 INDEXED BY X-VT X-VT2."),
    ]);

    const index = buildDefinitionIndex(doc);
    const names = index.dataItems.map((d) => d.name);
    expect(names).toContain("X-VT");
    expect(names).toContain("X-VT2");
  });

  it("extracts INDEXED BY names before PIC clause", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-TABLE."),
      fixedLine("   06 OWKN OCCURS 15 INDEXED BY X-OWKN X-OWKN2"),
      fixedLine("       X-OWKN3 PIC X(6)."),
    ]);

    const index = buildDefinitionIndex(doc);
    const names = index.dataItems.map((d) => d.name);
    expect(names).toContain("X-OWKN");
    expect(names).toContain("X-OWKN2");
    expect(names).toContain("X-OWKN3");
  });

  it("INDEXED BY names have usage INDEX", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-TABLE."),
      fixedLine("   05 WS-ITEM OCCURS 10 INDEXED BY X-IDX."),
    ]);

    const index = buildDefinitionIndex(doc);
    const idxItem = index.dataItems.find((d) => d.name === "X-IDX");
    expect(idxItem).toBeDefined();
    expect(idxItem!.usage).toBe("INDEX");
  });

  it("extracts FD file names as data items", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("FILE SECTION."),
      fixedLine("FD GAARCH."),
      fixedLine("01 VA-SATZ."),
      fixedLine("   05 VA-KEY PIC X(6)."),
    ]);

    const index = buildDefinitionIndex(doc);
    const fd = index.dataItems.find((d) => d.name === "GAARCH");
    expect(fd).toBeDefined();
    expect(fd!.level).toBe(0);
  });

  it("extracts SELECT file names from ENVIRONMENT DIVISION", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("ENVIRONMENT DIVISION."),
      fixedLine("INPUT-OUTPUT SECTION."),
      fixedLine("FILE-CONTROL."),
      fixedLine("    SELECT GAARCH ASSIGN TO GAARCH."),
      fixedLine("DATA DIVISION."),
    ]);

    const index = buildDefinitionIndex(doc);
    const sel = index.dataItems.find((d) => d.name === "GAARCH");
    expect(sel).toBeDefined();
  });

  it("extracts INDEXED BY on continuation line without level number", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-TABLE."),
      fixedLine("   10 MW-TAB OCCURS 50 DEPENDING ON MW-ANZ"),
      fixedLine("                          INDEXED BY X-MW."),
    ]);

    const index = buildDefinitionIndex(doc);
    const names = index.dataItems.map((d) => d.name);
    expect(names).toContain("X-MW");
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

describe("getQualifiers", () => {
  it("extracts qualifiers with OF and IN", () => {
    const doc = makeDoc([
      fixedLine("    MOVE WS-FIELD-C OF WS-FIELD-B IN WS-RECORD TO X."),
    ]);
    const lineText = doc.getText().split("\n")[0];
    const wordIdx = lineText.indexOf("WS-FIELD-C");
    const wordEnd = wordIdx + "WS-FIELD-C".length;
    const quals = getQualifiers(doc, 0, wordEnd);
    expect(quals).toEqual(["WS-FIELD-B", "WS-RECORD"]);
  });
});

describe("findSymbolInIndex with qualifiers", () => {
  it("resolves qualified data names", () => {
    const doc = makeDoc([
      fixedLine("DATA DIVISION."),
      fixedLine("01 REC-A."),
      fixedLine("   05 FIELD-1 PIC X."),
      fixedLine("01 REC-B."),
      fixedLine("   05 FIELD-1 PIC X."),
    ]);
    const index = buildDefinitionIndex(doc);
    const symA = findSymbolInIndex("FIELD-1", index, ["REC-A"]);
    expect(symA).toBeDefined();
    expect(symA!.line).toBe(2);

    const symB = findSymbolInIndex("FIELD-1", index, ["REC-B"]);
    expect(symB).toBeDefined();
    expect(symB!.line).toBe(4);
  });
});

// ---- End-to-end offset test: lintUndefinedIdentifiers + mapGenRange ----

describe("lintUndefinedIdentifiers offset mapping", () => {
  function buildPreprocessed(sourceLines: string[], uri = "file:///test.cob") {
    const builder = new PreBuilder();
    for (let i = 0; i < sourceLines.length; i++) {
      const line = sourceLines[i];
      const line72 = line.slice(0, 72);
      // Replace seq area (cols 1-6) with spaces but keep indicator + lang
      const normalized = " ".repeat(6) + line72.slice(6);
      builder.appendSourceLine(uri, i, 0, normalized, line72.length);
    }
    return builder.build();
  }

  it("maps offset of undefined identifier back to correct source position", () => {
    const sourceLines = [
      "000100 IDENTIFICATION DIVISION.                                           ",
      "000200 PROGRAM-ID. TEST1.                                                 ",
      "000300 DATA DIVISION.                                                     ",
      "000400 WORKING-STORAGE SECTION.                                           ",
      "000500 01  WS-FLAG PIC X.                                                 ",
      "000600 PROCEDURE DIVISION.                                                ",
      "000700     IF VK-UPDATERR                                                 ",
      "000800         DISPLAY WS-FLAG                                            ",
      "000900     END-IF.                                                        ",
    ];

    const pre = buildPreprocessed(sourceLines);
    const preDoc = TextDocument.create("file:///test.cob", "cobol85", 1, pre.text);
    const index = buildDefinitionIndex(preDoc);
    const diags = lintUndefinedIdentifiers(pre.text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");

    expect(undef).toHaveLength(1);
    expect(undef[0].message).toContain("VK-UPDATERR");

    // Map back to source
    const mapped = mapGenRange(pre, undef[0].startOff, undef[0].endOff);
    expect(mapped).toBeDefined();
    expect(mapped!.range.start.line).toBe(6); // line 7 (0-based index 6)

    // Verify character position: VK-UPDATERR starts at column 12 (0-based: 11)
    const srcLine = sourceLines[6];
    const vkIdx = srcLine.indexOf("VK-UPDATERR");
    expect(vkIdx).toBeGreaterThan(0);
    expect(mapped!.range.start.character).toBe(vkIdx);
    expect(mapped!.range.end.character).toBe(vkIdx + "VK-UPDATERR".length);
  });

  it("maps offset correctly after COPY expansion (inserted lines)", () => {
    // Simulate: main file has COPY at line 5.
    // The copy book inserts 200 lines of data definitions.
    // After the COPY, the main file continues with PROCEDURE DIVISION.
    // VK-UPDATERR should map back to correct source position despite COPY expansion.
    const mainUri = "file:///main.cob";
    const copyUri = "file:///copybook.cob";
    const builder = new PreBuilder();

    // Lines 0-4: main file preamble
    const mainLines = [
      "000100 IDENTIFICATION DIVISION.                                           ",
      "000200 PROGRAM-ID. TEST1.                                                 ",
      "000300 DATA DIVISION.                                                     ",
      "000400 WORKING-STORAGE SECTION.                                           ",
      "000500 01  WS-FLAG PIC X.                                                 ",
    ];
    for (let i = 0; i < mainLines.length; i++) {
      const line = mainLines[i];
      const normalized = " ".repeat(6) + line.slice(6, 72);
      builder.appendSourceLine(mainUri, i, 0, normalized, Math.min(line.length, 72));
    }

    // Line 5: COPY statement — neutralized (empty content)
    builder.appendSourceLine(mainUri, 5, 0, "", 72);

    // Simulate COPY book expansion: 200 lines of data definitions
    const copyBuilder = new PreBuilder();
    for (let k = 0; k < 200; k++) {
      const dataLine = `      ${String(k + 1).padStart(2, "0")} COPY-FIELD-${String(k).padStart(3, "0")} PIC X.`.padEnd(72, " ");
      const norm = " ".repeat(6) + " " + dataLine.slice(7);
      copyBuilder.appendSourceLine(copyUri, k, 0, norm, 72);
    }
    const copyPre = copyBuilder.build();
    builder.appendPreprocessed(copyPre);

    // Lines 6-10: main file after COPY — PROCEDURE DIVISION
    const postCopyLines = [
      "000700 PROCEDURE DIVISION.                                                ",
      "000800     PERFORM MAIN-PARA.                                             ",
      "000900 MAIN-PARA.                                                         ",
      "001000     IF VK-UPDATERR                                                 ",
      "001100         DISPLAY WS-FLAG                                            ",
      "001200     END-IF.                                                        ",
    ];
    for (let i = 0; i < postCopyLines.length; i++) {
      const line = postCopyLines[i];
      const normalized = " ".repeat(6) + line.slice(6, 72);
      builder.appendSourceLine(mainUri, 6 + i, 0, normalized, Math.min(line.length, 72));
    }

    const pre = builder.build();
    const preDoc = TextDocument.create(mainUri, "cobol85", 1, pre.text);
    const index = buildDefinitionIndex(preDoc);
    const diags = lintUndefinedIdentifiers(pre.text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");

    // Should flag VK-UPDATERR
    const vkDiag = undef.find((d) => d.message.includes("VK-UPDATERR"));
    expect(vkDiag).toBeDefined();

    // Map back to source
    const mapped = mapGenRange(pre, vkDiag!.startOff, vkDiag!.endOff);
    expect(mapped).toBeDefined();
    expect(mapped!.uri).toBe(mainUri);
    expect(mapped!.range.start.line).toBe(9); // main file source line 9

    // VK-UPDATERR column in the source line
    const srcLine = postCopyLines[3]; // line index 3 in postCopyLines = source line 9
    const vkIdx = srcLine.indexOf("VK-UPDATERR");
    expect(vkIdx).toBeGreaterThan(0);
    expect(mapped!.range.start.character).toBe(vkIdx);
    expect(mapped!.range.end.character).toBe(vkIdx + "VK-UPDATERR".length);
  });
});
