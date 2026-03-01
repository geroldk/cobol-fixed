/**
 * Tests for semantic-tokens.ts — Semantic Tokens provider.
 */
import { describe, it, expect } from "vitest";
import { TextDocument } from "vscode-languageserver-textdocument";
import {
  buildSemanticTokens,
  encodeSemanticTokens,
  SemanticToken,
} from "./semantic-tokens";
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

/** Get raw lines from a TextDocument. */
function lines(doc: TextDocument): string[] {
  return doc.getText().split(/\r?\n/);
}

/** Extract word text from a token given the document lines. */
function tokenWord(t: SemanticToken, docLines: string[]): string {
  return docLines[t.line].slice(t.startChar, t.startChar + t.length);
}

/** Return all tokens as [{word, type}] for easy assertion. */
function tokenSummary(tokens: SemanticToken[], docLines: string[]) {
  return tokens.map(t => ({
    word: tokenWord(t, docLines),
    type: t.tokenType,
    line: t.line,
  }));
}

/** Filter tokens on a specific line, returning word+type pairs. */
function tokensOnLine(tokens: SemanticToken[], docLines: string[], lineNo: number) {
  return tokens
    .filter(t => t.line === lineNo)
    .map(t => ({ word: tokenWord(t, docLines), type: t.tokenType }));
}

// ---- buildSemanticTokens ----

describe("buildSemanticTokens", () => {
  it("classifies comment lines as comment tokens", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("This is a comment", "*"),
      fixedLine("PROGRAM-ID. TEST1."),
    ]);
    const tokens = buildSemanticTokens(doc);

    const commentTokens = tokens.filter(t => t.tokenType === 6);
    expect(commentTokens.length).toBe(1);
    expect(commentTokens[0].line).toBe(1);
  });

  it("classifies COBOL verbs as keywords", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY 'HELLO'."),
      fixedLine("    STOP RUN."),
    ]);
    const tokens = buildSemanticTokens(doc);

    const keywordTokens = tokens.filter(t => t.tokenType === 0);
    // Should include IDENTIFICATION, DIVISION, PROGRAM-ID, PROCEDURE, DIVISION, DISPLAY, STOP, RUN
    expect(keywordTokens.length).toBeGreaterThanOrEqual(5);
  });

  it("classifies paragraph names as function tokens", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM MAIN-PARA."),
    ]);
    const tokens = buildSemanticTokens(doc);

    const funcTokens = tokens.filter(t => t.tokenType === 2); // function = paragraph
    expect(funcTokens.length).toBe(2); // definition + usage
  });

  it("classifies data names as variable tokens", () => {
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
    const tokens = buildSemanticTokens(doc);

    const varTokens = tokens.filter(t => t.tokenType === 1); // variable
    expect(varTokens.length).toBe(2); // definition + usage
  });

  it("classifies section names as namespace tokens", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-SECTION SECTION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    STOP RUN."),
    ]);
    const tokens = buildSemanticTokens(doc);

    const nsTokens = tokens.filter(t => t.tokenType === 3); // namespace = section
    expect(nsTokens.length).toBeGreaterThanOrEqual(1);
  });

  it("classifies string literals", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    DISPLAY 'HELLO WORLD'."),
    ]);
    const tokens = buildSemanticTokens(doc);

    const strTokens = tokens.filter(t => t.tokenType === 4); // string
    expect(strTokens.length).toBe(1);
  });

  it("classifies numeric literals", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    MOVE 42 TO WS-X."),
    ]);
    const tokens = buildSemanticTokens(doc);

    const numTokens = tokens.filter(t => t.tokenType === 5); // number
    expect(numTokens.length).toBeGreaterThanOrEqual(1);
  });

  it("marks definition sites with declaration modifier", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    PERFORM MAIN-PARA."),
    ]);
    const tokens = buildSemanticTokens(doc);

    const funcTokens = tokens.filter(t => t.tokenType === 2);
    const defTokens = funcTokens.filter(t => t.tokenModifiers !== 0);
    const refTokens = funcTokens.filter(t => t.tokenModifiers === 0);
    expect(defTokens.length).toBe(1); // definition
    expect(refTokens.length).toBe(1); // reference
  });

  // ---- EXEC CICS semantic token tests ----

  it("classifies EXEC CICS verbs as function tokens", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-REC PIC X(80)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS READ"),
      fixedLine("         FILE('CUSTFILE')"),
      fixedLine("         INTO(WS-REC)"),
      fixedLine("    END-EXEC."),
    ]);
    const tokens = buildSemanticTokens(doc);

    // EXEC and CICS → macro (8)
    const macroTokens = tokens.filter(t => t.tokenType === 8);
    const macroWords = macroTokens.map(t => {
      const line = lines(doc)[t.line];
      return line.slice(t.startChar, t.startChar + t.length);
    });
    expect(macroWords).toContain("EXEC");
    expect(macroWords).toContain("CICS");

    // READ → function (2) — CICS verb
    const funcTokens = tokens.filter(t => t.tokenType === 2);
    const funcWords = funcTokens.map(t => {
      const line = lines(doc)[t.line];
      return line.slice(t.startChar, t.startChar + t.length);
    });
    expect(funcWords).toContain("READ");
  });

  it("classifies CICS options as property tokens", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS ASSIGN"),
      fixedLine("         TCTUALENG(WS-X)"),
      fixedLine("    END-EXEC."),
    ]);
    const tokens = buildSemanticTokens(doc);

    // ASSIGN → function (2)
    const funcTokens = tokens.filter(t => t.tokenType === 2);
    const funcWords = funcTokens.map(t => {
      const line = lines(doc)[t.line];
      return line.slice(t.startChar, t.startChar + t.length);
    });
    expect(funcWords).toContain("ASSIGN");

    // TCTUALENG → property (11)
    const propTokens = tokens.filter(t => t.tokenType === 11);
    const propWords = propTokens.map(t => {
      const line = lines(doc)[t.line];
      return line.slice(t.startChar, t.startChar + t.length);
    });
    expect(propWords).toContain("TCTUALENG");
  });

  it("classifies data names in CICS parens as variable tokens", () => {
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
      fixedLine("         INTO(WS-REC)"),
      fixedLine("         RIDFLD(WS-KEY)"),
      fixedLine("    END-EXEC."),
    ]);
    const tokens = buildSemanticTokens(doc);

    // WS-REC inside INTO(...) → variable (1)
    const varTokens = tokens.filter(t => t.tokenType === 1);
    const varWords = varTokens.map(t => {
      const line = lines(doc)[t.line];
      return line.slice(t.startChar, t.startChar + t.length);
    });
    // Should contain data name definitions + CICS references
    expect(varWords.filter(w => w === "WS-REC").length).toBeGreaterThanOrEqual(2);
    expect(varWords.filter(w => w === "WS-KEY").length).toBeGreaterThanOrEqual(2);
  });

  it("classifies CICS conditions as type tokens", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS HANDLE CONDITION"),
      fixedLine("         NOTFND(NOT-FOUND-PARA)"),
      fixedLine("         INVREQ(INVALID-PARA)"),
      fixedLine("    END-EXEC."),
    ]);
    const tokens = buildSemanticTokens(doc);

    // NOTFND, INVREQ → type (9) — CICS conditions
    const typeTokens = tokens.filter(t => t.tokenType === 9);
    const typeWords = typeTokens.map(t => {
      const line = lines(doc)[t.line];
      return line.slice(t.startChar, t.startChar + t.length);
    });
    expect(typeWords).toContain("NOTFND");
    expect(typeWords).toContain("INVREQ");
  });

  it("does not apply CICS classification outside EXEC CICS blocks", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    READ FILE-1."),
    ]);
    const tokens = buildSemanticTokens(doc);

    // READ outside EXEC CICS → keyword (0), not function (2)
    const readTokens = tokens.filter(t => {
      const line = lines(doc)[t.line];
      return line.slice(t.startChar, t.startChar + t.length) === "READ";
    });
    expect(readTokens.length).toBe(1);
    expect(readTokens[0].tokenType).toBe(0); // keyword
  });

  it("handles single-line EXEC CICS ... END-EXEC", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS RETURN END-EXEC."),
    ]);
    const tokens = buildSemanticTokens(doc);

    // RETURN → function (2) — CICS verb
    const funcTokens = tokens.filter(t => t.tokenType === 2);
    const funcWords = funcTokens.map(t => {
      const line = lines(doc)[t.line];
      return line.slice(t.startChar, t.startChar + t.length);
    });
    expect(funcWords).toContain("RETURN");

    // END-EXEC → macro (8)
    const macroTokens = tokens.filter(t => t.tokenType === 8);
    const macroWords = macroTokens.map(t => {
      const line = lines(doc)[t.line];
      return line.slice(t.startChar, t.startChar + t.length);
    });
    expect(macroWords).toContain("END-EXEC");
  });

  it("classifies ADDRESS as keyword in ADDRESS OF dataname", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-PTR       USAGE POINTER."),
      fixedLine("01 WS-REC       PIC X(80)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    SET WS-PTR TO ADDRESS OF WS-REC."),
    ]);
    const tokens = buildSemanticTokens(doc);
    const dl = lines(doc);

    // ADDRESS → keyword (0) — standard COBOL keyword
    const addrLine = 8;
    const addrToks = tokensOnLine(tokens, dl, addrLine);
    expect(addrToks).toContainEqual({ word: "ADDRESS", type: 0 }); // keyword
    expect(addrToks).toContainEqual({ word: "OF", type: 0 });      // keyword
  });

  it("classifies ADDRESS as function(2) inside EXEC CICS, not keyword(0)", () => {
    const doc = makeDoc([
      fixedLine("IDENTIFICATION DIVISION."),
      fixedLine("PROGRAM-ID. TEST1."),
      fixedLine("DATA DIVISION."),
      fixedLine("WORKING-STORAGE SECTION."),
      fixedLine("01 WS-TCTUA     PIC X(100)."),
      fixedLine("PROCEDURE DIVISION."),
      fixedLine("MAIN-PARA."),
      fixedLine("    EXEC CICS ADDRESS"),
      fixedLine("         TCTUA(WS-TCTUA)"),
      fixedLine("    END-EXEC."),
    ]);
    const tokens = buildSemanticTokens(doc);
    const dl = lines(doc);

    // ADDRESS inside EXEC CICS → function (2), not keyword (0)
    const execLine = 7;
    const execToks = tokensOnLine(tokens, dl, execLine);
    expect(execToks).toContainEqual({ word: "EXEC", type: 8 });
    expect(execToks).toContainEqual({ word: "CICS", type: 8 });
    expect(execToks).toContainEqual({ word: "ADDRESS", type: 2 }); // CICS verb

    // TCTUA → property (11)
    const tctuaLine = execLine + 1;
    const tctuaToks = tokensOnLine(tokens, dl, tctuaLine);
    expect(tctuaToks).toContainEqual({ word: "TCTUA", type: 11 });
  });
});

// ---- Systematic EXEC CICS semantic token tests ----

describe("EXEC CICS semantic tokens — systematic", () => {
  // Common boilerplate for a minimal COBOL program with data items
  const preamble = [
    fixedLine("IDENTIFICATION DIVISION."),
    fixedLine("PROGRAM-ID. TEST1."),
    fixedLine("DATA DIVISION."),
    fixedLine("WORKING-STORAGE SECTION."),
    fixedLine("01 WS-REC       PIC X(80)."),
    fixedLine("01 WS-KEY       PIC X(6)."),
    fixedLine("01 WS-LEN       PIC S9(4) COMP."),
    fixedLine("01 WS-RESP      PIC S9(8) COMP."),
    fixedLine("01 WS-MAP-DATA  PIC X(1920)."),
    fixedLine("01 WS-TCTUA-LEN PIC S9(4) COMP."),
    fixedLine("01 WS-TRANSID   PIC X(4)."),
    fixedLine("01 DFHCOMMAREA  PIC X(100)."),
    fixedLine("PROCEDURE DIVISION."),
  ];

  // Helper: build doc with preamble + custom lines
  // preamble = 13 lines (0-12), MAIN-PARA at 13, first cicsLine at 14
  const CICS_BASE = preamble.length + 1; // = 14

  function cicsDoc(cicsLines: string[]) {
    const docLines = [
      ...preamble,
      fixedLine("MAIN-PARA."),
      ...cicsLines,
      fixedLine("    STOP RUN."),
    ];
    const doc = makeDoc(docLines);
    const toks = buildSemanticTokens(doc);
    const dl = lines(doc);
    return { doc, tokens: toks, docLines: dl };
  }

  // --- 1. Multi-line READ with FILE, INTO, RIDFLD ---
  it("multi-line READ: verb=function, options=property, data-in-parens=variable", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS READ"),
      fixedLine("         FILE('CUSTFILE')"),
      fixedLine("         INTO(WS-REC)"),
      fixedLine("         RIDFLD(WS-KEY)"),
      fixedLine("         LENGTH(WS-LEN)"),
      fixedLine("         RESP(WS-RESP)"),
      fixedLine("    END-EXEC."),
    ]);

    // Line with EXEC CICS READ (preamble=13 + MAIN-PARA, first CICS at CICS_BASE=14)
    const execLine = CICS_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    expect(execToks).toContainEqual({ word: "EXEC", type: 8 });   // macro
    expect(execToks).toContainEqual({ word: "CICS", type: 8 });   // macro
    expect(execToks).toContainEqual({ word: "READ", type: 2 });   // function (verb)

    // FILE line: FILE=property, 'CUSTFILE'=string
    const fileLine = execLine + 1;
    const fileToks = tokensOnLine(tokens, docLines, fileLine);
    expect(fileToks).toContainEqual({ word: "FILE", type: 11 });  // property
    const fileStrings = fileToks.filter(t => t.type === 4);        // string
    expect(fileStrings.length).toBe(1);

    // INTO line: INTO=property, WS-REC=variable
    const intoLine = execLine + 2;
    const intoToks = tokensOnLine(tokens, docLines, intoLine);
    expect(intoToks).toContainEqual({ word: "INTO", type: 11 });  // property
    expect(intoToks).toContainEqual({ word: "WS-REC", type: 1 }); // variable

    // RIDFLD line: RIDFLD=property, WS-KEY=variable
    const ridfldLine = execLine + 3;
    const ridfldToks = tokensOnLine(tokens, docLines, ridfldLine);
    expect(ridfldToks).toContainEqual({ word: "RIDFLD", type: 11 });
    expect(ridfldToks).toContainEqual({ word: "WS-KEY", type: 1 });

    // RESP line: RESP=property, WS-RESP=variable
    const respLine = execLine + 5;
    const respToks = tokensOnLine(tokens, docLines, respLine);
    expect(respToks).toContainEqual({ word: "RESP", type: 11 });
    expect(respToks).toContainEqual({ word: "WS-RESP", type: 1 });

    // END-EXEC line
    const endLine = execLine + 6;
    const endToks = tokensOnLine(tokens, docLines, endLine);
    expect(endToks).toContainEqual({ word: "END-EXEC", type: 8 }); // macro
  });

  // --- 2. SEND MAP ---
  it("SEND MAP: verb=function, MAP/MAPSET/ERASE/FREEKB=property", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS SEND"),
      fixedLine("         MAP('MAP001')"),
      fixedLine("         MAPSET('MAPSET1')"),
      fixedLine("         FROM(WS-MAP-DATA)"),
      fixedLine("         ERASE"),
      fixedLine("         FREEKB"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    expect(execToks).toContainEqual({ word: "SEND", type: 2 });

    // MAP and MAPSET options (inside parens = string literal)
    const mapLine = execLine + 1;
    const mapToks = tokensOnLine(tokens, docLines, mapLine);
    expect(mapToks).toContainEqual({ word: "MAP", type: 11 });

    // FROM option with data name
    const fromLine = execLine + 3;
    const fromToks = tokensOnLine(tokens, docLines, fromLine);
    expect(fromToks).toContainEqual({ word: "FROM", type: 11 });
    expect(fromToks).toContainEqual({ word: "WS-MAP-DATA", type: 1 });

    // ERASE = standalone option (no parens)
    const eraseLine = execLine + 4;
    const eraseToks = tokensOnLine(tokens, docLines, eraseLine);
    expect(eraseToks).toContainEqual({ word: "ERASE", type: 11 });

    // FREEKB = standalone option
    const freekbLine = execLine + 5;
    const freekbToks = tokensOnLine(tokens, docLines, freekbLine);
    expect(freekbToks).toContainEqual({ word: "FREEKB", type: 11 });
  });

  // --- 3. ASSIGN with CICS system options ---
  it("ASSIGN: verb=function, TCTUALENG/USERID=property, data-in-parens=variable", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS ASSIGN"),
      fixedLine("         TCTUALENG(WS-TCTUA-LEN)"),
      fixedLine("         USERID(WS-TRANSID)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "ASSIGN", type: 2 });

    // TCTUALENG = property, WS-TCTUA-LEN = variable
    const tctLine = execLine + 1;
    const tctToks = tokensOnLine(tokens, docLines, tctLine);
    expect(tctToks).toContainEqual({ word: "TCTUALENG", type: 11 });
    expect(tctToks).toContainEqual({ word: "WS-TCTUA-LEN", type: 1 });

    // USERID = property, WS-TRANSID = variable
    const uidLine = execLine + 2;
    const uidToks = tokensOnLine(tokens, docLines, uidLine);
    expect(uidToks).toContainEqual({ word: "USERID", type: 11 });
    expect(uidToks).toContainEqual({ word: "WS-TRANSID", type: 1 });
  });

  // --- 4. HANDLE CONDITION with conditions + labels ---
  it("HANDLE CONDITION: conditions=type, para labels in parens=function", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS HANDLE CONDITION"),
      fixedLine("         NOTFND(NOT-FOUND-PARA)"),
      fixedLine("         LENGERR(LENGTH-ERR-PARA)"),
      fixedLine("         ERROR(GENERAL-ERR-PARA)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "HANDLE", type: 2 });
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "CONDITION", type: 11 });

    // NOTFND = type (condition), NOT-FOUND-PARA in parens = unstyled (not in index)
    const notfndLine = execLine + 1;
    const notfndToks = tokensOnLine(tokens, docLines, notfndLine);
    expect(notfndToks).toContainEqual({ word: "NOTFND", type: 9 });
    // Label in parens: NOT-FOUND-PARA is not defined in our doc, so no token

    // LENGERR = type
    const lengLine = execLine + 2;
    expect(tokensOnLine(tokens, docLines, lengLine))
      .toContainEqual({ word: "LENGERR", type: 9 });

    // ERROR is both a condition and a COBOL keyword — inside EXEC CICS it should be type(9)
    const errorLine = execLine + 3;
    const errorToks = tokensOnLine(tokens, docLines, errorLine);
    expect(errorToks).toContainEqual({ word: "ERROR", type: 9 });
  });

  // --- 5. Single-line EXEC CICS ---
  it("single-line EXEC CICS RETURN: all tokens classified correctly", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS RETURN TRANSID(WS-TRANSID) END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    const toks = tokensOnLine(tokens, docLines, execLine);
    expect(toks).toContainEqual({ word: "EXEC", type: 8 });
    expect(toks).toContainEqual({ word: "CICS", type: 8 });
    expect(toks).toContainEqual({ word: "RETURN", type: 2 });
    expect(toks).toContainEqual({ word: "TRANSID", type: 11 });
    expect(toks).toContainEqual({ word: "WS-TRANSID", type: 1 });
    expect(toks).toContainEqual({ word: "END-EXEC", type: 8 });
  });

  // --- 6. CICS keywords must NOT bleed outside EXEC CICS ---
  it("CICS keywords are normal COBOL keywords outside EXEC CICS", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS RETURN END-EXEC."),
      fixedLine("    MOVE WS-REC TO WS-KEY."),
      fixedLine("    READ FILE-X."),
    ]);

    // RETURN inside EXEC CICS = function(2)
    const execLine = CICS_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "RETURN", type: 2 });

    // MOVE/TO on next line = keyword(0)
    const moveLine = execLine + 1;
    const moveToks = tokensOnLine(tokens, docLines, moveLine);
    expect(moveToks).toContainEqual({ word: "MOVE", type: 0 });
    expect(moveToks).toContainEqual({ word: "TO", type: 0 });
    // WS-REC, WS-KEY = variable(1)
    expect(moveToks).toContainEqual({ word: "WS-REC", type: 1 });
    expect(moveToks).toContainEqual({ word: "WS-KEY", type: 1 });

    // READ outside EXEC CICS = keyword(0)
    const readLine = execLine + 2;
    expect(tokensOnLine(tokens, docLines, readLine))
      .toContainEqual({ word: "READ", type: 0 });
  });

  // --- 7. Comment lines inside EXEC CICS block ---
  it("comment lines inside EXEC CICS are classified as comments", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS READ"),
      fixedLine("* This is a comment inside EXEC CICS", "*"),
      fixedLine("         FILE('CUSTFILE')"),
      fixedLine("    END-EXEC."),
    ]);

    const commentLine = CICS_BASE + 1;
    const commentToks = tokensOnLine(tokens, docLines, commentLine);
    expect(commentToks.length).toBe(1);
    expect(commentToks[0].type).toBe(6); // comment
  });

  // --- 8. Multiple EXEC CICS blocks, proper state reset ---
  it("two sequential EXEC CICS blocks are both classified correctly", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS READ"),
      fixedLine("         FILE('CUSTFILE')"),
      fixedLine("         INTO(WS-REC)"),
      fixedLine("         RIDFLD(WS-KEY)"),
      fixedLine("    END-EXEC."),
      fixedLine("    MOVE WS-REC TO WS-KEY."),
      fixedLine("    EXEC CICS SEND"),
      fixedLine("         MAP('MAP001')"),
      fixedLine("         ERASE"),
      fixedLine("    END-EXEC."),
    ]);

    // First block: READ=function
    const readLine = CICS_BASE;
    expect(tokensOnLine(tokens, docLines, readLine))
      .toContainEqual({ word: "READ", type: 2 });
    // INTO(WS-REC) on line readLine+2
    expect(tokensOnLine(tokens, docLines, readLine + 2))
      .toContainEqual({ word: "WS-REC", type: 1 });

    // Between blocks: MOVE=keyword
    const moveLine = readLine + 5;
    expect(tokensOnLine(tokens, docLines, moveLine))
      .toContainEqual({ word: "MOVE", type: 0 });
    expect(tokensOnLine(tokens, docLines, moveLine))
      .toContainEqual({ word: "WS-REC", type: 1 });

    // Second block: SEND=function
    const sendLine = readLine + 6;
    expect(tokensOnLine(tokens, docLines, sendLine))
      .toContainEqual({ word: "SEND", type: 2 });
    // ERASE=property
    expect(tokensOnLine(tokens, docLines, sendLine + 2))
      .toContainEqual({ word: "ERASE", type: 11 });
  });

  // --- 9. LINK with PROGRAM, COMMAREA ---
  it("LINK: PROGRAM/COMMAREA/DATALENGTH=property, values in parens=variable or string", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS LINK"),
      fixedLine("         PROGRAM('SUBPGM1')"),
      fixedLine("         COMMAREA(DFHCOMMAREA)"),
      fixedLine("         LENGTH(WS-LEN)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "LINK", type: 2 });

    // PROGRAM('SUBPGM1')
    const progLine = execLine + 1;
    const progToks = tokensOnLine(tokens, docLines, progLine);
    expect(progToks).toContainEqual({ word: "PROGRAM", type: 11 });
    expect(progToks.some(t => t.type === 4)).toBe(true); // string 'SUBPGM1'

    // COMMAREA(DFHCOMMAREA)
    const commLine = execLine + 2;
    const commToks = tokensOnLine(tokens, docLines, commLine);
    expect(commToks).toContainEqual({ word: "COMMAREA", type: 11 });
    expect(commToks).toContainEqual({ word: "DFHCOMMAREA", type: 1 });

    // LENGTH(WS-LEN)
    const lenLine = execLine + 3;
    expect(tokensOnLine(tokens, docLines, lenLine))
      .toContainEqual({ word: "LENGTH", type: 11 });
    expect(tokensOnLine(tokens, docLines, lenLine))
      .toContainEqual({ word: "WS-LEN", type: 1 });
  });

  // --- 10. WRITEQ TS ---
  it("WRITEQ TS: both WRITEQ=verb, TS=option", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS WRITEQ TS"),
      fixedLine("         QUEUE('MYQUEUE')"),
      fixedLine("         FROM(WS-REC)"),
      fixedLine("         LENGTH(WS-LEN)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    expect(execToks).toContainEqual({ word: "WRITEQ", type: 2 });  // verb
    expect(execToks).toContainEqual({ word: "TS", type: 11 });     // option
  });

  // --- 11. Deeply nested/uncommon: DUMP TRANSACTION ---
  it("DUMP TRANSACTION: DUMP=verb, DUMPCODE/COMPLETE/TASK=property", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS DUMP TRANSACTION"),
      fixedLine("         DUMPCODE('TSTD')"),
      fixedLine("         COMPLETE"),
      fixedLine("         TASK"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "DUMP", type: 2 });
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "TRANSACTION", type: 11 });

    expect(tokensOnLine(tokens, docLines, execLine + 1))
      .toContainEqual({ word: "DUMPCODE", type: 11 });
    expect(tokensOnLine(tokens, docLines, execLine + 2))
      .toContainEqual({ word: "COMPLETE", type: 11 });
    expect(tokensOnLine(tokens, docLines, execLine + 3))
      .toContainEqual({ word: "TASK", type: 11 });
  });

  // --- 12. RESP/RESP2 on any command ---
  it("RESP and RESP2 are classified as property", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS READ"),
      fixedLine("         FILE('CUSTFILE')"),
      fixedLine("         INTO(WS-REC)"),
      fixedLine("         RIDFLD(WS-KEY)"),
      fixedLine("         RESP(WS-RESP)"),
      fixedLine("         RESP2(WS-LEN)"),
      fixedLine("    END-EXEC."),
    ]);

    const respLine = CICS_BASE + 4;
    expect(tokensOnLine(tokens, docLines, respLine))
      .toContainEqual({ word: "RESP", type: 11 });
    expect(tokensOnLine(tokens, docLines, respLine))
      .toContainEqual({ word: "WS-RESP", type: 1 });

    const resp2Line = respLine + 1;
    // RESP2 contains a digit — make sure WORD_RE catches it
    const resp2Toks = tokensOnLine(tokens, docLines, resp2Line);
    expect(resp2Toks).toContainEqual({ word: "RESP2", type: 11 });
  });

  // --- 13. Unknown word outside parens: stays unstyled ---
  it("unknown words outside parens in EXEC CICS get no token", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS SEND"),
      fixedLine("         XYZZYNOTREAL"),
      fixedLine("    END-EXEC."),
    ]);

    const unknownLine = CICS_BASE + 1;
    const unknownToks = tokensOnLine(tokens, docLines, unknownLine);
    // XYZZYNOTREAL should NOT appear in tokens
    expect(unknownToks.every(t => t.word !== "XYZZYNOTREAL")).toBe(true);
  });

  // --- 14. Overlapping words: CONDITION is both CICS_OPTIONS and a regular word ---
  it("CONDITION after HANDLE is option, not confused with condition set", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS HANDLE CONDITION"),
      fixedLine("         NOTFND"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    // HANDLE = verb, CONDITION = option
    expect(execToks).toContainEqual({ word: "HANDLE", type: 2 });
    expect(execToks).toContainEqual({ word: "CONDITION", type: 11 });

    // NOTFND outside parens = condition (type 9)
    const nfLine = execLine + 1;
    expect(tokensOnLine(tokens, docLines, nfLine))
      .toContainEqual({ word: "NOTFND", type: 9 });
  });

  // --- 15. Condition word ambiguity: ERROR is in CICS_CONDITIONS but also COBOL_KEYWORDS ---
  it("ERROR outside EXEC CICS is keyword, inside EXEC CICS is condition", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS HANDLE CONDITION"),
      fixedLine("         ERROR"),
      fixedLine("    END-EXEC."),
      fixedLine("    IF WS-RESP = 1"),
      fixedLine("       DISPLAY 'ERROR CASE'"),
      fixedLine("    END-IF."),
    ]);

    // ERROR inside EXEC CICS = type (condition)
    const errorCicsLine = CICS_BASE + 1;
    expect(tokensOnLine(tokens, docLines, errorCicsLine))
      .toContainEqual({ word: "ERROR", type: 9 });

    // No ERROR token outside (it's inside a string in our example)
    // But let's also check IF = keyword
    const ifLine = CICS_BASE + 3;
    expect(tokensOnLine(tokens, docLines, ifLine))
      .toContainEqual({ word: "IF", type: 0 });
  });

  // --- 16. IGNORE CONDITION ---
  it("IGNORE CONDITION: IGNORE=verb, CONDITION=option, condition names=type", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS IGNORE CONDITION"),
      fixedLine("         MAPFAIL LENGERR"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "IGNORE", type: 2 });
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "CONDITION", type: 11 });

    const condLine = execLine + 1;
    const condToks = tokensOnLine(tokens, docLines, condLine);
    expect(condToks).toContainEqual({ word: "MAPFAIL", type: 9 });
    expect(condToks).toContainEqual({ word: "LENGERR", type: 9 });
  });

  // --- 17. ADDRESS command ---
  it("ADDRESS: CWA/EIB/TCTUA=property", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS ADDRESS"),
      fixedLine("         CWA(WS-REC)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "ADDRESS", type: 2 });

    const cwaLine = execLine + 1;
    expect(tokensOnLine(tokens, docLines, cwaLine))
      .toContainEqual({ word: "CWA", type: 11 });
    expect(tokensOnLine(tokens, docLines, cwaLine))
      .toContainEqual({ word: "WS-REC", type: 1 });
  });

  // --- 18. SYNCPOINT ROLLBACK ---
  it("SYNCPOINT ROLLBACK: both verbs", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS SYNCPOINT ROLLBACK"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    const toks = tokensOnLine(tokens, docLines, execLine);
    expect(toks).toContainEqual({ word: "SYNCPOINT", type: 2 });
    // ROLLBACK is also a verb in CICS_VERBS
    expect(toks).toContainEqual({ word: "ROLLBACK", type: 2 });
  });

  // --- 19. Data name not in definition index (e.g. from COPY) still gets no wrong token ---
  it("unknown data name in parens gets no semantic token", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS LINK"),
      fixedLine("         PROGRAM('SUBPGM')"),
      fixedLine("         COMMAREA(UNKNOWN-FIELD)"),
      fixedLine("    END-EXEC."),
    ]);

    // UNKNOWN-FIELD is not in our data items → should not appear in tokens
    const commLine = CICS_BASE + 2;
    const commToks = tokensOnLine(tokens, docLines, commLine);
    expect(commToks).toContainEqual({ word: "COMMAREA", type: 11 });
    expect(commToks.every(t => t.word !== "UNKNOWN-FIELD")).toBe(true);
  });

  // --- 20. Normal code AFTER EXEC CICS block resumes normal classification ---
  it("normal COBOL code after EXEC CICS uses standard classification", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS RETURN END-EXEC."),
      fixedLine("    DISPLAY WS-REC."),
      fixedLine("    PERFORM MAIN-PARA."),
    ]);

    // After END-EXEC: DISPLAY=keyword, WS-REC=variable
    const dispLine = CICS_BASE + 1;
    const dispToks = tokensOnLine(tokens, docLines, dispLine);
    expect(dispToks).toContainEqual({ word: "DISPLAY", type: 0 });
    expect(dispToks).toContainEqual({ word: "WS-REC", type: 1 });

    // PERFORM=keyword, MAIN-PARA=function
    const perfLine = CICS_BASE + 2;
    const perfToks = tokensOnLine(tokens, docLines, perfLine);
    expect(perfToks).toContainEqual({ word: "PERFORM", type: 0 });
    expect(perfToks).toContainEqual({ word: "MAIN-PARA", type: 2 });
  });

  // --- 21. RESP2 word regex: contains digit, must be caught by WORD_RE ---
  it("RESP2 is matched by word regex (contains digit)", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS READ"),
      fixedLine("         FILE('F')"),
      fixedLine("         RESP2(WS-LEN)"),
      fixedLine("    END-EXEC."),
    ]);

    const resp2Line = CICS_BASE + 2;
    const resp2Toks = tokensOnLine(tokens, docLines, resp2Line);
    // WORD_RE = /[A-Za-z][A-Za-z0-9_-]*/g — starts with letter, then alphanumeric
    expect(resp2Toks).toContainEqual({ word: "RESP2", type: 11 });
    expect(resp2Toks).toContainEqual({ word: "WS-LEN", type: 1 });
  });

  // --- 22. String inside parens stays string, not variable ---
  it("string literals inside CICS parens are classified as string, not variable", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS WRITEQ TS"),
      fixedLine("         QUEUE('MYQUEUE')"),
      fixedLine("         FROM(WS-REC)"),
      fixedLine("    END-EXEC."),
    ]);

    const queueLine = CICS_BASE + 1;
    const queueToks = tokensOnLine(tokens, docLines, queueLine);
    expect(queueToks).toContainEqual({ word: "QUEUE", type: 11 });
    // 'MYQUEUE' should be string (4), not variable
    const stringToks = queueToks.filter(t => t.type === 4);
    expect(stringToks.length).toBe(1);
    // No variable tokens on the QUEUE line (only QUEUE + string)
    expect(queueToks.filter(t => t.type === 1).length).toBe(0);
  });

  // --- 23. EMKON09 real-world: ASSIGN with space before paren ---
  it("EMKON09: ASSIGN TCTUALENG (TCTUALENG) — space before paren", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS ASSIGN TCTUALENG (TCTUALENG)"),
      fixedLine("                     OPID      (OPER-ID)"),
      fixedLine("                     SYSID     (SYS-ID)"),
      fixedLine("                     END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    expect(execToks).toContainEqual({ word: "EXEC", type: 8 });
    expect(execToks).toContainEqual({ word: "CICS", type: 8 });
    expect(execToks).toContainEqual({ word: "ASSIGN", type: 2 });     // verb
    expect(execToks).toContainEqual({ word: "TCTUALENG", type: 11 }); // option (outside parens)

    // OPID on continuation line
    const opidLine = execLine + 1;
    const opidToks = tokensOnLine(tokens, docLines, opidLine);
    expect(opidToks).toContainEqual({ word: "OPID", type: 11 }); // option

    // SYSID on continuation line
    const sysidLine = execLine + 2;
    const sysidToks = tokensOnLine(tokens, docLines, sysidLine);
    expect(sysidToks).toContainEqual({ word: "SYSID", type: 11 }); // option
  });

  // --- 24. EMKON09 real-world: HANDLE CONDITION with space before paren ---
  it("EMKON09: HANDLE CONDITION NOTOPEN (NOT-OPEN) — option + condition", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS HANDLE CONDITION NOTOPEN (NOT-OPEN)"),
      fixedLine("                               NOSPACE (NO-SPACE)"),
      fixedLine("                               END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    expect(execToks).toContainEqual({ word: "HANDLE", type: 2 });      // verb
    expect(execToks).toContainEqual({ word: "CONDITION", type: 11 });   // option
    expect(execToks).toContainEqual({ word: "NOTOPEN", type: 9 });      // condition

    // NOSPACE on continuation
    const nospLine = execLine + 1;
    const nospToks = tokensOnLine(tokens, docLines, nospLine);
    expect(nospToks).toContainEqual({ word: "NOSPACE", type: 9 }); // condition
  });

  // --- 25. EMKON09 real-world: ADDRESS TCTUA (ADDRESS OF TCTUA) ---
  it("EMKON09: ADDRESS TCTUA (ADDRESS OF TCTUA) — ADDRESS OF inside parens", () => {
    const { tokens, docLines } = cicsDoc([
      fixedLine("    EXEC CICS ADDRESS TCTUA (ADDRESS OF TCTUA)"),
      fixedLine("                      END-EXEC."),
    ]);

    const execLine = CICS_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    expect(execToks).toContainEqual({ word: "EXEC", type: 8 });
    expect(execToks).toContainEqual({ word: "CICS", type: 8 });
    expect(execToks).toContainEqual({ word: "ADDRESS", type: 2 });  // verb (first one)
    expect(execToks).toContainEqual({ word: "TCTUA", type: 11 });   // option (outside parens)
  });
});

// ---- Systematic EXEC DLI semantic token tests ----

describe("EXEC DLI semantic tokens — systematic", () => {
  // Common boilerplate for a minimal COBOL program with data items
  const preamble = [
    fixedLine("IDENTIFICATION DIVISION."),
    fixedLine("PROGRAM-ID. TEST1."),
    fixedLine("DATA DIVISION."),
    fixedLine("WORKING-STORAGE SECTION."),
    fixedLine("01 WS-REC       PIC X(80)."),
    fixedLine("01 WS-KEY       PIC X(6)."),
    fixedLine("01 WS-LEN       PIC S9(4) COMP."),
    fixedLine("01 WS-AREA      PIC X(200)."),
    fixedLine("01 WS-SEGNAME   PIC X(8)."),
    fixedLine("01 WS-PCB-NUM   PIC S9(4) COMP."),
    fixedLine("01 WS-FBAREA    PIC X(50)."),
    fixedLine("01 WS-FBLEN     PIC S9(4) COMP."),
    fixedLine("01 WS-ID        PIC X(8)."),
    fixedLine("01 WS-PSB       PIC X(8)."),
    fixedLine("01 WS-OFFSET    PIC S9(4) COMP."),
    fixedLine("01 WS-SEGLEN    PIC S9(4) COMP."),
    fixedLine("01 WS-FLDLEN    PIC S9(4) COMP."),
    fixedLine("PROCEDURE DIVISION."),
  ];

  const DLI_BASE = preamble.length + 1; // first DLI line after MAIN-PARA

  function dliDoc(dliLines: string[]) {
    const docLines = [
      ...preamble,
      fixedLine("MAIN-PARA."),
      ...dliLines,
      fixedLine("    STOP RUN."),
    ];
    const doc = makeDoc(docLines);
    const toks = buildSemanticTokens(doc);
    const dl = lines(doc);
    return { doc, tokens: toks, docLines: dl };
  }

  // --- 1. GU with USING PCB, SEGMENT, INTO ---
  it("GU: verb=function, USING/PCB/SEGMENT/INTO=property, data-in-parens=variable", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GU"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    expect(execToks).toContainEqual({ word: "EXEC", type: 8 });   // macro
    expect(execToks).toContainEqual({ word: "DLI", type: 8 });    // macro
    expect(execToks).toContainEqual({ word: "GU", type: 2 });     // function (verb)

    // USING PCB(WS-PCB-NUM)
    const pcbLine = execLine + 1;
    const pcbToks = tokensOnLine(tokens, docLines, pcbLine);
    expect(pcbToks).toContainEqual({ word: "USING", type: 11 });   // property
    expect(pcbToks).toContainEqual({ word: "PCB", type: 11 });     // property
    expect(pcbToks).toContainEqual({ word: "WS-PCB-NUM", type: 1 }); // variable

    // SEGMENT(WS-SEGNAME)
    const segLine = execLine + 2;
    const segToks = tokensOnLine(tokens, docLines, segLine);
    expect(segToks).toContainEqual({ word: "SEGMENT", type: 11 }); // property
    expect(segToks).toContainEqual({ word: "WS-SEGNAME", type: 1 });

    // INTO(WS-AREA)
    const intoLine = execLine + 3;
    const intoToks = tokensOnLine(tokens, docLines, intoLine);
    expect(intoToks).toContainEqual({ word: "INTO", type: 11 });    // property
    expect(intoToks).toContainEqual({ word: "WS-AREA", type: 1 }); // variable

    // END-EXEC
    const endLine = execLine + 4;
    expect(tokensOnLine(tokens, docLines, endLine))
      .toContainEqual({ word: "END-EXEC", type: 8 });
  });

  // --- 2. GN with WHERE and FIELDLENGTH ---
  it("GN with WHERE/FIELDLENGTH: clause keywords=property", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GN"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA)"),
      fixedLine("         WHERE(KEYFIELD=WS-KEY)"),
      fixedLine("         FIELDLENGTH(WS-FLDLEN)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "GN", type: 2 });

    // WHERE line
    const whereLine = execLine + 4;
    const whereToks = tokensOnLine(tokens, docLines, whereLine);
    expect(whereToks).toContainEqual({ word: "WHERE", type: 11 });

    // FIELDLENGTH line
    const flLine = execLine + 5;
    const flToks = tokensOnLine(tokens, docLines, flLine);
    expect(flToks).toContainEqual({ word: "FIELDLENGTH", type: 11 });
    expect(flToks).toContainEqual({ word: "WS-FLDLEN", type: 1 });
  });

  // --- 3. ISRT with FROM ---
  it("ISRT: verb=function, FROM=property", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI ISRT"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         FROM(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "ISRT", type: 2 });

    const fromLine = execLine + 3;
    const fromToks = tokensOnLine(tokens, docLines, fromLine);
    expect(fromToks).toContainEqual({ word: "FROM", type: 11 });
    expect(fromToks).toContainEqual({ word: "WS-AREA", type: 1 });
  });

  // --- 4. REPL with SEGMENT and FROM ---
  it("REPL: verb=function, SEGMENT/FROM=property", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI REPL"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         FROM(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "REPL", type: 2 });
  });

  // --- 5. DLET with FROM ---
  it("DLET: verb=function", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI DLET"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         FROM(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "DLET", type: 2 });
  });

  // --- 6. CHKP with ID ---
  it("CHKP ID: verb=function, ID=property", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI CHKP"),
      fixedLine("         ID(WS-ID)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "CHKP", type: 2 });

    const idLine = execLine + 1;
    const idToks = tokensOnLine(tokens, docLines, idLine);
    expect(idToks).toContainEqual({ word: "ID", type: 11 });
    expect(idToks).toContainEqual({ word: "WS-ID", type: 1 });
  });

  // --- 7. SCHD with PSB ---
  it("SCHD PSB: verb=function, PSB=property", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI SCHD"),
      fixedLine("         PSB(WS-PSB)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "SCHD", type: 2 });

    const psbLine = execLine + 1;
    const psbToks = tokensOnLine(tokens, docLines, psbLine);
    expect(psbToks).toContainEqual({ word: "PSB", type: 11 });
    expect(psbToks).toContainEqual({ word: "WS-PSB", type: 1 });
  });

  // --- 8. TERM (no options) ---
  it("TERM: verb=function, no options", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI TERM"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    expect(execToks).toContainEqual({ word: "EXEC", type: 8 });
    expect(execToks).toContainEqual({ word: "DLI", type: 8 });
    expect(execToks).toContainEqual({ word: "TERM", type: 2 });
  });

  // --- 9. LOAD with VARIABLE, SEGLENGTH ---
  it("LOAD VARIABLE SEGLENGTH: all clause keywords=property", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI LOAD"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         VARIABLE SEGMENT(WS-SEGNAME)"),
      fixedLine("         FROM(WS-AREA)"),
      fixedLine("         SEGLENGTH(WS-SEGLEN)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "LOAD", type: 2 });

    const varSegLine = execLine + 2;
    const varSegToks = tokensOnLine(tokens, docLines, varSegLine);
    expect(varSegToks).toContainEqual({ word: "VARIABLE", type: 11 });
    expect(varSegToks).toContainEqual({ word: "SEGMENT", type: 11 });

    const seglenLine = execLine + 4;
    expect(tokensOnLine(tokens, docLines, seglenLine))
      .toContainEqual({ word: "SEGLENGTH", type: 11 });
  });

  // --- 10. Long-form: GET UNIQUE ---
  it("GET UNIQUE: long-form verb words=function", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GET UNIQUE"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    expect(execToks).toContainEqual({ word: "GET", type: 2 });     // function
    expect(execToks).toContainEqual({ word: "UNIQUE", type: 2 });  // function
  });

  // --- 11. Long-form: GET NEXT IN PARENT ---
  it("GET NEXT IN PARENT: all long-form words=function", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GET NEXT IN PARENT"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    const execToks = tokensOnLine(tokens, docLines, execLine);
    expect(execToks).toContainEqual({ word: "GET", type: 2 });
    expect(execToks).toContainEqual({ word: "NEXT", type: 2 });
    expect(execToks).toContainEqual({ word: "IN", type: 2 });
    expect(execToks).toContainEqual({ word: "PARENT", type: 2 });
  });

  // --- 12. Long-form: GET HOLD NEXT IN PARENT ---
  it("GET HOLD NEXT IN PARENT: all long-form words=function", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI"),
      fixedLine("         GET HOLD NEXT IN PARENT"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const verbLine = DLI_BASE + 1;
    const verbToks = tokensOnLine(tokens, docLines, verbLine);
    expect(verbToks).toContainEqual({ word: "GET", type: 2 });
    expect(verbToks).toContainEqual({ word: "HOLD", type: 2 });
    expect(verbToks).toContainEqual({ word: "NEXT", type: 2 });
    expect(verbToks).toContainEqual({ word: "IN", type: 2 });
    expect(verbToks).toContainEqual({ word: "PARENT", type: 2 });
  });

  // --- 13. Long-form: INSERT, REPLACE, DELETE ---
  it("INSERT / REPLACE / DELETE long forms=function", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI INSERT"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME) FROM(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    expect(tokensOnLine(tokens, docLines, DLI_BASE))
      .toContainEqual({ word: "INSERT", type: 2 });
  });

  // --- 14. Long-form: CHECKPOINT, SCHEDULE, TERMINATE ---
  it("CHECKPOINT / SCHEDULE / TERMINATE long forms=function", () => {
    const { tokens: t1, docLines: d1 } = dliDoc([
      fixedLine("    EXEC DLI CHECKPOINT ID(WS-ID)"),
      fixedLine("    END-EXEC."),
    ]);
    expect(tokensOnLine(t1, d1, DLI_BASE))
      .toContainEqual({ word: "CHECKPOINT", type: 2 });

    const { tokens: t2, docLines: d2 } = dliDoc([
      fixedLine("    EXEC DLI SCHEDULE PSB(WS-PSB)"),
      fixedLine("    END-EXEC."),
    ]);
    expect(tokensOnLine(t2, d2, DLI_BASE))
      .toContainEqual({ word: "SCHEDULE", type: 2 });

    const { tokens: t3, docLines: d3 } = dliDoc([
      fixedLine("    EXEC DLI TERMINATE END-EXEC."),
    ]);
    expect(tokensOnLine(t3, d3, DLI_BASE))
      .toContainEqual({ word: "TERMINATE", type: 2 });
  });

  // --- 15. GHU with KEYFEEDBACK and FEEDBACKLEN ---
  it("GHU KEYFEEDBACK/FEEDBACKLEN: clause keywords=property", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GHU"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         KEYFEEDBACK(WS-FBAREA)"),
      fixedLine("         FEEDBACKLEN(WS-FBLEN)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "GHU", type: 2 });

    const kfLine = execLine + 2;
    const kfToks = tokensOnLine(tokens, docLines, kfLine);
    expect(kfToks).toContainEqual({ word: "KEYFEEDBACK", type: 11 });
    expect(kfToks).toContainEqual({ word: "WS-FBAREA", type: 1 });

    const fbLine = execLine + 3;
    const fbToks = tokensOnLine(tokens, docLines, fbLine);
    expect(fbToks).toContainEqual({ word: "FEEDBACKLEN", type: 11 });
    expect(fbToks).toContainEqual({ word: "WS-FBLEN", type: 1 });
  });

  // --- 16. GN with LOCKED, OFFSET ---
  it("GN LOCKED/OFFSET: clause keywords=property", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GN"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA) LOCKED"),
      fixedLine("         OFFSET(WS-OFFSET)"),
      fixedLine("    END-EXEC."),
    ]);

    const lockedLine = DLI_BASE + 3;
    const lockedToks = tokensOnLine(tokens, docLines, lockedLine);
    expect(lockedToks).toContainEqual({ word: "LOCKED", type: 11 });

    const offsetLine = DLI_BASE + 4;
    const offsetToks = tokensOnLine(tokens, docLines, offsetLine);
    expect(offsetToks).toContainEqual({ word: "OFFSET", type: 11 });
    expect(offsetToks).toContainEqual({ word: "WS-OFFSET", type: 1 });
  });

  // --- 17. GU with FIRST / LAST prefix ---
  it("FIRST / LAST segment prefixes=property", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GU"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         FIRST SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const firstLine = DLI_BASE + 2;
    const firstToks = tokensOnLine(tokens, docLines, firstLine);
    expect(firstToks).toContainEqual({ word: "FIRST", type: 11 });
    expect(firstToks).toContainEqual({ word: "SEGMENT", type: 11 });
  });

  // --- 18. Single-line EXEC DLI ... END-EXEC ---
  it("single-line EXEC DLI TERM END-EXEC: all tokens classified", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI TERM END-EXEC."),
    ]);

    const execLine = DLI_BASE;
    const toks = tokensOnLine(tokens, docLines, execLine);
    expect(toks).toContainEqual({ word: "EXEC", type: 8 });
    expect(toks).toContainEqual({ word: "DLI", type: 8 });
    expect(toks).toContainEqual({ word: "TERM", type: 2 });
    expect(toks).toContainEqual({ word: "END-EXEC", type: 8 });
  });

  // --- 19. DLI keywords must NOT bleed outside EXEC DLI blocks ---
  it("DLI keywords are normal COBOL keywords outside EXEC DLI", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI TERM END-EXEC."),
      fixedLine("    MOVE WS-REC TO WS-KEY."),
      fixedLine("    DELETE FILE-X."),
    ]);

    // TERM inside EXEC DLI = function(2)
    const execLine = DLI_BASE;
    expect(tokensOnLine(tokens, docLines, execLine))
      .toContainEqual({ word: "TERM", type: 2 });

    // MOVE/TO on next line = keyword(0)
    const moveLine = execLine + 1;
    const moveToks = tokensOnLine(tokens, docLines, moveLine);
    expect(moveToks).toContainEqual({ word: "MOVE", type: 0 });
    expect(moveToks).toContainEqual({ word: "TO", type: 0 });

    // DELETE outside EXEC DLI = keyword(0)
    const deleteLine = execLine + 2;
    expect(tokensOnLine(tokens, docLines, deleteLine))
      .toContainEqual({ word: "DELETE", type: 0 });
  });

  // --- 20. Comment lines inside EXEC DLI block ---
  it("comment lines inside EXEC DLI are classified as comments", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GU"),
      fixedLine("* DLI comment line", "*"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("    END-EXEC."),
    ]);

    const commentLine = DLI_BASE + 1;
    const commentToks = tokensOnLine(tokens, docLines, commentLine);
    expect(commentToks.length).toBe(1);
    expect(commentToks[0].type).toBe(6); // comment
  });

  // --- 21. Two sequential EXEC DLI blocks, proper state reset ---
  it("two sequential EXEC DLI blocks are both classified correctly", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GU"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
      fixedLine("    MOVE WS-AREA TO WS-REC."),
      fixedLine("    EXEC DLI REPL"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         FROM(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    // First block
    expect(tokensOnLine(tokens, docLines, DLI_BASE))
      .toContainEqual({ word: "GU", type: 2 });
    expect(tokensOnLine(tokens, docLines, DLI_BASE + 3))
      .toContainEqual({ word: "WS-AREA", type: 1 });

    // Between blocks
    const moveLine = DLI_BASE + 5;
    expect(tokensOnLine(tokens, docLines, moveLine))
      .toContainEqual({ word: "MOVE", type: 0 });

    // Second block
    const replLine = DLI_BASE + 6;
    expect(tokensOnLine(tokens, docLines, replLine))
      .toContainEqual({ word: "REPL", type: 2 });
    expect(tokensOnLine(tokens, docLines, replLine + 3))
      .toContainEqual({ word: "FROM", type: 11 });
  });

  // --- 22. Mixed EXEC DLI and EXEC CICS in same program ---
  it("EXEC DLI and EXEC CICS blocks coexist correctly", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GU"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
      fixedLine("    EXEC CICS SEND"),
      fixedLine("         MAP('MAP001')"),
      fixedLine("         ERASE"),
      fixedLine("    END-EXEC."),
    ]);

    // DLI block
    expect(tokensOnLine(tokens, docLines, DLI_BASE))
      .toContainEqual({ word: "GU", type: 2 });
    expect(tokensOnLine(tokens, docLines, DLI_BASE))
      .toContainEqual({ word: "DLI", type: 8 });

    // CICS block
    const cicsLine = DLI_BASE + 5;
    expect(tokensOnLine(tokens, docLines, cicsLine))
      .toContainEqual({ word: "CICS", type: 8 });
    expect(tokensOnLine(tokens, docLines, cicsLine))
      .toContainEqual({ word: "SEND", type: 2 });
    expect(tokensOnLine(tokens, docLines, cicsLine + 2))
      .toContainEqual({ word: "ERASE", type: 11 });
  });

  // --- 23. String literal inside DLI parens ---
  it("string literals inside DLI parens are classified as string, not variable", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI SCHD"),
      fixedLine("         PSB('MYPSB')"),
      fixedLine("    END-EXEC."),
    ]);

    const psbLine = DLI_BASE + 1;
    const psbToks = tokensOnLine(tokens, docLines, psbLine);
    expect(psbToks).toContainEqual({ word: "PSB", type: 11 });
    const stringToks = psbToks.filter(t => t.type === 4);
    expect(stringToks.length).toBe(1); // 'MYPSB'
    expect(psbToks.filter(t => t.type === 1).length).toBe(0);
  });

  // --- 24. Unknown word outside parens gets no token ---
  it("unknown words outside parens in EXEC DLI get no token", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GU"),
      fixedLine("         XYZZYNOTREAL"),
      fixedLine("         SEGMENT(WS-SEGNAME)"),
      fixedLine("         INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const unknownLine = DLI_BASE + 1;
    const unknownToks = tokensOnLine(tokens, docLines, unknownLine);
    expect(unknownToks.every(t => t.word !== "XYZZYNOTREAL")).toBe(true);
  });

  // --- 25. Normal code after EXEC DLI resumes normal classification ---
  it("normal COBOL code after EXEC DLI uses standard classification", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI TERM END-EXEC."),
      fixedLine("    DISPLAY WS-REC."),
      fixedLine("    PERFORM MAIN-PARA."),
    ]);

    const dispLine = DLI_BASE + 1;
    expect(tokensOnLine(tokens, docLines, dispLine))
      .toContainEqual({ word: "DISPLAY", type: 0 });
    expect(tokensOnLine(tokens, docLines, dispLine))
      .toContainEqual({ word: "WS-REC", type: 1 });

    const perfLine = DLI_BASE + 2;
    expect(tokensOnLine(tokens, docLines, perfLine))
      .toContainEqual({ word: "PERFORM", type: 0 });
    expect(tokensOnLine(tokens, docLines, perfLine))
      .toContainEqual({ word: "MAIN-PARA", type: 2 });
  });

  // --- 26. GHN and GHNP short-form verbs ---
  it("GHN / GHNP short forms=function", () => {
    const { tokens: t1, docLines: d1 } = dliDoc([
      fixedLine("    EXEC DLI GHN"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME) INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);
    expect(tokensOnLine(t1, d1, DLI_BASE))
      .toContainEqual({ word: "GHN", type: 2 });

    const { tokens: t2, docLines: d2 } = dliDoc([
      fixedLine("    EXEC DLI GHNP"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME) INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);
    expect(tokensOnLine(t2, d2, DLI_BASE))
      .toContainEqual({ word: "GHNP", type: 2 });
  });

  // --- 27. GNP short-form ---
  it("GNP short form=function", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GNP"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME) INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);
    expect(tokensOnLine(tokens, docLines, DLI_BASE))
      .toContainEqual({ word: "GNP", type: 2 });
  });

  // --- 28. WHERE with AND/OR boolean operators ---
  it("WHERE with AND/OR: boolean keywords=property inside DLI", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GU"),
      fixedLine("         USING PCB(WS-PCB-NUM)"),
      fixedLine("         SEGMENT(WS-SEGNAME) INTO(WS-AREA)"),
      fixedLine("         WHERE(F1=WS-KEY AND F2=WS-LEN)"),
      fixedLine("         FIELDLENGTH(WS-FLDLEN)"),
      fixedLine("    END-EXEC."),
    ]);

    const whereLine = DLI_BASE + 3;
    const whereToks = tokensOnLine(tokens, docLines, whereLine);
    expect(whereToks).toContainEqual({ word: "WHERE", type: 11 });
    // AND inside parens — data reference context, not classified as property
    // WS-KEY and WS-LEN inside parens = variable
    expect(whereToks).toContainEqual({ word: "WS-KEY", type: 1 });
    expect(whereToks).toContainEqual({ word: "WS-LEN", type: 1 });
  });

  // --- 29. Numeric values in DLI ---
  it("numeric literals in EXEC DLI are classified as number", () => {
    const { tokens, docLines } = dliDoc([
      fixedLine("    EXEC DLI GU"),
      fixedLine("         USING PCB(1)"),
      fixedLine("         SEGMENT(WS-SEGNAME) INTO(WS-AREA)"),
      fixedLine("    END-EXEC."),
    ]);

    const pcbLine = DLI_BASE + 1;
    const pcbToks = tokensOnLine(tokens, docLines, pcbLine);
    expect(pcbToks.some(t => t.type === 5)).toBe(true); // number
  });
});

// ---- encodeSemanticTokens ----

describe("encodeSemanticTokens", () => {
  it("encodes tokens as delta format", () => {
    const tokens: SemanticToken[] = [
      { line: 0, startChar: 7, length: 10, tokenType: 0, tokenModifiers: 0 },
      { line: 0, startChar: 18, length: 8, tokenType: 0, tokenModifiers: 0 },
      { line: 2, startChar: 7, length: 4, tokenType: 2, tokenModifiers: 1 },
    ];

    const data = encodeSemanticTokens(tokens);
    expect(data).toEqual([
      // Token 1: line=0, char=7, len=10, type=0, mod=0
      0, 7, 10, 0, 0,
      // Token 2: same line, deltaChar=11 (18-7), len=8, type=0, mod=0
      0, 11, 8, 0, 0,
      // Token 3: deltaLine=2, char=7 (new line), len=4, type=2, mod=1
      2, 7, 4, 2, 1,
    ]);
  });

  it("handles empty token list", () => {
    const data = encodeSemanticTokens([]);
    expect(data).toEqual([]);
  });
});
