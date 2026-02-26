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
