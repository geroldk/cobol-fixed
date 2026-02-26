import { describe, it, expect } from "vitest";
import {
  tokenizeCobolText,
  parseCopyStatements,
  normalizeCopyName,
  unwrapReplacingToken,
  replaceAllHeuristic,
} from "./preprocessor";

// ======================= tokenizeCobolText =======================

describe("tokenizeCobolText", () => {
  it("tokenizes simple COPY statement", () => {
    const toks = tokenizeCobolText("COPY MYBOOK.");
    expect(toks).toHaveLength(3);
    expect(toks[0]).toMatchObject({ upper: "COPY", kind: "word" });
    expect(toks[1]).toMatchObject({ upper: "MYBOOK", kind: "word" });
    expect(toks[2]).toMatchObject({ kind: "dot" });
  });

  it("handles pseudo-text delimiters", () => {
    const toks = tokenizeCobolText("==FOO== BY ==BAR==");
    expect(toks).toHaveLength(3);
    expect(toks[0].kind).toBe("pseudo");
    expect(toks[0].text).toBe("==FOO==");
    expect(toks[1]).toMatchObject({ upper: "BY", kind: "word" });
    expect(toks[2].kind).toBe("pseudo");
  });

  it("handles string literals", () => {
    const toks = tokenizeCobolText("'hello' \"world\"");
    expect(toks).toHaveLength(2);
    expect(toks[0]).toMatchObject({ kind: "string", text: "'hello'" });
    expect(toks[1]).toMatchObject({ kind: "string", text: "\"world\"" });
  });

  it("marks unclosed pseudo-text", () => {
    const toks = tokenizeCobolText("==UNCLOSED");
    expect(toks).toHaveLength(1);
    expect(toks[0].kind).toBe("pseudo");
    expect(toks[0].closed).toBe(false);
  });
});

// ======================= parseCopyStatements =======================

describe("parseCopyStatements", () => {
  it("parses simple COPY", () => {
    const stmts = parseCopyStatements("COPY MYBOOK.");
    expect(stmts).toHaveLength(1);
    expect(stmts[0].copybookName).toBe("MYBOOK");
    expect(stmts[0].terminatedByDot).toBe(true);
    expect(stmts[0].errors).toHaveLength(0);
  });

  it("parses COPY with REPLACING", () => {
    const stmts = parseCopyStatements("COPY MYBOOK REPLACING ==FOO== BY ==BAR==.");
    expect(stmts).toHaveLength(1);
    expect(stmts[0].copybookName).toBe("MYBOOK");
    expect(stmts[0].replacing).toHaveLength(1);
    expect(stmts[0].replacing[0].from).toBe("==FOO==");
    expect(stmts[0].replacing[0].to).toBe("==BAR==");
  });

  it("reports error for missing copybook name", () => {
    const stmts = parseCopyStatements("COPY.");
    expect(stmts).toHaveLength(0); // COPY without name skips
  });

  it("reports error for missing period", () => {
    const stmts = parseCopyStatements("COPY MYBOOK");
    expect(stmts).toHaveLength(1);
    expect(stmts[0].terminatedByDot).toBe(false);
    expect(stmts[0].errors.some((e) => e.code === "COPY_MISSING_PERIOD")).toBe(true);
  });

  it("parses COPY with IN library", () => {
    const stmts = parseCopyStatements("COPY MYBOOK IN MYLIB.");
    expect(stmts).toHaveLength(1);
    expect(stmts[0].copybookName).toBe("MYBOOK");
  });
});

// ======================= normalizeCopyName =======================

describe("normalizeCopyName", () => {
  it("strips quotes", () => {
    expect(normalizeCopyName("\"MYBOOK\"")).toBe("MYBOOK");
    expect(normalizeCopyName("'MYBOOK'")).toBe("MYBOOK");
  });

  it("strips pseudo-text delimiters", () => {
    expect(normalizeCopyName("==MYBOOK==")).toBe("MYBOOK");
  });

  it("strips trailing dots", () => {
    expect(normalizeCopyName("MYBOOK..")).toBe("MYBOOK");
  });

  it("passes through plain names", () => {
    expect(normalizeCopyName("MYBOOK")).toBe("MYBOOK");
  });
});

// ======================= unwrapReplacingToken =======================

describe("unwrapReplacingToken", () => {
  it("unwraps pseudo-text", () => {
    expect(unwrapReplacingToken("==FOO==", "pseudo")).toBe("FOO");
  });

  it("unwraps string", () => {
    expect(unwrapReplacingToken("'FOO'", "string")).toBe("FOO");
    expect(unwrapReplacingToken("\"FOO\"", "string")).toBe("FOO");
  });

  it("passes through word tokens", () => {
    expect(unwrapReplacingToken("FOO", "word")).toBe("FOO");
  });
});

// ======================= replaceAllHeuristic =======================

describe("replaceAllHeuristic", () => {
  it("replaces word tokens with word-boundary matching", () => {
    const r = replaceAllHeuristic("MOVE FOO TO BAR", "FOO", "QUUX", "word");
    expect(r.nextText).toBe("MOVE QUUX TO BAR");
    expect(r.count).toBe(1);
  });

  it("does not replace partial word matches for word kind", () => {
    const r = replaceAllHeuristic("MOVE FOOBAR TO BAR", "FOO", "QUUX", "word");
    expect(r.nextText).toBe("MOVE FOOBAR TO BAR");
    expect(r.count).toBe(0);
  });

  it("replaces pseudo-text substring matches", () => {
    const r = replaceAllHeuristic("ABCFOODEF", "FOO", "X", "pseudo");
    expect(r.nextText).toBe("ABCXDEF");
    expect(r.count).toBe(1);
  });

  it("handles empty needle", () => {
    const r = replaceAllHeuristic("TEXT", "", "X", "word");
    expect(r.nextText).toBe("TEXT");
    expect(r.count).toBe(0);
  });
});
