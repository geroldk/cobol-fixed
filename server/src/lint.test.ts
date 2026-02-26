import { describe, it, expect } from "vitest";
import {
  scanLintTokens,
  collectExecDliRanges,
  overlapsAnyRange,
  suggestProcedureVerb,
  lintPreprocessed,
} from "./lint";

// ======================= scanLintTokens =======================

describe("scanLintTokens", () => {
  it("tokenizes words and dots", () => {
    const toks = scanLintTokens("MOVE A TO B.");
    const words = toks.filter((t) => t.kind === "word");
    const dots = toks.filter((t) => t.kind === "dot");
    expect(words).toHaveLength(4);
    expect(dots).toHaveLength(1);
    expect(words[0]).toMatchObject({ upper: "MOVE" });
  });

  it("skips inline comments *>", () => {
    const toks = scanLintTokens("MOVE A *> comment\nMOVE B.");
    const words = toks.filter((t) => t.kind === "word");
    expect(words.map((w) => w.upper)).toEqual(["MOVE", "A", "MOVE", "B"]);
  });

  it("skips string literals", () => {
    const toks = scanLintTokens("DISPLAY 'HELLO.WORLD'.");
    const words = toks.filter((t) => t.kind === "word");
    expect(words).toHaveLength(1);
    expect(words[0].upper).toBe("DISPLAY");
  });

  it("does not treat decimal point as separator", () => {
    const toks = scanLintTokens("PIC 9.99");
    const dots = toks.filter((t) => t.kind === "dot");
    expect(dots).toHaveLength(0);
  });
});

// ======================= collectExecDliRanges =======================

describe("collectExecDliRanges", () => {
  it("collects EXEC DLI ... END-EXEC range", () => {
    const text = "EXEC DLI GU USING PCB(1) END-EXEC.";
    const ranges = collectExecDliRanges(text);
    expect(ranges).toHaveLength(1);
    expect(ranges[0].start).toBe(0);
  });

  it("ignores EXEC SQL blocks", () => {
    const text = "EXEC SQL SELECT * FROM T END-EXEC.";
    const ranges = collectExecDliRanges(text);
    expect(ranges).toHaveLength(0);
  });
});

// ======================= overlapsAnyRange =======================

describe("overlapsAnyRange", () => {
  it("detects overlap", () => {
    expect(overlapsAnyRange(5, 10, [{ start: 3, end: 7 }])).toBe(true);
  });

  it("detects no overlap", () => {
    expect(overlapsAnyRange(0, 3, [{ start: 5, end: 10 }])).toBe(false);
  });

  it("works with empty ranges", () => {
    expect(overlapsAnyRange(0, 5, [])).toBe(false);
  });
});

// ======================= suggestProcedureVerb =======================

describe("suggestProcedureVerb", () => {
  it("suggests MOVE for MOZE", () => {
    const knownVerbs = ["MOVE", "ADD", "SUBTRACT", "DISPLAY"];
    expect(suggestProcedureVerb("MOZE", knownVerbs)).toBe("MOVE");
  });

  it("returns undefined for short tokens", () => {
    expect(suggestProcedureVerb("MO", ["MOVE"])).toBeUndefined();
  });

  it("returns undefined for non-alpha tokens", () => {
    expect(suggestProcedureVerb("MO-VE", ["MOVE"])).toBeUndefined();
  });

  it("returns undefined when no match within limit", () => {
    expect(suggestProcedureVerb("XYZW", ["MOVE"])).toBeUndefined();
  });
});

// ======================= lintPreprocessed (integration) =======================

describe("lintPreprocessed", () => {
  it("reports IF closed by period (BLOCK_CLOSED_BY_PERIOD)", () => {
    const text = "       IF A = B\n       MOVE C TO D.\n";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "BLOCK_CLOSED_BY_PERIOD")).toBe(true);
  });

  it("reports END-IF without IF", () => {
    const text = "       END-IF.\n";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "END_IF_WITHOUT_IF")).toBe(true);
  });

  it("accepts properly closed IF", () => {
    const text = "       IF A = B\n       MOVE C TO D\n       END-IF.\n";
    const diags = lintPreprocessed(text);
    expect(diags.filter((d) => d.code === "UNCLOSED_BLOCK" || d.code === "END_IF_WITHOUT_IF")).toHaveLength(0);
  });
});
