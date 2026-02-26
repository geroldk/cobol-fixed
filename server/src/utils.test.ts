import { describe, it, expect } from "vitest";
import {
  hasFixedColumns,
  isFixedCommentIndicator,
  isValidFixedIndicator,
  escapeRegExp,
  excerpt,
  hasSeparatorPeriodOutsideLiterals,
  levenshteinDistanceWithinLimit,
  sliceLines,
} from "./utils";

// ======================= hasFixedColumns =======================

describe("hasFixedColumns", () => {
  it("returns true for lines >= 7 chars with sequence area", () => {
    expect(hasFixedColumns("000100 IDENTIFICATION DIVISION.")).toBe(true);
  });

  it("returns false for short lines", () => {
    expect(hasFixedColumns("ABC")).toBe(false);
    expect(hasFixedColumns("")).toBe(false);
  });

  it("returns true for exactly 7-char line", () => {
    expect(hasFixedColumns("123456 ")).toBe(true);
  });
});

// ======================= isFixedCommentIndicator =======================

describe("isFixedCommentIndicator", () => {
  it("recognises * / D d", () => {
    expect(isFixedCommentIndicator("*")).toBe(true);
    expect(isFixedCommentIndicator("/")).toBe(true);
    expect(isFixedCommentIndicator("D")).toBe(true);
    expect(isFixedCommentIndicator("d")).toBe(true);
  });

  it("rejects non-comment indicators", () => {
    expect(isFixedCommentIndicator(" ")).toBe(false);
    expect(isFixedCommentIndicator("-")).toBe(false);
    expect(isFixedCommentIndicator("X")).toBe(false);
  });
});

// ======================= isValidFixedIndicator =======================

describe("isValidFixedIndicator", () => {
  it("accepts space, *, /, -, D, d", () => {
    for (const c of [" ", "*", "/", "-", "D", "d"]) {
      expect(isValidFixedIndicator(c)).toBe(true);
    }
  });

  it("rejects others", () => {
    expect(isValidFixedIndicator("X")).toBe(false);
    expect(isValidFixedIndicator("1")).toBe(false);
  });
});

// ======================= escapeRegExp =======================

describe("escapeRegExp", () => {
  it("escapes special regex characters", () => {
    expect(escapeRegExp("a.b*c")).toBe("a\\.b\\*c");
    expect(escapeRegExp("foo(bar)")).toBe("foo\\(bar\\)");
  });
});

// ======================= excerpt =======================

describe("excerpt", () => {
  it("truncates and collapses whitespace", () => {
    expect(excerpt("hello  world\nnext", 10)).toBe("hello worl");
  });

  it("handles empty", () => {
    expect(excerpt("", 5)).toBe("");
  });
});

// ======================= hasSeparatorPeriodOutsideLiterals =======================

describe("hasSeparatorPeriodOutsideLiterals", () => {
  it("finds period in plain text", () => {
    expect(hasSeparatorPeriodOutsideLiterals("MOVE A TO B.")).toBe(true);
  });

  it("ignores period inside string literal", () => {
    expect(hasSeparatorPeriodOutsideLiterals("DISPLAY 'A.B'")).toBe(false);
  });

  it("ignores decimal in PIC 9(3).9(2)", () => {
    expect(hasSeparatorPeriodOutsideLiterals("PIC 9.99")).toBe(false);
  });

  it("finds trailing period even after PIC clause", () => {
    expect(hasSeparatorPeriodOutsideLiterals("PIC X(10).")).toBe(true);
  });
});

// ======================= levenshteinDistanceWithinLimit =======================

describe("levenshteinDistanceWithinLimit", () => {
  it("returns 0 for identical words", () => {
    expect(levenshteinDistanceWithinLimit("MOVE", "MOVE", 2)).toBe(0);
  });

  it("returns 1 for single-char difference", () => {
    expect(levenshteinDistanceWithinLimit("MOVE", "MOZE", 2)).toBe(1);
  });

  it("returns limit+1 when beyond limit", () => {
    expect(levenshteinDistanceWithinLimit("ABC", "XYZ", 2)).toBe(3);
  });
});

// ======================= sliceLines =======================

describe("sliceLines", () => {
  it("parses fixed-format lines correctly", () => {
    const text = "000100 IDENTIFICATION DIVISION.";
    const slices = sliceLines(text);
    expect(slices).toHaveLength(1);
    expect(slices[0].isFixed).toBe(true);
    expect(slices[0].indicator).toBe(" ");
    expect(slices[0].isComment).toBe(false);
    expect(slices[0].lang).toBe("IDENTIFICATION DIVISION.");
  });

  it("detects comment lines", () => {
    const text = "000100*THIS IS A COMMENT";
    const slices = sliceLines(text);
    expect(slices[0].isComment).toBe(true);
  });

  it("marks short lines as non-fixed", () => {
    const text = "SHORT";
    const slices = sliceLines(text);
    expect(slices[0].isFixed).toBe(false);
    expect(slices[0].lang).toBe("");
  });
});
