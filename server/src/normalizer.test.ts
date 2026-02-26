import { describe, it, expect } from "vitest";
import {
  normalizeForCobol85Parser,
  isListingControlStatement,
  isCompilerDirectiveStatement,
  isCompilerDirectiveLine,
  normalizeFixedLineForParser,
} from "./normalizer";

// ======================= normalizeFixedLineForParser =======================

describe("normalizeFixedLineForParser", () => {
  it("blanks sequence area and keeps language area", () => {
    const line72 = "000100 IDENTIFICATION DIVISION.                                        ";
    const result = normalizeFixedLineForParser(line72, " ", false);
    expect(result.slice(0, 6)).toBe("      ");       // sequence blanked
    expect(result[6]).toBe(" ");                      // indicator preserved
    expect(result.slice(7)).toContain("IDENTIFICATION");
  });

  it("blanks comment lines entirely", () => {
    const line72 = "000100*THIS IS A COMMENT LINE                                          ";
    const result = normalizeFixedLineForParser(line72, "*", true);
    expect(result.trim()).toBe("");
  });

  it("preserves continuation indicator for string literals", () => {
    const line72 = "000100-    'CONTINUED STRING'                                          ";
    const result = normalizeFixedLineForParser(line72, "-", false);
    // The '-' indicator should be preserved when continuation starts with a quote
    expect(result[6]).toBe("-");
  });
});

// ======================= normalizeForCobol85Parser =======================

describe("normalizeForCobol85Parser", () => {
  it("normalizes basic COBOL text", () => {
    const text = "000100 IDENTIFICATION DIVISION.";
    const result = normalizeForCobol85Parser(text);
    expect(result.text).toContain("IDENTIFICATION");
    expect(result.adjustments).toBeDefined();
  });

  it("handles ID DIVISION alias", () => {
    const text = "000100 ID DIVISION.";
    const result = normalizeForCobol85Parser(text);
    // ID DIVISION should become IDENTIFICATION DIVISION in normalized form
    expect(result.text).toContain("IDENTIFICATION DIVISION");
  });
});

// ======================= isListingControlStatement =======================

describe("isListingControlStatement", () => {
  it("detects EJECT", () => {
    expect(isListingControlStatement("EJECT")).toBe(true);
    expect(isListingControlStatement("EJECT.")).toBe(true);
  });

  it("detects SKIP1/SKIP2/SKIP3", () => {
    expect(isListingControlStatement("SKIP1")).toBe(true);
    expect(isListingControlStatement("SKIP2.")).toBe(true);
    expect(isListingControlStatement("SKIP3")).toBe(true);
  });

  it("rejects non-listing statements", () => {
    expect(isListingControlStatement("MOVE")).toBe(false);
  });
});

// ======================= isCompilerDirectiveStatement =======================

describe("isCompilerDirectiveStatement", () => {
  it("detects CBL/PROCESS but not TITLE (TITLE not implemented)", () => {
    expect(isCompilerDirectiveStatement("CBL RENT")).toBe(true);
    expect(isCompilerDirectiveStatement("PROCESS RENT")).toBe(true);
    expect(isCompilerDirectiveStatement("TITLE 'My Program'")).toBe(false);
  });
});

// ======================= isCompilerDirectiveLine =======================

describe("isCompilerDirectiveLine", () => {
  it("detects fixed-format compiler directive in language area", () => {
    expect(isCompilerDirectiveLine("000100 CBL RENT")).toBe(true);
  });

  it("rejects regular COBOL lines", () => {
    expect(isCompilerDirectiveLine("000100 MOVE A TO B")).toBe(false);
  });
});
