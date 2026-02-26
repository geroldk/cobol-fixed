import { describe, it, expect } from "vitest";
import {
  computeTreeEdit,
  utf8ByteOffsetToUtf16Index,
  utf16IndexToUtf8ByteOffset,
  utf16IndexToTreeSitterPoint,
  isLikelyGlobalParserFailure,
  filterTreeSitterErrors,
  collectTreeSitterErrors,
} from "./parser-bridge";

// ======================= utf8ByteOffsetToUtf16Index =======================

describe("utf8ByteOffsetToUtf16Index", () => {
  it("matches for pure ASCII", () => {
    expect(utf8ByteOffsetToUtf16Index("hello", 0)).toBe(0);
    expect(utf8ByteOffsetToUtf16Index("hello", 3)).toBe(3);
    expect(utf8ByteOffsetToUtf16Index("hello", 5)).toBe(5);
  });

  it("handles multi-byte UTF-8 (e.g. ä = 2 bytes)", () => {
    const text = "aä";
    // 'a' = 1 byte, 'ä' = 2 bytes
    expect(utf8ByteOffsetToUtf16Index(text, 0)).toBe(0); // start of 'a'
    expect(utf8ByteOffsetToUtf16Index(text, 1)).toBe(1); // start of 'ä'
    expect(utf8ByteOffsetToUtf16Index(text, 3)).toBe(2); // past 'ä'
  });
});

// ======================= utf16IndexToUtf8ByteOffset =======================

describe("utf16IndexToUtf8ByteOffset", () => {
  it("matches for pure ASCII", () => {
    expect(utf16IndexToUtf8ByteOffset("hello", 3)).toBe(3);
  });

  it("handles multi-byte UTF-8", () => {
    const text = "aä";
    expect(utf16IndexToUtf8ByteOffset(text, 0)).toBe(0);
    expect(utf16IndexToUtf8ByteOffset(text, 1)).toBe(1);
    expect(utf16IndexToUtf8ByteOffset(text, 2)).toBe(3);
  });
});

// ======================= utf16IndexToTreeSitterPoint =======================

describe("utf16IndexToTreeSitterPoint", () => {
  it("returns row=0, col=0 for start", () => {
    const pt = utf16IndexToTreeSitterPoint("hello\nworld", 0);
    expect(pt).toEqual({ row: 0, column: 0 });
  });

  it("computes column correctly", () => {
    const pt = utf16IndexToTreeSitterPoint("hello\nworld", 8);
    expect(pt).toEqual({ row: 1, column: 2 }); // 'w' at col 0, 'o' at col 1, 'r' at col 2 -> byte offset
  });
});

// ======================= computeTreeEdit =======================

describe("computeTreeEdit", () => {
  it("returns undefined for identical text", () => {
    expect(computeTreeEdit("hello", "hello")).toBeUndefined();
  });

  it("detects single-char insertion", () => {
    const edit = computeTreeEdit("hllo", "hello");
    expect(edit).toBeDefined();
    expect(edit!.startIndex).toBe(1);
  });

  it("detects replacement", () => {
    const edit = computeTreeEdit("ab", "xb");
    expect(edit).toBeDefined();
    expect(edit!.startIndex).toBe(0);
  });
});

// ======================= isLikelyGlobalParserFailure =======================

describe("isLikelyGlobalParserFailure", () => {
  it("returns true for error spanning >= 70% of large text from near start", () => {
    // must be >= max(512, 70% of textLength) and startByte <= 16
    expect(isLikelyGlobalParserFailure({ startByte: 0, endByte: 1000, row: 0, col: 0 }, 1000)).toBe(true);
  });

  it("returns false for small error", () => {
    expect(isLikelyGlobalParserFailure({ startByte: 5, endByte: 10, row: 0, col: 5 }, 100)).toBe(false);
  });

  it("returns false when span is below 512", () => {
    expect(isLikelyGlobalParserFailure({ startByte: 0, endByte: 100, row: 0, col: 0 }, 100)).toBe(false);
  });
});

// ======================= filterTreeSitterErrors =======================

describe("filterTreeSitterErrors", () => {
  it("removes global failures from first position", () => {
    const errs = [{ startByte: 0, endByte: 1000, row: 0, col: 0 }];
    expect(filterTreeSitterErrors(errs, 1000)).toHaveLength(0);
  });

  it("keeps localised errors", () => {
    const errs = [{ startByte: 10, endByte: 15, row: 1, col: 5 }];
    expect(filterTreeSitterErrors(errs, 100)).toHaveLength(1);
  });
});
