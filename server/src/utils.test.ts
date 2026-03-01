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
  isBmsSource,
  parseBmsMapset,
  generateCobolFromBms,
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

// ======================= isBmsSource =======================

describe("isBmsSource", () => {
  it("returns true for text containing DFHMSD", () => {
    expect(isBmsSource("MAPSET  DFHMSD TYPE=MAP,LANG=COBOL")).toBe(true);
  });

  it("returns false for normal COBOL", () => {
    expect(isBmsSource("       01  MY-FIELD PIC X(10).")).toBe(false);
  });
});

// ======================= parseBmsMapset =======================

describe("parseBmsMapset", () => {
  // Helper: build a fixed-width 80-col assembler line
  const bmsLine = (label: string, op: string, operands: string, cont = " ") => {
    const labelPad = label.padEnd(8, " ");
    const opPad = op.padEnd(6, " ");
    const body = `${labelPad}${opPad} ${operands}`;
    const padded = body.padEnd(71, " ");
    return padded + cont + "00000000";
  };

  it("parses a minimal BMS mapset with one map and two fields", () => {
    const text = [
      bmsLine("TESTMS", "DFHMSD", "TYPE=MAP,LANG=COBOL,MODE=INOUT,TIOAPFX=YES"),
      bmsLine("MAP1", "DFHMDI", "COLUMN=1,LINE=1,SIZE=(24,80)"),
      bmsLine("FLD1", "DFHMDF", "POS=(1,1),LENGTH=10"),
      bmsLine("FLD2", "DFHMDF", "POS=(2,1),LENGTH=05"),
      bmsLine("", "DFHMDF", "POS=(3,1),LENGTH=20"),   // unnamed → no COBOL field
      bmsLine("", "DFHMSD", "TYPE=FINAL"),
      "         END",
    ].join("\n");

    const ms = parseBmsMapset(text);
    expect(ms).toBeDefined();
    expect(ms!.mapsetName).toBe("TESTMS");
    expect(ms!.tioapfx).toBe(true);
    expect(ms!.maps).toHaveLength(1);
    expect(ms!.maps[0].mapName).toBe("MAP1");
    expect(ms!.maps[0].fields).toHaveLength(2);
    expect(ms!.maps[0].fields[0]).toEqual({ name: "FLD1", length: 10 });
    expect(ms!.maps[0].fields[1]).toEqual({ name: "FLD2", length: 5 });
  });

  it("handles continuation lines for LENGTH", () => {
    const text = [
      bmsLine("MS1", "DFHMSD", "TYPE=MAP,LANG=COBOL,MODE=INOUT"),
      bmsLine("M1", "DFHMDI", "COLUMN=1,LINE=1,SIZE=(24,80)"),
      bmsLine("F1", "DFHMDF", "POS=(1,1),ATTRB=(ASKIP,BRT),", "-"),  // continuation
      bmsLine("", "", "       LENGTH=04"),                             // cont. line (operands at col 16)
      bmsLine("", "DFHMSD", "TYPE=FINAL"),
    ].join("\n");

    const ms = parseBmsMapset(text);
    expect(ms).toBeDefined();
    expect(ms!.maps[0].fields).toHaveLength(1);
    expect(ms!.maps[0].fields[0]).toEqual({ name: "F1", length: 4 });
  });

  it("handles multiple maps in one mapset", () => {
    const text = [
      bmsLine("MS", "DFHMSD", "TYPE=MAP,LANG=COBOL,MODE=INOUT,TIOAPFX=YES"),
      bmsLine("MAPA", "DFHMDI", "COLUMN=1,LINE=1,SIZE=(24,80)"),
      bmsLine("FA1", "DFHMDF", "POS=(1,1),LENGTH=05"),
      bmsLine("MAPB", "DFHMDI", "COLUMN=1,LINE=1,SIZE=(24,80)"),
      bmsLine("FB1", "DFHMDF", "POS=(1,1),LENGTH=08"),
      bmsLine("FB2", "DFHMDF", "POS=(2,1),LENGTH=12"),
      bmsLine("", "DFHMSD", "TYPE=FINAL"),
    ].join("\n");

    const ms = parseBmsMapset(text);
    expect(ms).toBeDefined();
    expect(ms!.maps).toHaveLength(2);
    expect(ms!.maps[0].mapName).toBe("MAPA");
    expect(ms!.maps[0].fields).toHaveLength(1);
    expect(ms!.maps[1].mapName).toBe("MAPB");
    expect(ms!.maps[1].fields).toHaveLength(2);
  });

  it("returns undefined for text with no maps", () => {
    expect(parseBmsMapset("       01  FOO PIC X.")).toBeUndefined();
  });
});

// ======================= generateCobolFromBms =======================

describe("generateCobolFromBms", () => {
  it("generates I/O 01-levels with L/F/A/I/O fields per map field", () => {
    const cobol = generateCobolFromBms({
      mapsetName: "TESTMS",
      tioapfx: true,
      maps: [{
        mapName: "MAP1",
        fields: [
          { name: "FLD1", length: 10 },
          { name: "FLD2", length: 5 },
        ],
      }],
    });

    // 01-levels
    expect(cobol).toContain("01  MAP1I.");
    expect(cobol).toContain("01  MAP1O REDEFINES MAP1I.");

    // TIOAPFX filler (appears twice: once in I, once in O)
    expect(cobol.match(/FILLER PIC X\(12\)/g)!.length).toBe(2);

    // Field suffixes for input map
    expect(cobol).toContain("FLD1L PIC S9(4) COMP.");
    expect(cobol).toContain("FLD1F PIC X.");
    expect(cobol).toContain("FLD1A PIC X.");
    expect(cobol).toContain("FLD1I PIC X(10).");

    // Field suffixes for output map
    expect(cobol).toContain("FLD1O PIC X(10).");
    expect(cobol).toContain("FLD2O PIC X(5).");

    // No I suffix in the O block — each field appears once with I and once with O
    const iCount = cobol.match(/FLD1I/g)!.length;
    const oCount = cobol.match(/FLD1O/g)!.length;
    expect(iCount).toBe(1);
    expect(oCount).toBe(1);
  });

  it("omits TIOAPFX filler when tioapfx is false", () => {
    const cobol = generateCobolFromBms({
      mapsetName: "X",
      tioapfx: false,
      maps: [{ mapName: "M", fields: [{ name: "F", length: 1 }] }],
    });

    expect(cobol).not.toContain("FILLER PIC X(12)");
  });

  it("generates valid fixed-format COBOL (all lines ≤ 72 cols)", () => {
    const cobol = generateCobolFromBms({
      mapsetName: "LONGNAME",
      tioapfx: true,
      maps: [{
        mapName: "DCEMI9",
        fields: [{ name: "DC24161", length: 64 }],
      }],
    });

    for (const line of cobol.split("\n")) {
      if (line.length > 0) {
        expect(line.length).toBeLessThanOrEqual(72);
      }
    }
  });
});
