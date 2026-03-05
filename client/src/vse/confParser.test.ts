import { describe, expect, it } from "vitest";
import { parseMemberConf, serializeMemberConf } from "./confParser";
import type { MemberConf } from "./types";

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------

function minimalConf(overrides: Partial<Record<string, string>> = {}): string {
  const defaults: Record<string, string> = {
    source: "0",
    phase: "TESTPHAS",
    type: "2",
    tjcl: '"// JOB TEST\\n/&"',
    pjcl: '"// JOB PROD\\n/&"',
    toptions: '"LIB, APOST, NOADV"',
    poptions: '"LIB, APOST, NOADV"',
  };
  const merged = { ...defaults, ...overrides };
  const lines = ["[General]"];
  for (const [k, v] of Object.entries(merged)) {
    lines.push(`${k}=${v}`);
  }
  return lines.join("\n");
}

// ---------------------------------------------------------------------------
// parseMemberConf
// ---------------------------------------------------------------------------

describe("parseMemberConf", () => {
  it("parses a valid conf with all fields", () => {
    const conf = parseMemberConf(minimalConf());
    expect(conf.source).toBe(0);
    expect(conf.phase).toBe("TESTPHAS");
    expect(conf.type).toBe(2);
    expect(conf.tjcl).toBe("// JOB TEST\n/&");
    expect(conf.pjcl).toBe("// JOB PROD\n/&");
  });

  it("parses multi-line toptions with CBL prefix (encoded newlines)", () => {
    const conf = parseMemberConf(minimalConf({
      toptions: '" CBL LIB, APOST, NOADV, NODYNAM, RENT, BUF(4096), NOSEQ\\n CBL DATA(24), TRUNC(BIN), ZWB, NOOPTIMIZE, TEST, NOSSRANGE"',
    }));
    const lines = conf.toptions.split("\n");
    expect(lines).toHaveLength(2);
    expect(lines[0]).toBe(" CBL LIB, APOST, NOADV, NODYNAM, RENT, BUF(4096), NOSEQ");
    expect(lines[1]).toBe(" CBL DATA(24), TRUNC(BIN), ZWB, NOOPTIMIZE, TEST, NOSSRANGE");
    // Each line must start with " CBL "
    for (const line of lines) {
      expect(line).toMatch(/^ CBL /);
    }
  });

  it("preserves leading space in CBL lines through encode/decode", () => {
    const conf = parseMemberConf(minimalConf({
      toptions: '" CBL OPT1\\n CBL OPT2"',
    }));
    expect(conf.toptions).toBe(" CBL OPT1\n CBL OPT2");
    expect(conf.toptions.startsWith(" CBL ")).toBe(true);
  });

  it("handles optional txopts / pxopts defaulting to empty string", () => {
    const conf = parseMemberConf(minimalConf());
    expect(conf.txopts).toBe("");
    expect(conf.pxopts).toBe("");
  });

  it("parses txopts and pxopts when present", () => {
    const conf = parseMemberConf(minimalConf({
      txopts: "XOPTS(DLI NOLIST XREF DEBUG COBOL2)",
      pxopts: "XOPTS(CICS DLI NOLIST XREF DEBUG COBOL2)",
    }));
    expect(conf.txopts).toBe("XOPTS(DLI NOLIST XREF DEBUG COBOL2)");
    expect(conf.pxopts).toBe("XOPTS(CICS DLI NOLIST XREF DEBUG COBOL2)");
  });

  it("throws on missing required key 'phase'", () => {
    const raw = "[General]\nsource=0\ntype=2\ntjcl=\"x\"\npjcl=\"x\"\ntoptions=\"x\"\npoptions=\"x\"";
    expect(() => parseMemberConf(raw)).toThrow(/phase/i);
  });

  it("throws on missing required key 'type'", () => {
    const raw = "[General]\nsource=0\nphase=X\ntjcl=\"x\"\npjcl=\"x\"\ntoptions=\"x\"\npoptions=\"x\"";
    expect(() => parseMemberConf(raw)).toThrow(/type/i);
  });

  it("throws on missing required key 'tjcl'", () => {
    const raw = "[General]\nsource=0\nphase=X\ntype=2\npjcl=\"x\"\ntoptions=\"x\"\npoptions=\"x\"";
    expect(() => parseMemberConf(raw)).toThrow(/tjcl/i);
  });

  it("throws on unsupported type value", () => {
    expect(() => parseMemberConf(minimalConf({ type: "9" }))).toThrow(/unsupported.*type/i);
  });

  it("accepts all valid type values (0–4)", () => {
    for (const t of [0, 1, 2, 3, 4]) {
      const conf = parseMemberConf(minimalConf({ type: String(t) }));
      expect(conf.type).toBe(t);
    }
  });

  it("handles empty quoted values", () => {
    const conf = parseMemberConf(minimalConf({ toptions: '""', poptions: '""' }));
    expect(conf.toptions).toBe("");
    expect(conf.poptions).toBe("");
  });

  it("ignores lines outside [General] section", () => {
    const raw = "[Other]\nphase=WRONG\n" + minimalConf();
    const conf = parseMemberConf(raw);
    expect(conf.phase).toBe("TESTPHAS");
  });

  it("ignores comment lines starting with ;", () => {
    const raw = minimalConf().replace("[General]", "[General]\n; this is a comment");
    const conf = parseMemberConf(raw);
    expect(conf.phase).toBe("TESTPHAS");
  });
});

// ---------------------------------------------------------------------------
// serializeMemberConf
// ---------------------------------------------------------------------------

describe("serializeMemberConf", () => {
  it("produces valid output with all fields", () => {
    const conf: MemberConf = {
      source: 0,
      phase: "MYPHASE",
      type: 3,
      tjcl: "// JOB T\n/&",
      pjcl: "// JOB P\n/&",
      toptions: "LIB, APOST, DATA(24)",
      poptions: "LIB, APOST, DATA(24)",
      txopts: "XOPTS(DLI NOLIST)",
      pxopts: "XOPTS(DLI NOLIST)",
    };
    const text = serializeMemberConf(conf);
    expect(text).toContain("[General]");
    expect(text).toContain("phase=MYPHASE");
    expect(text).toContain("type=3");
    expect(text).toContain("txopts=XOPTS(DLI NOLIST)");
  });

  it("serializes empty txopts as 'txopts='", () => {
    const conf: MemberConf = {
      source: 0, phase: "X", type: 2,
      tjcl: "a", pjcl: "b", toptions: "c", poptions: "d",
      txopts: "", pxopts: "",
    };
    const text = serializeMemberConf(conf);
    expect(text).toContain("txopts=\n");
    expect(text).toContain("pxopts=\n");
  });

  it("encodes newlines in quoted values", () => {
    const conf: MemberConf = {
      source: 0, phase: "X", type: 2,
      tjcl: "line1\nline2", pjcl: "a", toptions: "b", poptions: "c",
      txopts: "", pxopts: "",
    };
    const text = serializeMemberConf(conf);
    expect(text).toContain("\\n");
    expect(text).not.toContain("line1\nline2");  // should be escaped
  });
});

// ---------------------------------------------------------------------------
// Roundtrip: serialize → parse
// ---------------------------------------------------------------------------

describe("roundtrip", () => {
  it("preserves all fields through serialize → parse", () => {
    const original: MemberConf = {
      source: 42,
      phase: "ROUNDTRP",
      type: 4,
      tjcl: "* $$ JOB\n// JOB TEST\n/&\n* $$ EOJ",
      pjcl: "* $$ JOB\n// JOB PROD\n/&\n* $$ EOJ",
      toptions: "LIB, APOST, NOADV, NODYNAM, RENT, BUF(4096), NOSEQ, DATA(24), TRUNC(BIN), ZWB",
      poptions: "LIB, RENT, APOST",
      txopts: "XOPTS(CICS DLI NOLIST XREF DEBUG COBOL2)",
      pxopts: "XOPTS(CICS DLI NOLIST XREF DEBUG COBOL2)",
    };
    const text = serializeMemberConf(original);
    const parsed = parseMemberConf(text);

    expect(parsed.source).toBe(original.source);
    expect(parsed.phase).toBe(original.phase);
    expect(parsed.type).toBe(original.type);
    expect(parsed.tjcl).toBe(original.tjcl);
    expect(parsed.pjcl).toBe(original.pjcl);
    expect(parsed.toptions).toBe(original.toptions);
    expect(parsed.poptions).toBe(original.poptions);
    expect(parsed.txopts).toBe(original.txopts);
    expect(parsed.pxopts).toBe(original.pxopts);
  });

  it("preserves long compiler options string through roundtrip (no CBL prefix in conf)", () => {
    const original: MemberConf = {
      source: 0,
      phase: "CBLTEST",
      type: 2,
      tjcl: "// JOB\n/&",
      pjcl: "// JOB\n/&",
      toptions: "LIB, APOST, NOADV, NODYNAM, RENT, BUF(4096), NOSEQ, DATA(24), TRUNC(BIN), ZWB, NOOPTIMIZE, TEST, NOSSRANGE",
      poptions: "LIB, APOST, NOADV, NODYNAM, RENT, BUF(4096), NOSEQ, DATA(24), TRUNC(BIN), ZWB, NOOPTIMIZE, TEST, NOSSRANGE",
      txopts: "",
      pxopts: "",
    };
    const text = serializeMemberConf(original);
    const parsed = parseMemberConf(text);

    // Plain options survive roundtrip exactly
    expect(parsed.toptions).toBe(original.toptions);
    expect(parsed.poptions).toBe(original.poptions);
    // No CBL prefix in the stored conf
    expect(parsed.toptions).not.toContain(" CBL ");
  });

  it("preserves backslash sequences in JCL through roundtrip", () => {
    const original: MemberConf = {
      source: 0, phase: "ESCAPE", type: 2,
      tjcl: "PARM='A\\B'\tTAB",
      pjcl: "PARM='A\\B'\tTAB",
      toptions: "x", poptions: "x",
      txopts: "", pxopts: "",
    };
    const text = serializeMemberConf(original);
    const parsed = parseMemberConf(text);
    expect(parsed.tjcl).toBe(original.tjcl);
    expect(parsed.pjcl).toBe(original.pjcl);
  });
});
