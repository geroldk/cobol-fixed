import { describe, expect, it, vi } from "vitest";

// Mock vscode before importing the module under test
vi.mock("vscode", () => ({
  workspace: { getConfiguration: () => ({}) },
  window: { showQuickPick: vi.fn(), showInputBox: vi.fn() },
  ProgressLocation: { Notification: 1 },
}));

import { assembleJobText } from "./jobAssembler";
import type { MemberConf, VseSettings } from "./types";

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------

function makeSettings(overrides: Partial<VseSettings["placeholders"]> = {}): VseSettings {
  return {
    host: "localhost",
    port: 2893,
    user: "TEST",
    charset: "utf8",
    submit: { executionMode: "blocking", timeoutSec: 120, previewBeforeSubmit: false },
    conf: { autoCreateOnMissing: true, previewBeforeCreate: false },
    placeholders: {
      catalogBatchTest: "USRWMT.BATCH",
      catalogBatchProd: "USRWMP.BATCH",
      catalogCicsTest: "USRWMT.CICS",
      catalogCicsProd: "USRWMP.CICS",
      id: "DLI/COBOL",
      lnkstep: "// EXEC LNKEDT",
      ...overrides,
    },
    output: { baseDir: ".vse/out" },
  };
}

function makeConf(overrides: Partial<MemberConf> = {}): MemberConf {
  return {
    source: 0,
    phase: "TESTPHAS",
    type: 2,
    tjcl: [
      "* $$ JOB JNM=TEST",
      "// LIBDEF *,CATALOG=#CATALOG#",
      "// EXEC IGYCRCTL",
      "#XOPTS#",
      "#COMPILEOPTIONS#",
      "#SOURCE#",
      "/*",
      "#LNKSTEP#",
      "/&",
      "* $$ EOJ",
    ].join("\n"),
    pjcl: [
      "* $$ JOB JNM=TEST",
      "// LIBDEF *,CATALOG=#CATALOG#",
      "// EXEC IGYCRCTL",
      "#XOPTS#",
      "#COMPILEOPTIONS#",
      "#SOURCE#",
      "/*",
      "#LNKSTEP#",
      "/&",
      "* $$ EOJ",
    ].join("\n"),
    toptions: " CBL LIB, APOST",
    poptions: " CBL LIB, APOST",
    txopts: "",
    pxopts: "",
    ...overrides,
  };
}

// ---------------------------------------------------------------------------
// catalog selection by type × mode
// ---------------------------------------------------------------------------

describe("assembleJobText — catalog selection", () => {
  it("type 2 (COBOL85) + mode T → uses catalogBatchTest", () => {
    const { jobText } = assembleJobText({
      conf: makeConf({ type: 2 }),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(jobText).toContain("CATALOG=USRWMT.BATCH");
    expect(jobText).not.toContain("CICS");
  });

  it("type 2 (COBOL85) + mode P → uses catalogBatchProd", () => {
    const { jobText } = assembleJobText({
      conf: makeConf({ type: 2 }),
      mode: "P",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(jobText).toContain("CATALOG=USRWMP.BATCH");
  });

  it("type 3 (DLI) + mode T → uses catalogBatchTest", () => {
    const { jobText } = assembleJobText({
      conf: makeConf({ type: 3 }),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(jobText).toContain("CATALOG=USRWMT.BATCH");
  });

  it("type 3 (DLI) + mode P → uses catalogBatchProd", () => {
    const { jobText } = assembleJobText({
      conf: makeConf({ type: 3 }),
      mode: "P",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(jobText).toContain("CATALOG=USRWMP.BATCH");
  });

  it("type 4 (DLI+CICS) + mode T → uses catalogCicsTest", () => {
    const { jobText } = assembleJobText({
      conf: makeConf({ type: 4 }),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(jobText).toContain("CATALOG=USRWMT.CICS");
  });

  it("type 4 (DLI+CICS) + mode P → uses catalogCicsProd", () => {
    const { jobText } = assembleJobText({
      conf: makeConf({ type: 4 }),
      mode: "P",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(jobText).toContain("CATALOG=USRWMP.CICS");
  });

  it("custom catalog values are used verbatim", () => {
    const { jobText } = assembleJobText({
      conf: makeConf({ type: 4 }),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings({ catalogCicsTest: "CUSTOM.LIB" }),
    });
    expect(jobText).toContain("CATALOG=CUSTOM.LIB");
  });
});

// ---------------------------------------------------------------------------
// missing catalog setting
// ---------------------------------------------------------------------------

describe("assembleJobText — missing catalog", () => {
  it("throws with setting key when catalogBatchTest is empty", () => {
    expect(() =>
      assembleJobText({
        conf: makeConf({ type: 2 }),
        mode: "T",
        sourceText: "SOURCE",
        settings: makeSettings({ catalogBatchTest: "" }),
      })
    ).toThrow(/catalogBatchTest/);
  });

  it("throws with setting key when catalogCicsProd is empty", () => {
    expect(() =>
      assembleJobText({
        conf: makeConf({ type: 4 }),
        mode: "P",
        sourceText: "SOURCE",
        settings: makeSettings({ catalogCicsProd: "" }),
      })
    ).toThrow(/catalogCicsProd/);
  });

  it("throws with setting key when catalogBatchProd is whitespace-only", () => {
    expect(() =>
      assembleJobText({
        conf: makeConf({ type: 3 }),
        mode: "P",
        sourceText: "SOURCE",
        settings: makeSettings({ catalogBatchProd: "   " }),
      })
    ).toThrow(/catalogBatchProd/);
  });
});

// ---------------------------------------------------------------------------
// #COMPILEOPTIONS# with multi-line CBL content
// ---------------------------------------------------------------------------

describe("assembleJobText — CBL compile options", () => {
  it("replaces #COMPILEOPTIONS# with multi-line CBL content", () => {
    const multiLineCbl =
      " CBL LIB, APOST, NOADV, NODYNAM, RENT, BUF(4096), NOSEQ\n" +
      " CBL DATA(24), TRUNC(BIN), ZWB, NOOPTIMIZE, TEST, NOSSRANGE";
    const { jobText } = assembleJobText({
      conf: makeConf({ toptions: multiLineCbl }),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(jobText).toContain(" CBL LIB, APOST, NOADV, NODYNAM, RENT, BUF(4096), NOSEQ");
    expect(jobText).toContain(" CBL DATA(24), TRUNC(BIN), ZWB, NOOPTIMIZE, TEST, NOSSRANGE");
  });

  it("each CBL line in the output is ≤ 72 chars", () => {
    const multiLineCbl =
      " CBL LIB, APOST, NOADV, NODYNAM, RENT, BUF(4096), NOSEQ\n" +
      " CBL DATA(24), TRUNC(BIN), ZWB, NOOPTIMIZE, TEST, NOSSRANGE";
    const { jobText } = assembleJobText({
      conf: makeConf({ toptions: multiLineCbl }),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    const cblLines = jobText.split("\n").filter((l) => l.trimStart().startsWith("CBL") || l.startsWith(" CBL"));
    expect(cblLines.length).toBeGreaterThanOrEqual(2);
    for (const line of cblLines) {
      expect(line.length).toBeLessThanOrEqual(72);
    }
  });
});

// ---------------------------------------------------------------------------
// #LNKSTEP# expansion
// ---------------------------------------------------------------------------

describe("assembleJobText — LNKSTEP expansion", () => {
  it("replaces #LNKSTEP# with lnkstep value", () => {
    const { jobText } = assembleJobText({
      conf: makeConf(),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings({ lnkstep: "// EXEC LNKEDT\n// EXEC LISTLOG" }),
    });
    expect(jobText).toContain("// EXEC LNKEDT");
    expect(jobText).toContain("// EXEC LISTLOG");
  });

  it("multi-line lnkstep preserves line breaks", () => {
    const lnk = "// EXEC LNKEDT\n// EXEC LISTLOG";
    const { jobText } = assembleJobText({
      conf: makeConf(),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings({ lnkstep: lnk }),
    });
    const idx1 = jobText.indexOf("// EXEC LNKEDT");
    const idx2 = jobText.indexOf("// EXEC LISTLOG");
    expect(idx1).toBeLessThan(idx2);
    // They must be on separate lines
    const between = jobText.slice(idx1, idx2);
    expect(between).toContain("\n");
  });
});

// ---------------------------------------------------------------------------
// placeholder resolution
// ---------------------------------------------------------------------------

describe("assembleJobText — placeholder resolution", () => {
  it("resolves all required tokens without error", () => {
    const result = assembleJobText({
      conf: makeConf(),
      mode: "T",
      sourceText: "IDENTIFICATION DIVISION.",
      settings: makeSettings(),
    });
    expect(result.jobText).not.toContain("#PHASENAME#");
    expect(result.jobText).not.toContain("#CATALOG#");
    expect(result.jobText).not.toContain("#ID#");
    expect(result.jobText).not.toContain("#XOPTS#");
    expect(result.jobText).not.toContain("#COMPILEOPTIONS#");
    expect(result.jobText).not.toContain("#SOURCE#");
    expect(result.jobText).not.toContain("#LNKSTEP#");
  });

  it("phase is returned alongside jobText", () => {
    const result = assembleJobText({
      conf: makeConf({ phase: "MYPHASE" }),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(result.phase).toBe("MYPHASE");
  });

  it("throws when a required placeholder remains unresolved", () => {
    // JCL with an extra #PHASENAME# that won't be substituted normally is fine
    // because #PHASENAME# IS substituted. But if #ID# is in the JCL and id is
    // empty, requirePlaceholderValue throws before we get to unresolved check.
    expect(() =>
      assembleJobText({
        conf: makeConf(),
        mode: "T",
        sourceText: "SOURCE",
        settings: makeSettings({ id: "" }),
      })
    ).toThrow(/id/i);
  });

  it("throws when lnkstep placeholder is empty", () => {
    expect(() =>
      assembleJobText({
        conf: makeConf(),
        mode: "T",
        sourceText: "SOURCE",
        settings: makeSettings({ lnkstep: "" }),
      })
    ).toThrow(/lnkstep/i);
  });
});

// ---------------------------------------------------------------------------
// #XOPTS# handling
// ---------------------------------------------------------------------------

describe("assembleJobText — XOPTS", () => {
  it("replaces #XOPTS# with xopts value for DLI (type 3)", () => {
    const { jobText } = assembleJobText({
      conf: makeConf({ type: 3, txopts: "XOPTS(DLI NOLIST XREF DEBUG COBOL2)" }),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(jobText).toContain("XOPTS(DLI NOLIST XREF DEBUG COBOL2)");
  });

  it("replaces #XOPTS# with empty string for type 2 (no xopts)", () => {
    const { jobText } = assembleJobText({
      conf: makeConf({ type: 2, txopts: "" }),
      mode: "T",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(jobText).not.toContain("XOPTS(");
  });

  it("uses pxopts in production mode", () => {
    const { jobText } = assembleJobText({
      conf: makeConf({
        type: 4,
        txopts: "XOPTS(TEST)",
        pxopts: "XOPTS(CICS DLI PROD)",
      }),
      mode: "P",
      sourceText: "SOURCE",
      settings: makeSettings(),
    });
    expect(jobText).toContain("XOPTS(CICS DLI PROD)");
    expect(jobText).not.toContain("XOPTS(TEST)");
  });
});

// ---------------------------------------------------------------------------
// #SOURCE# substitution
// ---------------------------------------------------------------------------

describe("assembleJobText — SOURCE", () => {
  it("inserts source text into the job", () => {
    const src = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. HELLO.";
    const { jobText } = assembleJobText({
      conf: makeConf(),
      mode: "T",
      sourceText: src,
      settings: makeSettings(),
    });
    expect(jobText).toContain("IDENTIFICATION DIVISION.");
    expect(jobText).toContain("PROGRAM-ID. HELLO.");
  });
});
