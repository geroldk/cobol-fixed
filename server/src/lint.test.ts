import { describe, it, expect } from "vitest";
import {
  scanLintTokens,
  collectExecDliRanges,
  overlapsAnyRange,
  suggestProcedureVerb,
  lintPreprocessed,
  resolveDliRequest,
  validateDliClausesByRequest,
  lintUndefinedIdentifiers,
} from "./lint";
import { DiagnosticSeverity } from "vscode-languageserver/node";
import { DefinitionIndex } from "./definition";

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

  it("accepts EXEC CICS READ with SYSID and UPDATE", () => {
    const text = "EXEC CICS READ FILE(F1) INTO(REC) UPDATE SYSID(SYSA) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags UPDATE on EXEC CICS WRITE", () => {
    const text = "EXEC CICS WRITE FILE(F1) FROM(REC) UPDATE END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(true);
  });

  it("accepts ERASE for EXEC CICS SEND MAP", () => {
    const text = "EXEC CICS SEND MAP(MAP1) MAPSET(MSET1) ERASE END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("flags ERASE for EXEC CICS RECEIVE MAP", () => {
    const text = "EXEC CICS RECEIVE MAP(MAP1) INTO(AREA1) ERASE END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(true);
  });

  it("accepts EXEC CICS RECEIVE MAP without INTO (STORAGE=AUTO)", () => {
    const text = "EXEC CICS RECEIVE MAP('DCEMI9') END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("accepts DATASET as synonym for FILE in EXEC CICS READ", () => {
    const text = "EXEC CICS READ INTO(AREA1) DATASET('MYFILE') RIDFLD(K) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("FILE"))).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("accepts DATASET in EXEC CICS ENDBR", () => {
    const text = "EXEC CICS ENDBR DATASET('EAENDAT') END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("accepts EXEC CICS DELETE DATASET without RIDFLD", () => {
    const text = "EXEC CICS DELETE DATASET('LEICODB') END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("accepts HANDLE CONDITION LENGERR(label)", () => {
    const text = "EXEC CICS HANDLE CONDITION LENGERR(LERR) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_CONDITION_UNKNOWN")).toBe(false);
  });

  it("accepts HANDLE CONDITION with empty parens (ignore condition)", () => {
    const text = "EXEC CICS HANDLE CONDITION NOTFND ( ) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_CONDITION_MISSING_LABEL")).toBe(false);
  });

  it("accepts HANDLE CONDITION without label (cancel handling)", () => {
    const text = "EXEC CICS HANDLE CONDITION INVREQ END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_CONDITION_MISSING_LABEL")).toBe(false);
  });

  it("does not report unknown procedure verbs inside multiline EXEC CICS blocks", () => {
    const text = [
      "       PROCEDURE DIVISION.",
      "           EXEC CICS ASSIGN TCTUALENG(TCTUALENG)",
      "                     OPID(OPER-ID)",
      "                     SYSID(SYS-ID)",
      "           END-EXEC.",
      "           EXEC CICS HANDLE CONDITION QIDERR(ENTRE1)",
      "                     ITEMERR(ENTRE1)",
      "                     LENGERR(STAERR)",
      "           END-EXEC.",
    ].join("\n");
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "PROCEDURE_VERB_UNKNOWN")).toBe(false);
  });

  it("flags unknown HANDLE CONDITION value", () => {
    const text = "EXEC CICS HANDLE CONDITION FOOBAR(ERRLBL) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_CONDITION_UNKNOWN")).toBe(true);
  });

  it("flags unknown EXEC CICS command typo", () => {
    const text = "EXEC CICS REED FILE(F1) INTO(REC) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_UNKNOWN_COMMAND")).toBe(true);
  });

  // ── ABEND ──────────────────────────────────────────────────────────────
  it("accepts EXEC CICS ABEND with ABCODE", () => {
    const text = "EXEC CICS ABEND ABCODE(UC01) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("accepts EXEC CICS ABEND without options", () => {
    const text = "EXEC CICS ABEND END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("accepts EXEC CICS ABEND ABCODE CANCEL NODUMP", () => {
    const text = "EXEC CICS ABEND ABCODE(A1) CANCEL NODUMP END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── ADDRESS ────────────────────────────────────────────────────────────
  it("accepts EXEC CICS ADDRESS with CWA EIB", () => {
    const text = "EXEC CICS ADDRESS CWA(P1) EIB(P2) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── ASKTIME ────────────────────────────────────────────────────────────
  it("accepts EXEC CICS ASKTIME ABSTIME", () => {
    const text = "EXEC CICS ASKTIME ABSTIME(ATIME) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("accepts EXEC CICS ASKTIME without options", () => {
    const text = "EXEC CICS ASKTIME END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  // ── ASSIGN ─────────────────────────────────────────────────────────────
  it("accepts EXEC CICS ASSIGN with system fields", () => {
    const text = "EXEC CICS ASSIGN USERID(UID) SYSID(SID) PROGRAM(PGM) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── CANCEL ─────────────────────────────────────────────────────────────
  it("accepts EXEC CICS CANCEL REQID", () => {
    const text = "EXEC CICS CANCEL REQID(RQ01) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── DELAY ──────────────────────────────────────────────────────────────
  it("accepts EXEC CICS DELAY with INTERVAL", () => {
    const text = "EXEC CICS DELAY INTERVAL(003000) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("accepts EXEC CICS DELAY FOR SECONDS", () => {
    const text = "EXEC CICS DELAY FOR SECONDS(30) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── FORMATTIME ─────────────────────────────────────────────────────────
  it("accepts EXEC CICS FORMATTIME with ABSTIME and date options", () => {
    const text = "EXEC CICS FORMATTIME ABSTIME(AT) YYYYMMDD(DT) DATESEP(DS) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing ABSTIME on EXEC CICS FORMATTIME", () => {
    const text = "EXEC CICS FORMATTIME YYYYMMDD(DT) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("ABSTIME"))).toBe(true);
  });

  // ── GETMAIN ────────────────────────────────────────────────────────────
  it("accepts EXEC CICS GETMAIN SET FLENGTH", () => {
    const text = "EXEC CICS GETMAIN SET(PTR) FLENGTH(1024) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing SET on EXEC CICS GETMAIN", () => {
    const text = "EXEC CICS GETMAIN FLENGTH(1024) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("SET"))).toBe(true);
  });

  it("flags missing FLENGTH/LENGTH on EXEC CICS GETMAIN", () => {
    const text = "EXEC CICS GETMAIN SET(PTR) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("FLENGTH"))).toBe(true);
  });

  it("accepts EXEC CICS GETMAIN with BELOW INITIMG SHARED", () => {
    const text = "EXEC CICS GETMAIN SET(P) FLENGTH(256) BELOW INITIMG(X) SHARED END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── FREEMAIN ───────────────────────────────────────────────────────────
  it("accepts EXEC CICS FREEMAIN DATA", () => {
    const text = "EXEC CICS FREEMAIN DATA(AREA1) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing DATA/DATAPOINTER on EXEC CICS FREEMAIN", () => {
    const text = "EXEC CICS FREEMAIN END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(true);
  });

  // ── LOAD ───────────────────────────────────────────────────────────────
  it("accepts EXEC CICS LOAD PROGRAM SET HOLD", () => {
    const text = "EXEC CICS LOAD PROGRAM(TB1) SET(PTR) HOLD END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing PROGRAM on EXEC CICS LOAD", () => {
    const text = "EXEC CICS LOAD SET(PTR) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("PROGRAM"))).toBe(true);
  });

  // ── RELEASE ────────────────────────────────────────────────────────────
  it("accepts EXEC CICS RELEASE PROGRAM", () => {
    const text = "EXEC CICS RELEASE PROGRAM(PGM1) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing PROGRAM on EXEC CICS RELEASE", () => {
    const text = "EXEC CICS RELEASE END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("PROGRAM"))).toBe(true);
  });

  // ── RETRIEVE ───────────────────────────────────────────────────────────
  it("accepts EXEC CICS RETRIEVE INTO LENGTH", () => {
    const text = "EXEC CICS RETRIEVE INTO(AREA) LENGTH(LN) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing INTO/SET on EXEC CICS RETRIEVE", () => {
    const text = "EXEC CICS RETRIEVE LENGTH(LN) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(true);
  });

  // ── SUSPEND ────────────────────────────────────────────────────────────
  it("accepts EXEC CICS SUSPEND without options", () => {
    const text = "EXEC CICS SUSPEND END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  // ── POST ───────────────────────────────────────────────────────────────
  it("accepts EXEC CICS POST with SET INTERVAL", () => {
    const text = "EXEC CICS POST SET(EC) INTERVAL(003000) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("flags missing SET on EXEC CICS POST", () => {
    const text = "EXEC CICS POST INTERVAL(003000) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("SET"))).toBe(true);
  });

  // ── ENQ ────────────────────────────────────────────────────────────────
  it("accepts EXEC CICS ENQ RESOURCE LENGTH", () => {
    const text = "EXEC CICS ENQ RESOURCE(RES1) LENGTH(8) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("flags missing RESOURCE on EXEC CICS ENQ", () => {
    const text = "EXEC CICS ENQ LENGTH(8) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("RESOURCE"))).toBe(true);
  });

  // ── DEQ ────────────────────────────────────────────────────────────────
  it("accepts EXEC CICS DEQ RESOURCE", () => {
    const text = "EXEC CICS DEQ RESOURCE(RES1) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("flags missing RESOURCE on EXEC CICS DEQ", () => {
    const text = "EXEC CICS DEQ LENGTH(8) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("RESOURCE"))).toBe(true);
  });

  // ── READQ TD ───────────────────────────────────────────────────────────
  it("accepts EXEC CICS READQ TD QUEUE INTO", () => {
    const text = "EXEC CICS READQ TD QUEUE(TDQN) INTO(AREA) LENGTH(LN) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing QUEUE on EXEC CICS READQ TD", () => {
    const text = "EXEC CICS READQ TD INTO(AREA) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("QUEUE"))).toBe(true);
  });

  // ── READQ TS ───────────────────────────────────────────────────────────
  it("accepts EXEC CICS READQ TS QUEUE INTO ITEM", () => {
    const text = "EXEC CICS READQ TS QUEUE(TSQ1) INTO(AREA) ITEM(1) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── WRITEQ TD ──────────────────────────────────────────────────────────
  it("accepts EXEC CICS WRITEQ TD QUEUE FROM", () => {
    const text = "EXEC CICS WRITEQ TD QUEUE(TDQN) FROM(AREA) LENGTH(LN) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing FROM on EXEC CICS WRITEQ TD", () => {
    const text = "EXEC CICS WRITEQ TD QUEUE(Q1) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("FROM"))).toBe(true);
  });

  // ── WRITEQ TS ──────────────────────────────────────────────────────────
  it("accepts EXEC CICS WRITEQ TS QUEUE FROM MAIN", () => {
    const text = "EXEC CICS WRITEQ TS QUEUE(TSQ1) FROM(DATA) MAIN END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("flags missing QUEUE on EXEC CICS WRITEQ TS", () => {
    const text = "EXEC CICS WRITEQ TS FROM(DATA) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("QUEUE"))).toBe(true);
  });

  // ── DELETEQ TD ─────────────────────────────────────────────────────────
  it("accepts EXEC CICS DELETEQ TD QUEUE", () => {
    const text = "EXEC CICS DELETEQ TD QUEUE(Q1) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing QUEUE on EXEC CICS DELETEQ TD", () => {
    const text = "EXEC CICS DELETEQ TD END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("QUEUE"))).toBe(true);
  });

  // ── DELETEQ TS ─────────────────────────────────────────────────────────
  it("accepts EXEC CICS DELETEQ TS QUEUE SYSID", () => {
    const text = "EXEC CICS DELETEQ TS QUEUE(Q1) SYSID(S1) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── HANDLE ABEND ───────────────────────────────────────────────────────
  it("accepts EXEC CICS HANDLE ABEND PROGRAM", () => {
    const text = "EXEC CICS HANDLE ABEND PROGRAM(ERRPGM) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("accepts EXEC CICS HANDLE ABEND CANCEL", () => {
    const text = "EXEC CICS HANDLE ABEND CANCEL END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags EXEC CICS HANDLE ABEND without action", () => {
    const text = "EXEC CICS HANDLE ABEND END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(true);
  });

  // ── IGNORE CONDITION ───────────────────────────────────────────────────
  it("accepts EXEC CICS IGNORE CONDITION with conditions", () => {
    const text = "EXEC CICS IGNORE CONDITION LENGERR INVREQ END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_CONDITION_UNKNOWN")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_CONDITION_MISSING")).toBe(false);
  });

  it("flags EXEC CICS IGNORE CONDITION without conditions", () => {
    const text = "EXEC CICS IGNORE CONDITION END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_CONDITION_MISSING")).toBe(true);
  });

  it("flags unknown condition in EXEC CICS IGNORE CONDITION", () => {
    const text = "EXEC CICS IGNORE CONDITION XYZFOO END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_CONDITION_UNKNOWN")).toBe(true);
  });

  // ── SYNCPOINT ──────────────────────────────────────────────────────────
  it("accepts EXEC CICS SYNCPOINT without options", () => {
    const text = "EXEC CICS SYNCPOINT END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── SYNCPOINT ROLLBACK ─────────────────────────────────────────────────
  it("accepts EXEC CICS SYNCPOINT ROLLBACK", () => {
    const text = "EXEC CICS SYNCPOINT ROLLBACK END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── ISSUE ERASE / ISSUE ERASEAUP ──────────────────────────────────────
  it("accepts EXEC CICS ISSUE ERASE", () => {
    const text = "EXEC CICS ISSUE ERASE END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("accepts EXEC CICS ISSUE ERASEAUP", () => {
    const text = "EXEC CICS ISSUE ERASEAUP END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── SEND PAGE ──────────────────────────────────────────────────────────
  it("accepts EXEC CICS SEND PAGE RELEASE", () => {
    const text = "EXEC CICS SEND PAGE RELEASE END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── PURGE MESSAGE ──────────────────────────────────────────────────────
  it("accepts EXEC CICS PURGE MESSAGE", () => {
    const text = "EXEC CICS PURGE MESSAGE END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── PUSH / POP HANDLE ─────────────────────────────────────────────────
  it("accepts EXEC CICS PUSH HANDLE", () => {
    const text = "EXEC CICS PUSH HANDLE END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  it("accepts EXEC CICS POP HANDLE", () => {
    const text = "EXEC CICS POP HANDLE END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── WAIT EVENT ─────────────────────────────────────────────────────────
  it("accepts EXEC CICS WAIT EVENT ECADDR", () => {
    const text = "EXEC CICS WAIT EVENT ECADDR(EC) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing ECADDR on EXEC CICS WAIT EVENT", () => {
    const text = "EXEC CICS WAIT EVENT END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("ECADDR"))).toBe(true);
  });

  // ── DUMP TRANSACTION ───────────────────────────────────────────────────
  it("accepts EXEC CICS DUMP TRANSACTION DUMPCODE", () => {
    const text = "EXEC CICS DUMP TRANSACTION DUMPCODE(DC01) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing DUMPCODE on EXEC CICS DUMP TRANSACTION", () => {
    const text = "EXEC CICS DUMP TRANSACTION END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("DUMPCODE"))).toBe(true);
  });

  // ── ENTER TRACENUM ─────────────────────────────────────────────────────
  it("accepts EXEC CICS ENTER TRACENUM without options", () => {
    const text = "EXEC CICS ENTER TRACENUM FROM(DATA) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
  });

  // ── WRITE OPERATOR ─────────────────────────────────────────────────────
  it("accepts EXEC CICS WRITE OPERATOR TEXT", () => {
    const text = "EXEC CICS WRITE OPERATOR TEXT(MSG1) TEXTLENGTH(80) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_NOT_ALLOWED")).toBe(false);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING")).toBe(false);
  });

  it("flags missing TEXT on EXEC CICS WRITE OPERATOR", () => {
    const text = "EXEC CICS WRITE OPERATOR TEXTLENGTH(80) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_CICS_OPTION_MISSING" && d.message.includes("TEXT"))).toBe(true);
  });
});

// ======================= resolveDliRequest =======================

describe("resolveDliRequest", () => {
  function mkTokens(text: string) {
    return text.split(/\s+/).map((w, i) => ({
      upper: w.toUpperCase(),
      start: i * 10,
      end: i * 10 + w.length,
    }));
  }

  it("resolves short-form GU", () => {
    const r = resolveDliRequest(mkTokens("DLI GU USING PCB(1)"));
    expect(r.request).toBe("GU");
    expect(r.requestTokenCount).toBe(1);
  });

  it("resolves long-form GET UNIQUE → GU", () => {
    const r = resolveDliRequest(mkTokens("DLI GET UNIQUE USING PCB(1)"));
    expect(r.request).toBe("GU");
    expect(r.requestTokenCount).toBe(2);
  });

  it("resolves GET NEXT → GN", () => {
    const r = resolveDliRequest(mkTokens("DLI GET NEXT"));
    expect(r.request).toBe("GN");
  });

  it("resolves GET NEXT IN PARENT → GNP", () => {
    const r = resolveDliRequest(mkTokens("DLI GET NEXT IN PARENT"));
    expect(r.request).toBe("GNP");
    expect(r.requestTokenCount).toBe(4);
  });

  it("resolves GET HOLD UNIQUE → GHU", () => {
    const r = resolveDliRequest(mkTokens("DLI GET HOLD UNIQUE"));
    expect(r.request).toBe("GHU");
    expect(r.requestTokenCount).toBe(3);
  });

  it("resolves GET HOLD NEXT → GHN", () => {
    const r = resolveDliRequest(mkTokens("DLI GET HOLD NEXT"));
    expect(r.request).toBe("GHN");
  });

  it("resolves GET HOLD NEXT IN PARENT → GHNP", () => {
    const r = resolveDliRequest(mkTokens("DLI GET HOLD NEXT IN PARENT"));
    expect(r.request).toBe("GHNP");
    expect(r.requestTokenCount).toBe(5);
  });

  it("resolves INSERT → ISRT", () => {
    const r = resolveDliRequest(mkTokens("DLI INSERT SEGMENT(S1)"));
    expect(r.request).toBe("ISRT");
  });

  it("resolves REPLACE → REPL", () => {
    const r = resolveDliRequest(mkTokens("DLI REPLACE"));
    expect(r.request).toBe("REPL");
  });

  it("resolves DELETE → DLET", () => {
    const r = resolveDliRequest(mkTokens("DLI DELETE"));
    expect(r.request).toBe("DLET");
  });

  it("resolves LOAD", () => {
    const r = resolveDliRequest(mkTokens("DLI LOAD"));
    expect(r.request).toBe("LOAD");
  });

  it("resolves CHECKPOINT → CHKP", () => {
    const r = resolveDliRequest(mkTokens("DLI CHECKPOINT ID(X)"));
    expect(r.request).toBe("CHKP");
  });

  it("resolves SCHEDULE → SCHD", () => {
    const r = resolveDliRequest(mkTokens("DLI SCHEDULE PSB(MYPSB)"));
    expect(r.request).toBe("SCHD");
  });

  it("resolves TERMINATE → TERM", () => {
    const r = resolveDliRequest(mkTokens("DLI TERMINATE"));
    expect(r.request).toBe("TERM");
  });

  it("resolves short-form CHKP", () => {
    const r = resolveDliRequest(mkTokens("DLI CHKP ID(X)"));
    expect(r.request).toBe("CHKP");
  });

  it("resolves short-form SCHD", () => {
    const r = resolveDliRequest(mkTokens("DLI SCHD PSB(P)"));
    expect(r.request).toBe("SCHD");
  });

  it("resolves short-form TERM", () => {
    const r = resolveDliRequest(mkTokens("DLI TERM"));
    expect(r.request).toBe("TERM");
  });
});

// ======================= validateDliClausesByRequest =======================

describe("validateDliClausesByRequest", () => {
  type DiagResult = { code?: string | number; message: string };

  function mkTokens(text: string) {
    const result: { upper: string; start: number; end: number }[] = [];
    const re = /[A-Za-z0-9]+(-[A-Za-z0-9]+)*/g;
    let m: RegExpExecArray | null;
    while ((m = re.exec(text)) !== null) {
      result.push({ upper: m[0].toUpperCase(), start: m.index, end: m.index + m[0].length });
    }
    return result;
  }

  function validate(request: string, reqTokenCount: number, clauseText: string): DiagResult[] {
    const tokens = mkTokens(`DLI ${clauseText}`);
    const diags: DiagResult[] = [];
    validateDliClausesByRequest(request, reqTokenCount, tokens, diags as any);
    return diags;
  }

  // --- GU ---

  it("GU: valid with USING SEGMENT INTO", () => {
    const diags = validate("GU", 1, "GU USING PCB(1) SEGMENT(S1) INTO(A)");
    expect(diags.filter((d) => d.code?.toString().startsWith("EXEC_DLI"))).toHaveLength(0);
  });

  it("GU: missing SEGMENT reports error", () => {
    const diags = validate("GU", 1, "GU USING PCB(1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("SEGMENT"))).toBe(true);
  });

  it("GU: USING PCB is optional (no error)", () => {
    const diags = validate("GU", 1, "GU SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("USING"))).toBe(false);
  });

  it("GU: PSB not allowed", () => {
    const diags = validate("GU", 1, "GU SEGMENT(S1) INTO(A) PSB(P)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_NOT_ALLOWED" && d.message.includes("PSB"))).toBe(true);
  });

  it("GU: KEYFEEDBACK is allowed", () => {
    const diags = validate("GU", 1, "GU KEYFEEDBACK(K) FEEDBACKLEN(80) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_NOT_ALLOWED")).toBe(false);
  });

  it("GU: LOCKED is allowed for GET", () => {
    const diags = validate("GU", 1, "GU SEGMENT(S1) INTO(A) LOCKED");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_NOT_ALLOWED")).toBe(false);
  });

  // --- GN ---

  it("GN: valid without SEGMENT (unqualified)", () => {
    const diags = validate("GN", 1, "GN USING PCB(1)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING")).toBe(false);
  });

  it("GN: valid with SEGMENT", () => {
    const diags = validate("GN", 1, "GN SEGMENT(S1) INTO(A)");
    expect(diags.filter((d) => d.code?.toString().startsWith("EXEC_DLI"))).toHaveLength(0);
  });

  // --- ISRT ---

  it("ISRT: valid with SEGMENT and FROM", () => {
    const diags = validate("ISRT", 1, "ISRT SEGMENT(S1) FROM(A)");
    expect(diags.filter((d) => d.code?.toString().startsWith("EXEC_DLI"))).toHaveLength(0);
  });

  it("ISRT: missing SEGMENT", () => {
    const diags = validate("ISRT", 1, "ISRT FROM(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("SEGMENT"))).toBe(true);
  });

  it("ISRT: missing FROM", () => {
    const diags = validate("ISRT", 1, "ISRT SEGMENT(S1)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("FROM"))).toBe(true);
  });

  it("ISRT: INTO not allowed", () => {
    const diags = validate("ISRT", 1, "ISRT SEGMENT(S1) FROM(A) INTO(B)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_NOT_ALLOWED" && d.message.includes("INTO"))).toBe(true);
  });

  // --- REPL ---

  it("REPL: valid with FROM", () => {
    const diags = validate("REPL", 1, "REPL FROM(A)");
    expect(diags.filter((d) => d.code?.toString().startsWith("EXEC_DLI"))).toHaveLength(0);
  });

  it("REPL: WHERE not allowed", () => {
    const diags = validate("REPL", 1, "REPL FROM(A) WHERE(X=1)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_NOT_ALLOWED" && d.message.includes("WHERE"))).toBe(true);
  });

  // --- DLET ---

  it("DLET: valid with SEGMENT FROM", () => {
    const diags = validate("DLET", 1, "DLET SEGMENT(S1) FROM(A)");
    expect(diags.filter((d) => d.code?.toString().startsWith("EXEC_DLI"))).toHaveLength(0);
  });

  it("DLET: missing FROM+SEGMENT", () => {
    const diags = validate("DLET", 1, "DLET");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("SEGMENT"))).toBe(true);
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("FROM"))).toBe(true);
  });

  // --- LOAD ---

  it("LOAD: valid with SEGMENT FROM", () => {
    const diags = validate("LOAD", 1, "LOAD SEGMENT(S1) FROM(A)");
    expect(diags.filter((d) => d.code?.toString().startsWith("EXEC_DLI"))).toHaveLength(0);
  });

  it("LOAD: missing SEGMENT", () => {
    const diags = validate("LOAD", 1, "LOAD FROM(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("SEGMENT"))).toBe(true);
  });

  it("LOAD: INTO not allowed", () => {
    const diags = validate("LOAD", 1, "LOAD SEGMENT(S1) FROM(A) INTO(B)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_NOT_ALLOWED" && d.message.includes("INTO"))).toBe(true);
  });

  // --- CHKP ---

  it("CHKP: valid with ID", () => {
    const diags = validate("CHKP", 1, "CHKP ID(X)");
    expect(diags.filter((d) => d.code?.toString().startsWith("EXEC_DLI"))).toHaveLength(0);
  });

  it("CHKP: missing ID", () => {
    const diags = validate("CHKP", 1, "CHKP");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("ID"))).toBe(true);
  });

  it("CHKP: SEGMENT not allowed", () => {
    const diags = validate("CHKP", 1, "CHKP ID(X) SEGMENT(S)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_NOT_ALLOWED" && d.message.includes("SEGMENT"))).toBe(true);
  });

  // --- SCHD ---

  it("SCHD: valid with PSB", () => {
    const diags = validate("SCHD", 1, "SCHD PSB(MYPSB)");
    expect(diags.filter((d) => d.code?.toString().startsWith("EXEC_DLI"))).toHaveLength(0);
  });

  it("SCHD: missing PSB", () => {
    const diags = validate("SCHD", 1, "SCHD");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("PSB"))).toBe(true);
  });

  it("SCHD: SEGMENT not allowed", () => {
    const diags = validate("SCHD", 1, "SCHD PSB(P) SEGMENT(S)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_NOT_ALLOWED" && d.message.includes("SEGMENT"))).toBe(true);
  });

  // --- TERM ---

  it("TERM: valid empty", () => {
    const diags = validate("TERM", 1, "TERM");
    expect(diags.filter((d) => d.code?.toString().startsWith("EXEC_DLI"))).toHaveLength(0);
  });

  it("TERM: any clause not allowed", () => {
    const diags = validate("TERM", 1, "TERM SEGMENT(S1)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_NOT_ALLOWED")).toBe(true);
  });

  // --- Semantic rules ---

  it("FIRST and LAST mutual exclusivity", () => {
    const diags = validate("GU", 1, "GU SEGMENT(S1) INTO(A) FIRST LAST");
    expect(diags.some((d) => d.code === "EXEC_DLI_FIRST_LAST_EXCLUSIVE")).toBe(true);
  });

  it("LOCKED without INTO", () => {
    const diags = validate("GN", 1, "GN SEGMENT(S1) LOCKED");
    expect(diags.some((d) => d.code === "EXEC_DLI_LOCKED_WITHOUT_INTO")).toBe(true);
  });

  it("LOCKED with INTO is fine", () => {
    const diags = validate("GN", 1, "GN SEGMENT(S1) INTO(A) LOCKED");
    expect(diags.some((d) => d.code === "EXEC_DLI_LOCKED_WITHOUT_INTO")).toBe(false);
  });

  it("FEEDBACKLEN without KEYFEEDBACK", () => {
    const diags = validate("GU", 1, "GU FEEDBACKLEN(80) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_FEEDBACKLEN_WITHOUT_KEYFEEDBACK")).toBe(true);
  });

  it("FEEDBACKLEN with KEYFEEDBACK is fine", () => {
    const diags = validate("GU", 1, "GU KEYFEEDBACK(K) FEEDBACKLEN(80) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_FEEDBACKLEN_WITHOUT_KEYFEEDBACK")).toBe(false);
  });

  // --- Unknown clause keywords ---

  it("flags unknown clause keyword KEYY", () => {
    const diags = validate("ISRT", 1, "ISRT USING PCB(5) VARIABLE KEYY SEGMENT(GV) FROM(V-SEGM)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_UNKNOWN" && d.message.includes("KEYY"))).toBe(true);
  });

  it("does not flag valid clause keywords as unknown", () => {
    const diags = validate("ISRT", 1, "ISRT USING PCB(5) VARIABLE SEGMENT(GV) FROM(V-SEGM)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_UNKNOWN")).toBe(false);
  });

  it("does not flag data names with hyphens as unknown clauses", () => {
    const diags = validate("ISRT", 1, "ISRT SEGMENT(GV) FROM(V-SEGM)");
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_UNKNOWN")).toBe(false);
  });

  it("does not flag data names inside WHERE() as unknown clauses", () => {
    const text = "EXEC DLI GU USING PCB(1) WHERE(DKEY = XDKEY) FIELDLENGTH(16) SEGMENT(S1) INTO(A) SEGLENGTH(2000) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_UNKNOWN")).toBe(false);
  });
});

// ======================= lintPreprocessed EXEC DLI integration =======================

describe("lintPreprocessed – EXEC DLI", () => {
  it("accepts valid EXEC DLI GU block", () => {
    const text = "EXEC DLI GU USING PCB(1) SEGMENT(S1) INTO(A) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code?.toString().startsWith("EXEC_DLI"))).toBe(false);
  });

  it("accepts EXEC DLI TERM (no clauses)", () => {
    const text = "EXEC DLI TERM END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code?.toString().startsWith("EXEC_DLI"))).toBe(false);
  });

  it("accepts EXEC DLI CHKP ID(X)", () => {
    const text = "EXEC DLI CHKP ID(X) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code?.toString().startsWith("EXEC_DLI"))).toBe(false);
  });

  it("accepts EXEC DLI SCHD PSB(P)", () => {
    const text = "EXEC DLI SCHD PSB(MYPSB) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code?.toString().startsWith("EXEC_DLI"))).toBe(false);
  });

  it("flags EXEC DLI SCHD without PSB", () => {
    const text = "EXEC DLI SCHD END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("PSB"))).toBe(true);
  });

  it("flags EXEC DLI CHKP without ID", () => {
    const text = "EXEC DLI CHKP END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code === "EXEC_DLI_CLAUSE_MISSING" && d.message.includes("ID"))).toBe(true);
  });

  it("accepts long-form GET UNIQUE", () => {
    const text = "EXEC DLI GET UNIQUE SEGMENT(S1) INTO(A) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code?.toString().startsWith("EXEC_DLI"))).toBe(false);
  });

  it("accepts long-form SCHEDULE PSB", () => {
    const text = "EXEC DLI SCHEDULE PSB(P) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code?.toString().startsWith("EXEC_DLI"))).toBe(false);
  });

  it("accepts long-form LOAD", () => {
    const text = "EXEC DLI LOAD SEGMENT(S1) FROM(A) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code?.toString().startsWith("EXEC_DLI"))).toBe(false);
  });

  it("accepts long-form CHECKPOINT ID", () => {
    const text = "EXEC DLI CHECKPOINT ID(X) END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code?.toString().startsWith("EXEC_DLI"))).toBe(false);
  });

  it("accepts long-form TERMINATE", () => {
    const text = "EXEC DLI TERMINATE END-EXEC.";
    const diags = lintPreprocessed(text);
    expect(diags.some((d) => d.code?.toString().startsWith("EXEC_DLI"))).toBe(false);
  });
});

// ======================= EXEC DLI clause ordering =======================

describe("validateDliClauseOrdering", () => {
  // Helper: wraps token string in EXEC DLI ... END-EXEC and calls lintPreprocessed
  function orderDiags(tokens: string) {
    const text = `EXEC DLI ${tokens} END-EXEC.`;
    return lintPreprocessed(text).filter((d) => d.code?.toString().startsWith("EXEC_DLI_"));
  }

  // --- USING PCB before SEGMENT ---

  it("accepts USING PCB before SEGMENT", () => {
    const diags = orderDiags("GU USING PCB(1) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_USING_AFTER_SEGMENT")).toBe(false);
  });

  it("flags USING PCB after SEGMENT", () => {
    const diags = orderDiags("GU SEGMENT(S1) INTO(A) USING PCB(1)");
    expect(diags.some((d) => d.code === "EXEC_DLI_USING_AFTER_SEGMENT")).toBe(true);
  });

  // --- KEYFEEDBACK before SEGMENT ---

  it("accepts KEYFEEDBACK before SEGMENT (GET)", () => {
    const diags = orderDiags("GU KEYFEEDBACK(K) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_KEYFEEDBACK_AFTER_SEGMENT")).toBe(false);
  });

  it("flags KEYFEEDBACK after SEGMENT (GET)", () => {
    const diags = orderDiags("GU SEGMENT(S1) INTO(A) KEYFEEDBACK(K)");
    expect(diags.some((d) => d.code === "EXEC_DLI_KEYFEEDBACK_AFTER_SEGMENT")).toBe(true);
  });

  // --- FIRST/LAST/VARIABLE before SEGMENT ---

  it("accepts VARIABLE before SEGMENT", () => {
    const diags = orderDiags("GU USING PCB(4) SEGMENT(GR) WHERE(RKEY = K) FIELDLENGTH(13) VARIABLE SEGMENT(GD) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_PREFIX_AFTER_SEGMENT")).toBe(false);
  });

  it("accepts FIRST VARIABLE SEGMENT", () => {
    const diags = orderDiags("GU FIRST VARIABLE SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_PREFIX_AFTER_SEGMENT")).toBe(false);
  });

  it("accepts VARIABLE after last SEGMENT (in segment block)", () => {
    const diags = orderDiags("GU SEGMENT(S1) VARIABLE INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_PREFIX_AFTER_SEGMENT")).toBe(false);
  });

  // --- INTO/FROM after SEGMENT ---

  it("accepts INTO after SEGMENT", () => {
    const diags = orderDiags("GU SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_TRANSFER_BEFORE_SEGMENT")).toBe(false);
  });

  it("flags FROM before any SEGMENT", () => {
    const diags = orderDiags("ISRT FROM(A) SEGMENT(S1)");
    expect(diags.some((d) => d.code === "EXEC_DLI_TRANSFER_BEFORE_SEGMENT")).toBe(true);
  });

  // --- Multi-segment block with correct ordering ---

  it("accepts multi-segment GU with correct ordering", () => {
    const diags = orderDiags(
      "GU USING PCB(4) SEGMENT(GR) WHERE(RKEY = K) FIELDLENGTH(13) VARIABLE SEGMENT(GD) WHERE(DKEY = D) FIELDLENGTH(16) INTO(A) SEGLENGTH(4000)"
    );
    const orderCodes = diags.filter((d) =>
      ["EXEC_DLI_USING_AFTER_SEGMENT", "EXEC_DLI_KEYFEEDBACK_AFTER_SEGMENT",
       "EXEC_DLI_PREFIX_AFTER_SEGMENT", "EXEC_DLI_TRANSFER_BEFORE_SEGMENT"].includes(d.code?.toString() ?? "")
    );
    expect(orderCodes).toHaveLength(0);
  });

  // --- Duplicate USING ---

  it("flags duplicate USING PCB", () => {
    const diags = orderDiags("GU USING PCB(1) USING PCB(2) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_DUPLICATE_USING")).toBe(true);
  });

  it("accepts single USING PCB", () => {
    const diags = orderDiags("GU USING PCB(1) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_DUPLICATE_USING")).toBe(false);
  });

  // --- Duplicate KEYFEEDBACK ---

  it("flags duplicate KEYFEEDBACK", () => {
    const diags = orderDiags("GU KEYFEEDBACK(K1) KEYFEEDBACK(K2) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_DUPLICATE_KEYFEEDBACK")).toBe(true);
  });

  it("accepts single KEYFEEDBACK", () => {
    const diags = orderDiags("GU KEYFEEDBACK(K) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_DUPLICATE_KEYFEEDBACK")).toBe(false);
  });

  // --- WHERE before SEGMENT ---

  it("flags WHERE before any SEGMENT", () => {
    const diags = orderDiags("GU WHERE(K = V) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_WHERE_BEFORE_SEGMENT")).toBe(true);
  });

  it("accepts WHERE after SEGMENT", () => {
    const diags = orderDiags("GU SEGMENT(S1) WHERE(K = V) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_WHERE_BEFORE_SEGMENT")).toBe(false);
  });

  // --- LOCKED positional check ---

  it("flags LOCKED without INTO in segment block", () => {
    const diags = orderDiags("GU SEGMENT(S1) FROM(A) LOCKED");
    expect(diags.some((d) => d.code === "EXEC_DLI_LOCKED_POSITION")).toBe(true);
  });

  it("accepts LOCKED after INTO in segment block", () => {
    const diags = orderDiags("GU SEGMENT(S1) INTO(A) LOCKED");
    expect(diags.some((d) => d.code === "EXEC_DLI_LOCKED_POSITION")).toBe(false);
  });

  // --- OFFSET/SEGLENGTH without transfer ---

  it("flags OFFSET without INTO/FROM in segment block", () => {
    const diags = orderDiags("GU SEGMENT(S1) OFFSET(0)");
    expect(diags.some((d) => d.code === "EXEC_DLI_OFFSET_WITHOUT_TRANSFER")).toBe(true);
  });

  it("accepts OFFSET after INTO in segment block", () => {
    const diags = orderDiags("GU SEGMENT(S1) INTO(A) OFFSET(0)");
    expect(diags.some((d) => d.code === "EXEC_DLI_OFFSET_WITHOUT_TRANSFER")).toBe(false);
  });

  it("flags SEGLENGTH without INTO/FROM in segment block", () => {
    const diags = orderDiags("GU SEGMENT(S1) SEGLENGTH(80)");
    expect(diags.some((d) => d.code === "EXEC_DLI_SEGLENGTH_WITHOUT_TRANSFER")).toBe(true);
  });

  it("accepts SEGLENGTH after FROM in segment block", () => {
    const diags = orderDiags("ISRT SEGMENT(S1) FROM(A) SEGLENGTH(80)");
    expect(diags.some((d) => d.code === "EXEC_DLI_SEGLENGTH_WITHOUT_TRANSFER")).toBe(false);
  });

  // --- FEEDBACKLEN after SEGMENT ---

  it("flags FEEDBACKLEN after first SEGMENT", () => {
    const diags = orderDiags("GU SEGMENT(S1) INTO(A) FEEDBACKLEN(80)");
    expect(diags.some((d) => d.code === "EXEC_DLI_FEEDBACKLEN_AFTER_SEGMENT")).toBe(true);
  });

  it("accepts FEEDBACKLEN before SEGMENT", () => {
    const diags = orderDiags("GU KEYFEEDBACK(K) FEEDBACKLEN(80) SEGMENT(S1) INTO(A)");
    expect(diags.some((d) => d.code === "EXEC_DLI_FEEDBACKLEN_AFTER_SEGMENT")).toBe(false);
  });
});

// ======================= lintUndefinedIdentifiers =======================

describe("lintUndefinedIdentifiers", () => {
  /** Build a minimal fixed-format COBOL program with given PROCEDURE code lines */
  function mkCobol(procLines: string[], dataNames: string[] = [], paragraphs: string[] = []): string {
    const pad = (s: string) => "       " + s.padEnd(65);
    const lines = [
      pad("IDENTIFICATION DIVISION."),
      pad("PROGRAM-ID. TESTPROG."),
      pad("DATA DIVISION."),
      pad("WORKING-STORAGE SECTION."),
      ...dataNames.map((n) => pad(`01  ${n}  PIC X(10).`)),
      pad("PROCEDURE DIVISION."),
      ...paragraphs.map((p) => pad(`${p}.`)),
      ...procLines.map((l) => pad(l)),
      pad("STOP RUN."),
    ];
    return lines.join("\n");
  }

  /** Build a DefinitionIndex with the given names */
  function mkIndex(dataNames: string[] = [], paragraphs: string[] = [], sections: string[] = []): DefinitionIndex {
    return {
      dataItems: dataNames.map((name) => ({
        name: name.toUpperCase(),
        level: 1,
        line: 0,
        character: 0,
        endCharacter: name.length,
        parentNames: [],
      })),
      paragraphs: paragraphs.map((name) => ({
        name: name.toUpperCase(),
        line: 0,
        character: 0,
        endCharacter: name.length,
      })),
      sections: sections.map((name) => ({
        name: name.toUpperCase(),
        line: 0,
        character: 0,
        endCharacter: name.length,
      })),
    };
  }

  it("does not flag defined data names", () => {
    const text = mkCobol(["MOVE MY-VAR TO OTHER-VAR."], ["MY-VAR", "OTHER-VAR"]);
    const index = mkIndex(["MY-VAR", "OTHER-VAR"]);
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("flags undefined data names", () => {
    const text = mkCobol(["MOVE VK-UPDATERR TO WS-FLAG."], ["WS-FLAG"]);
    const index = mkIndex(["WS-FLAG"]);
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    expect(undef).toHaveLength(1);
    expect(undef[0].message).toContain("VK-UPDATERR");
  });

  it("does not flag COBOL reserved words", () => {
    const text = mkCobol(["MOVE SPACES TO WS-OUT."], ["WS-OUT"]);
    const index = mkIndex(["WS-OUT"]);
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("does not flag COBOL verbs", () => {
    const text = mkCobol(["DISPLAY WS-OUT."], ["WS-OUT"]);
    const index = mkIndex(["WS-OUT"]);
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("does not flag DL/I Interface Block (DIB) fields", () => {
    const text = mkCobol([
      "IF DIBSTAT NOT = SPACE",
      "   IF DIBSEGM = 'GR'",
      "      DISPLAY DIBKFBL",
      "   END-IF",
      "END-IF.",
    ]);
    const index = mkIndex();
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("skips DLI/CICS keywords inside EXEC blocks but checks data names in arguments", () => {
    const text = mkCobol([
      "EXEC DLI GU SEGMENT(S1) INTO(A) END-EXEC.",
    ]);
    // S1 is a DLI segment name (not a COBOL data name) — not checked.
    // A is a host variable inside INTO() — must be defined.
    const index = mkIndex(["A"]);
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("flags undefined data names inside EXEC DLI argument parens", () => {
    const text = mkCobol([
      "EXEC DLI GU SEGMENT(S1) INTO(UNDEF-AREA) END-EXEC.",
    ]);
    // S1 = DLI segment name, not checked. UNDEF-AREA = host var in INTO(), must be flagged.
    const index = mkIndex([]);
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    expect(undef).toHaveLength(1);
    expect(undef[0].message).toContain("UNDEF-AREA");
  });

  it("flags undefined host-var (ref) inside WHERE but not the DLI field name", () => {
    const lines = [
      "       IDENTIFICATION DIVISION.",
      "       PROGRAM-ID. TESTPROG.",
      "       PROCEDURE DIVISION.",
      "           EXEC DLI GU USING PCB(1)",
      "                WHERE(DKEY = XDKE)",
      "                FIELDLENGTH(16)",
      "                SEGMENT(S1) INTO(A) SEGLENGTH(200)",
      "           END-EXEC.",
    ];
    const text = lines.join("\n");
    // DKEY is a DLI segment field name — NOT a COBOL data name.
    // Only XDKE (the host-variable ref) should be flagged.
    // S1 = DLI segment name (not checked), A = host var in INTO().
    const index = mkIndex(["A"]);
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    expect(undef).toHaveLength(1);
    expect(undef[0].message).toContain("XDKE");
    // DKEY (field name, left side) must NOT be flagged
    expect(undef.some((d) => d.message.includes("DKEY"))).toBe(false);
  });

  it("does not flag defined host-var inside WHERE()", () => {
    const lines = [
      "       IDENTIFICATION DIVISION.",
      "       PROGRAM-ID. TESTPROG.",
      "       PROCEDURE DIVISION.",
      "           EXEC DLI GU USING PCB(1)",
      "                WHERE(DKEY = XDKEY)",
      "                FIELDLENGTH(16)",
      "                SEGMENT(S1) INTO(A) SEGLENGTH(200)",
      "           END-EXEC.",
    ];
    const text = lines.join("\n");
    // DKEY is a DLI field name — not needed in index.
    // XDKEY is the host variable (ref) — must be defined.
    const index = mkIndex(["XDKEY", "A"]);
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    expect(undef).toHaveLength(0);
  });

  it("WHERE with AND/OR: only flags undefined refs, not field names", () => {
    const lines = [
      "       IDENTIFICATION DIVISION.",
      "       PROGRAM-ID. TESTPROG.",
      "       PROCEDURE DIVISION.",
      "           EXEC DLI GU USING PCB(1)",
      "                WHERE(AKEY = B0-AKEY AND BKEY = UNDEF-REF)",
      "                FIELDLENGTH(7)",
      "                SEGMENT(S1) INTO(A) SEGLENGTH(200)",
      "           END-EXEC.",
    ];
    const text = lines.join("\n");
    const index = mkIndex(["B0-AKEY", "A"]);
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    // Only UNDEF-REF (right side) should be flagged
    expect(undef).toHaveLength(1);
    expect(undef[0].message).toContain("UNDEF-REF");
    // Field names AKEY and BKEY (left side) must NOT be flagged
    expect(undef.some((d) => d.message.includes("AKEY"))).toBe(false);
    expect(undef.some((d) => d.message.includes("BKEY"))).toBe(false);
  });

  it("skips paragraph/section label lines", () => {
    const text = mkCobol([], [], ["MAIN-PARA"]);
    const index = mkIndex([], ["MAIN-PARA"]);
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("skips numeric tokens", () => {
    const text = mkCobol(["MOVE 42 TO WS-COUNT."], ["WS-COUNT"]);
    const index = mkIndex(["WS-COUNT"]);
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("skips tokens inside string literals", () => {
    const text = mkCobol(["DISPLAY 'HELLO-WORLD'."], []);
    const index = mkIndex();
    const diags = lintUndefinedIdentifiers(text, index);
    // HELLO-WORLD is inside a string, should not be flagged
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("warns only on first occurrence of each undefined name", () => {
    const text = mkCobol([
      "MOVE UNDEF-X TO WS-A.",
      "MOVE UNDEF-X TO WS-B.",
    ], ["WS-A", "WS-B"]);
    const index = mkIndex(["WS-A", "WS-B"]);
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    expect(undef).toHaveLength(1);
    expect(undef[0].message).toContain("UNDEF-X");
  });

  it("does not flag FILLER", () => {
    const text = mkCobol(["MOVE FILLER TO WS-X."], ["WS-X"]);
    const index = mkIndex(["WS-X"]);
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("does not flag identifiers before PROCEDURE DIVISION", () => {
    const text = mkCobol([], ["SOME-DATA"]);
    const index = mkIndex(["SOME-DATA"]);
    // No PROCEDURE DIVISION code beyond STOP RUN; SOME-DATA appears only in DATA DIVISION
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("does not flag paragraph names used as PERFORM targets", () => {
    const text = mkCobol(["PERFORM INIT-PARA."], [], []);
    const index = mkIndex([], ["INIT-PARA"]);
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("uses severity Warning", () => {
    const text = mkCobol(["IF UNKNOWN-VAR DISPLAY 'X'."], []);
    const index = mkIndex();
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    expect(undef.length).toBeGreaterThan(0);
    expect(undef[0].severity).toBe(DiagnosticSeverity.Warning);
  });

  it("flags multiple distinct undefined names", () => {
    const text = mkCobol(["MOVE AAA-X TO BBB-Y."], []);
    const index = mkIndex();
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    expect(undef).toHaveLength(2);
    const names = undef.map((d) => d.message);
    expect(names.some((m) => m.includes("AAA-X"))).toBe(true);
    expect(names.some((m) => m.includes("BBB-Y"))).toBe(true);
  });

  it("skips comment lines in PROCEDURE DIVISION", () => {
    const lines = [
      "       IDENTIFICATION DIVISION.",
      "       PROGRAM-ID. TESTPROG.",
      "       PROCEDURE DIVISION.",
      "      * THIS IS A COMMENT WITH BADVAR-X",
      "           STOP RUN.",
    ];
    const text = lines.join("\n");
    const index = mkIndex();
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("offset points exactly to the undefined word", () => {
    const lines = [
      "       IDENTIFICATION DIVISION.",
      "       PROGRAM-ID. TESTPROG.",
      "       PROCEDURE DIVISION.",
      "           IF BADVAR-X",
    ];
    const text = lines.join("\n");
    const index = mkIndex();
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    expect(undef).toHaveLength(1);
    // Verify the offset points to BADVAR-X inside the text
    const flagged = text.slice(undef[0].startOff, undef[0].endOff);
    expect(flagged).toBe("BADVAR-X");
  });

  it("does not flag identifiers inside multi-line EXEC DLI blocks when defined", () => {
    const lines = [
      "       IDENTIFICATION DIVISION.",
      "       PROGRAM-ID. TESTPROG.",
      "       PROCEDURE DIVISION.",
      "           IF VK-UPDATERR",
      "              PERFORM  LESEN-GV-W",
      "              EXEC DLI ISRT USING PCB(5) VARIABLE KEY (V-KEY)",
      "                    SEGMENT(GV)",
      "                    FROM (V-SEGM)",
      "              END-EXEC",
      "           END-IF.",
    ];
    const text = lines.join("\n");
    // GV = DLI segment name (not checked), V-KEY inside KEY() (not checked),
    // V-SEGM = host var inside FROM() (checked).
    const index = mkIndex(["V-SEGM"], ["LESEN-GV-W"]);
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    // VK-UPDATERR should be flagged; data names inside EXEC DLI args are defined
    expect(undef).toHaveLength(1);
    expect(undef[0].message).toContain("VK-UPDATERR");
    // Verify offset is exact
    const flagged = text.slice(undef[0].startOff, undef[0].endOff);
    expect(flagged).toBe("VK-UPDATERR");
  });

  it("does not flag inside EXEC CICS blocks", () => {
    const text = mkCobol([
      "EXEC CICS LINK PROGRAM('MYPROG') END-EXEC.",
    ]);
    const index = mkIndex();
    const diags = lintUndefinedIdentifiers(text, index);
    expect(diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER")).toHaveLength(0);
  });

  it("resumes scanning after END-EXEC", () => {
    const lines = [
      "       IDENTIFICATION DIVISION.",
      "       PROGRAM-ID. TESTPROG.",
      "       PROCEDURE DIVISION.",
      "           EXEC DLI GU SEGMENT(S1) INTO(A) END-EXEC",
      "           MOVE NOTDEF-X TO WS-A.",
    ];
    const text = lines.join("\n");
    // S1 = DLI segment name (not checked), A = INTO host var
    const index = mkIndex(["WS-A", "A"]);
    const diags = lintUndefinedIdentifiers(text, index);
    const undef = diags.filter((d) => d.code === "UNDEFINED_IDENTIFIER");
    expect(undef).toHaveLength(1);
    expect(undef[0].message).toContain("NOTDEF-X");
  });
});
