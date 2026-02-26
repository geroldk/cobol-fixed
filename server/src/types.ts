/**
 * Shared type definitions for the COBOL 85 Language Server.
 */
import { DiagnosticSeverity, Range } from "vscode-languageserver/node";

// ---- Settings ----

export type Cobol85Settings = {
  warnings: {
    blockClosedByPeriod: boolean;
  };
  copybookPaths: string[];
};

export const DEFAULT_SETTINGS: Cobol85Settings = {
  warnings: {
    blockClosedByPeriod: true,
  },
  copybookPaths: ["copybooks", "COPYBOOKS"],
};

export type ForceValidateParams = { uri?: string };

// ---- Cache ----

export type FileCacheEntry = { mtimeMs: number; text: string };

// ---- Validation profiling ----

export type ValidateProfile = {
  basicMs: number;
  preprocessMs: number;
  parserInitMs: number;
  parserMs: number;
  lintMs: number;
  publishMs: number;
  totalMs: number;
  preTextLen: number;
};

// ---- Normalizer ----

export type NormalizationAdjustment = {
  sourceStart: number;
  sourceEnd: number;
  normalizedStart: number;
  normalizedEnd: number;
};

export type NormalizedParserText = {
  text: string;
  adjustments: NormalizationAdjustment[];
};

// ---- Preprocessor ----

export type TextRange = { start: number; end: number };

export type CopyParseError = { code: string; message: string; range: TextRange };

export type TokKind = "word" | "pseudo" | "string" | "dot";
export type Tok = { text: string; upper: string; start: number; end: number; kind: TokKind; closed?: boolean };

export type ReplacingPair = {
  from: string;
  to: string;
  fromRange: TextRange;
  toRange: TextRange;
  fromKind: TokKind;
  toKind: TokKind;
};

export type CopyStmt = {
  copybookName: string;
  nameRange: TextRange;

  replacing: ReplacingPair[];
  hasReplacingKeyword: boolean;
  replacingKeywordRange: TextRange;

  copyKeywordRange: TextRange;
  terminatedByDot: boolean;
  dotRange?: TextRange;

  errors: CopyParseError[];
};

export type LineSlice = {
  lineNo: number;
  full: string;
  isFixed: boolean;
  indicator: string;   // col 7 when fixed
  langStart: number;   // char offset in full where lang area starts
  lang: string;        // the extracted "parse text" of the line
  isComment: boolean;
};

export type StmtSeg = { lineNo: number; langStart: number; text: string; startOffset: number };
export type CollectedStmt = { text: string; segs: StmtSeg[]; firstLine: number; lastLine: number; inUri: string };

export type Segment =
  | {
    kind: "source";
    genStart: number;
    genEnd: number;
    sourceUri: string;
    sourceLine: number;
    sourceCharStart: number;
    sourceLineTextLen: number;
  }
  | {
    kind: "generated";
    genStart: number;
    genEnd: number;
    anchorUri: string;
    anchorRange: Range;
  };

export type PreprocessedDoc = {
  text: string;
  segments: Segment[];
};

// ---- Lint ----

export type GenDiag = {
  startOff: number;
  endOff: number;
  severity: DiagnosticSeverity;
  code: string;
  message: string;
};

export type ReplaceStat = { fromRange: TextRange; fromDisplay: string; count: number };

export type ProcedureLineContext = {
  indent: number;
  startsKnownOrClause: boolean;
  hasSeparatorPeriod: boolean;
};

export type LintTok =
  | { kind: "word"; upper: string; start: number; end: number }
  | { kind: "dot"; start: number; end: number };

export type OffsetRange = { start: number; end: number };

// ---- Parser bridge ----

export type TSTreePoint = { row: number; column: number };
export type TSTreeEdit = {
  startIndex: number;
  oldEndIndex: number;
  newEndIndex: number;
  startPosition: TSTreePoint;
  oldEndPosition: TSTreePoint;
  newEndPosition: TSTreePoint;
};

export type TsErr = { startByte: number; endByte: number; row: number; col: number };
