import { Parser as TSParser, Language as TSLanguage } from "web-tree-sitter";
import {
  createConnection,
  Diagnostic,
  DiagnosticSeverity,
  DidChangeWatchedFilesNotification,
  DocumentSymbol,
  InitializeParams,
  InitializeResult,
  ProposedFeatures,
  Range,
  SymbolKind,
  TextDocumentSyncKind,
} from "vscode-languageserver/node";
import { TextDocuments } from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import * as fs from "fs";
import * as path from "path";
import { performance } from "perf_hooks";
import { fileURLToPath } from "url";

const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let workspaceFolderPaths: string[] = [];

let tsInitPromise: Promise<void> | null = null;
let tsParser: TSParser | null = null;
type TSTree = NonNullable<ReturnType<TSParser["parse"]>>;
type TSTreePoint = { row: number; column: number };
type TSTreeEdit = {
  startIndex: number;
  oldEndIndex: number;
  newEndIndex: number;
  startPosition: TSTreePoint;
  oldEndPosition: TSTreePoint;
  newEndPosition: TSTreePoint;
};
type ParserState = { tree: TSTree; parseText: string };
type ForceValidateParams = { uri?: string };

function ensureTreeSitterReady(): Promise<void> {
  if (tsInitPromise) return tsInitPromise;

  tsInitPromise = (async () => {
    try {
      // 1) runtime wasm finden (tree-sitter.wasm liegt beim Package)
      const webTsMain = require.resolve("web-tree-sitter");
      const webTsDir = path.dirname(webTsMain);

      await TSParser.init({
        locateFile(scriptName: string) {
          // typischerweise: tree-sitter.wasm
          return path.join(webTsDir, scriptName);
        },
      });

      // 2) COBOL language wasm laden (deins aus server/assets)
      const cobolWasmPath = path.join(__dirname, "..", "assets", "tree-sitter-cobol.wasm");
      const cobolLang = await TSLanguage.load(cobolWasmPath);

      tsParser = new TSParser();
      tsParser.setLanguage(cobolLang);
    } catch (e: any) {
      connection.console.error(`Tree-sitter init failed: ${String(e?.stack ?? e)}`);
      tsParser = null;
      throw e;
    }
  })();

  return tsInitPromise;
}

// ---------------- Cache ----------------
type FileCacheEntry = { mtimeMs: number; text: string };
const fileCache = new Map<string, FileCacheEntry>();
const copybookResolveCache = new Map<string, string | null>();
const parserStateByUri = new Map<string, ParserState>();

const VALIDATE_DEBOUNCE_MS = 180;
const VALIDATE_RERUN_DELAY_MS = 30;
const PARSER_IDLE_MS = Number(process.env.COBOL85_PARSER_IDLE_MS ?? "900");
const VALIDATE_PROFILE_ALL = process.env.COBOL85_PROFILE_VALIDATE === "1";
const VALIDATE_PROFILE_MIN_MS = Number(process.env.COBOL85_PROFILE_MIN_MS ?? "300");

type PendingValidateTimer = { timer: ReturnType<typeof setTimeout>; forceParser: boolean };
const validateTimers = new Map<string, PendingValidateTimer>();
const parserIdleTimers = new Map<string, ReturnType<typeof setTimeout>>();
const validateInFlight = new Set<string>();
const validateRerunPending = new Set<string>();
const validateRerunForceParser = new Set<string>();
const validateEpoch = new Map<string, number>();
const lastContentChangeAt = new Map<string, number>();

type ValidateProfile = {
  basicMs: number;
  preprocessMs: number;
  parserInitMs: number;
  parserMs: number;
  lintMs: number;
  publishMs: number;
  totalMs: number;
  preTextLen: number;
};

// "published uris" clearing (MVP)
let lastPublishedUris = new Set<string>();

connection.onInitialize((params: InitializeParams): InitializeResult => {
  workspaceFolderPaths = (params.workspaceFolders ?? [])
    .map((wf) => fsPathFromUri(wf.uri))
    .filter((p): p is string => !!p);

  if (workspaceFolderPaths.length === 0 && params.rootUri) {
    const p = fsPathFromUri(params.rootUri);
    if (p) workspaceFolderPaths = [p];
  }

  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      documentSymbolProvider: true,
    },
  };
});

connection.onInitialized(async () => {
  try {
    const wfs = await connection.workspace.getWorkspaceFolders();
    if (wfs && wfs.length) {
      workspaceFolderPaths = wfs
        .map((wf) => fsPathFromUri(wf.uri))
        .filter((p): p is string => !!p);
    }
  } catch {
    // ignore
  }

  await connection.client.register(DidChangeWatchedFilesNotification.type, {
    watchers: [
      { globPattern: "**/*.{cpy,CPY,cob,COB,cbl,CBL}" },
      { globPattern: "**/copybooks/**/*.{cpy,CPY,cob,COB,cbl,CBL}" },
      { globPattern: "**/COPYBOOKS/**/*.{cpy,CPY,cob,COB,cbl,CBL}" },
    ],
  });
});

connection.onDidChangeWatchedFiles(() => {
  fileCache.clear();
  copybookResolveCache.clear();
  disposeAllParserStates();
  for (const d of documents.all()) {
    scheduleValidate(d.uri, 0, true);
  }
});

documents.onDidChangeContent((change) => {
  const uri = change.document.uri;
  lastContentChangeAt.set(uri, Date.now());
  scheduleValidate(uri, VALIDATE_DEBOUNCE_MS, false);
  scheduleParserIdleValidate(uri);
});

documents.onDidOpen((e) => {
  scheduleValidate(e.document.uri, 0, true);
});

connection.onNotification("cobol85/forceValidate", (params?: ForceValidateParams) => {
  const uri = params?.uri;
  if (!uri) return;
  if (!documents.get(uri)) return;
  scheduleValidate(uri, 0, true);
});

documents.onDidClose((e) => {
  const uri = e.document.uri;
  const t = validateTimers.get(uri);
  if (t) clearTimeout(t.timer);
  validateTimers.delete(uri);
  const pt = parserIdleTimers.get(uri);
  if (pt) clearTimeout(pt);
  parserIdleTimers.delete(uri);
  validateInFlight.delete(uri);
  validateRerunPending.delete(uri);
  validateRerunForceParser.delete(uri);
  validateEpoch.delete(uri);
  lastContentChangeAt.delete(uri);
  disposeParserState(uri);
  connection.sendDiagnostics({ uri, diagnostics: [] });
});

function disposeParserState(uri: string): void {
  const prev = parserStateByUri.get(uri);
  if (!prev) return;
  parserStateByUri.delete(uri);
  try { prev.tree.delete(); } catch { }
}

function disposeAllParserStates(): void {
  for (const uri of parserStateByUri.keys()) disposeParserState(uri);
}

connection.onDocumentSymbol((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return [];
  return buildDocumentSymbols(doc);
});

function scheduleValidate(uri: string, delayMs: number, forceParser: boolean) {
  bumpValidateEpoch(uri);

  const prev = validateTimers.get(uri);
  if (prev) clearTimeout(prev.timer);

  const nextForceParser = forceParser || !!prev?.forceParser;

  const timer = setTimeout(() => {
    validateTimers.delete(uri);
    void runValidate(uri, nextForceParser);
  }, Math.max(0, delayMs));

  validateTimers.set(uri, { timer, forceParser: nextForceParser });
}

function scheduleParserIdleValidate(uri: string) {
  const prev = parserIdleTimers.get(uri);
  if (prev) clearTimeout(prev);

  const timer = setTimeout(() => {
    parserIdleTimers.delete(uri);
    scheduleValidate(uri, 0, true);
  }, PARSER_IDLE_MS);

  parserIdleTimers.set(uri, timer);
}

async function runValidate(uri: string, forceParser: boolean): Promise<void> {
  const doc = documents.get(uri);
  if (!doc) return;

  if (validateInFlight.has(uri)) {
    validateRerunPending.add(uri);
    if (forceParser) validateRerunForceParser.add(uri);
    return;
  }

  validateInFlight.add(uri);
  const epoch = currentValidateEpoch(uri);

  try {
    await validateDocument(doc, epoch, forceParser);
  } finally {
    validateInFlight.delete(uri);

    if (validateRerunPending.delete(uri)) {
      const rerunForceParser = validateRerunForceParser.delete(uri);
      scheduleValidate(uri, VALIDATE_RERUN_DELAY_MS, rerunForceParser);
    }
  }
}

function bumpValidateEpoch(uri: string): number {
  const next = (validateEpoch.get(uri) ?? 0) + 1;
  validateEpoch.set(uri, next);
  return next;
}

function currentValidateEpoch(uri: string): number {
  return validateEpoch.get(uri) ?? 0;
}

function isValidateStale(uri: string, epoch: number): boolean {
  return currentValidateEpoch(uri) !== epoch;
}

function countDiags(diagsByUri: Map<string, Diagnostic[]>): number {
  let n = 0;
  for (const arr of diagsByUri.values()) n += arr.length;
  return n;
}

function fmtMs(v: number): string {
  return v.toFixed(1);
}

function maybeLogValidateProfile(uri: string, p: ValidateProfile, diagCount: number, note?: string) {
  const slow = p.totalMs >= VALIDATE_PROFILE_MIN_MS;
  if (!VALIDATE_PROFILE_ALL && !slow) return;

  const fsPath = fsPathFromUri(uri);
  const file = fsPath ? path.basename(fsPath) : uri;
  const suffix = note ? `, note=${note}` : "";
  const msg =
    `[validate] ${file}: total=${fmtMs(p.totalMs)}ms ` +
    `(basic=${fmtMs(p.basicMs)}, preprocess=${fmtMs(p.preprocessMs)}, parserInit=${fmtMs(p.parserInitMs)}, ` +
    `parser=${fmtMs(p.parserMs)}, lint=${fmtMs(p.lintMs)}, publish=${fmtMs(p.publishMs)}), ` +
    `diags=${diagCount}, preLen=${p.preTextLen}${suffix}`;

  if (slow) connection.console.warn(msg);
  else connection.console.log(msg);
}

async function validateDocument(doc: TextDocument, epoch: number, forceParser: boolean): Promise<void> {
  if (isValidateStale(doc.uri, epoch)) return;

  const profile: ValidateProfile = {
    basicMs: 0,
    preprocessMs: 0,
    parserInitMs: 0,
    parserMs: 0,
    lintMs: 0,
    publishMs: 0,
    totalMs: 0,
    preTextLen: 0,
  };
  const totalStart = performance.now();

  const diagsByUri = new Map<string, Diagnostic[]>();

  const docFsPath = fsPathFromUri(doc.uri);
  const docDir = docFsPath ? path.dirname(docFsPath) : undefined;

  // base search dirs
  const baseDirs: string[] = [];
  if (docDir) baseDirs.push(docDir);
  for (const wf of workspaceFolderPaths) {
    baseDirs.push(wf);
    baseDirs.push(path.join(wf, "copybooks"));
    baseDirs.push(path.join(wf, "COPYBOOKS"));
  }
  const uniqueBaseDirs = Array.from(new Set(baseDirs));

  // ----- Basic checks on root doc (tabs, overflow, IDENTIFICATION/ID DIVISION)
  const basicStart = performance.now();
  basicFixedFormatChecks(doc.uri, doc.getText(), diagsByUri);

  // IDENTIFICATION DIVISION present? (ID DIVISION wird als Alias akzeptiert)
  const text0 = doc.getText();
  const requireIdentificationDivision = shouldRequireIdentificationDivision(doc.uri, text0);
  if (requireIdentificationDivision && !hasIdentificationDivision(text0)) {
    const anchor = findFirstCodeAnchor(text0);
    pushDiag(diagsByUri, doc.uri, {
      severity: DiagnosticSeverity.Error,
      range: anchor,
      message: "Fehlt: IDENTIFICATION DIVISION.",
      source: "cobol85",
      code: "MISSING_IDENTIFICATION_DIVISION",
    });
  }
  profile.basicMs = performance.now() - basicStart;

  // ----- Preprocess (COPY expansion + mapping infra)
  const preprocessStart = performance.now();
  const pre = preprocessUri({
    uri: doc.uri,
    text: doc.getText(),
    baseDirs: uniqueBaseDirs,
    diagsByUri,
    includeStack: [],
    depth: 0,
    maxDepth: 20,
  });
  profile.preprocessMs = performance.now() - preprocessStart;
  profile.preTextLen = pre.text.length;

  if (isValidateStale(doc.uri, epoch)) return;

  const lastChanged = lastContentChangeAt.get(doc.uri);
  const msSinceChange =
    lastChanged === undefined ? Number.POSITIVE_INFINITY : Date.now() - lastChanged;
  const runParser = forceParser || msSinceChange >= PARSER_IDLE_MS;
  let parserNote: string | undefined;

  if (runParser) {
    const parserInitStart = performance.now();
    await ensureTreeSitterReady();
    profile.parserInitMs = performance.now() - parserInitStart;
    if (isValidateStale(doc.uri, epoch)) return;

    const parserStart = performance.now();
    if (tsParser) {
    const normalized = normalizeForCobol85Parser(pre.text);
    const parseText = normalized.text;
    const prevState = parserStateByUri.get(doc.uri);
    let oldTree: TSTree | undefined = prevState?.tree;

      if (oldTree && prevState && prevState.parseText !== parseText) {
        const edit = computeTreeEdit(prevState.parseText, parseText);
        if (edit) oldTree.edit(edit);
        else oldTree = undefined;
      }
      parserNote = oldTree ? "parser-incremental" : "parser-full";

    const tree = tsParser.parse(parseText, oldTree);
    const root = tree?.rootNode;

    if (root?.hasError) {
      const rawErrs = collectTreeSitterErrors(root, 25);
      const errs = filterTreeSitterErrors(rawErrs, parseText.length);
      const execDliRanges = collectExecDliRanges(pre.text);

      // Wenn selbst nach Begrenzung noch viele: 1 Summary-Diag
      if (errs.length >= 25) {
        const mapped = mapGenRange(pre, 0, 1);
        if (mapped) {
          pushDiag(diagsByUri, mapped.uri, {
            severity: DiagnosticSeverity.Warning,
            range: mapped.range,
            message: "Viele Syntaxfehler (Tree-sitter). Anzeige wurde begrenzt â€“ erster Fehler ist weiter unten markiert.",
            source: "cobol85",
            code: "TS_ERROR_THROTTLED"
          });
        }
      }

      for (const e of errs) {
        if (isValidateStale(doc.uri, epoch)) {
          if (tree && (!prevState || prevState.tree !== tree)) {
            try { tree.delete(); } catch { }
          }
          return;
        }

        const startNormUtf16 = utf8ByteOffsetToUtf16Index(parseText, e.startByte);
        const endNormUtf16 = utf8ByteOffsetToUtf16Index(parseText, e.endByte);
        const startUtf16 = mapNormalizedOffsetToSource(
          startNormUtf16,
          normalized.adjustments,
          pre.text.length
        );
        const endUtf16 = mapNormalizedOffsetToSource(
          endNormUtf16,
          normalized.adjustments,
          pre.text.length
        );
        if (overlapsAnyRange(startUtf16, endUtf16, execDliRanges)) continue;

        const len = Math.min(20, Math.max(1, endUtf16 - startUtf16)); // max 20 chars highlight
        const mapped = mapGenRange(pre, startUtf16, startUtf16 + len);
        if (!mapped) continue;

        const ctx = lineWithCaret(mapped.uri, mapped.range.start.line, mapped.range.start.character);
        const msg = ctx ? `Syntaxfehler (Parser)\n${ctx}` : "Syntaxfehler (Parser)";

        pushDiag(diagsByUri, mapped.uri, {
          severity: DiagnosticSeverity.Error,
          range: clampRangeToSingleLine(mapped.uri, mapped.range),
          message: msg,
          source: "cobol85",
          code: "TS_PARSE_ERROR"
        });
      }
    }

    if (isValidateStale(doc.uri, epoch)) {
      if (tree && (!prevState || prevState.tree !== tree)) {
        try { tree.delete(); } catch { }
      }
      return;
    }

    if (tree) {
      if (prevState?.tree && prevState.tree !== tree) {
        try { prevState.tree.delete(); } catch { }
      }
      parserStateByUri.set(doc.uri, { tree, parseText });
    } else if (prevState) {
      parserStateByUri.delete(doc.uri);
      try { prevState.tree.delete(); } catch { }
    }
    }
    profile.parserMs = performance.now() - parserStart;
  }

  const lintStart = performance.now();
  const genDiags = lintPreprocessed(pre.text);
  for (const gd of genDiags) {
    if (isValidateStale(doc.uri, epoch)) return;

    const mapped = mapGenRange(pre, gd.startOff, gd.endOff);
    if (!mapped) continue;

    pushDiag(diagsByUri, mapped.uri, {
      severity: gd.severity,
      range: mapped.range,
      message: gd.message,
      source: "cobol85",
      code: gd.code,
    });
  }
  profile.lintMs = performance.now() - lintStart;

  // ----- Publish diagnostics per URI (MVP clearing)
  const newUris = new Set(diagsByUri.keys());
  if (isValidateStale(doc.uri, epoch)) return;

  const publishStart = performance.now();
  for (const uri of lastPublishedUris) {
    if (!newUris.has(uri)) connection.sendDiagnostics({ uri, diagnostics: [] });
  }

  publishDiagnostics(diagsByUri);
  profile.publishMs += performance.now() - publishStart;

  lastPublishedUris = newUris;
  profile.totalMs = performance.now() - totalStart;
  maybeLogValidateProfile(
    doc.uri,
    profile,
    countDiags(diagsByUri),
    runParser ? parserNote : "parser-skipped-typing"
  );
}

function publishDiagnostics(diagsByUri: Map<string, Diagnostic[]>) {
  for (const [uri, diagnostics] of diagsByUri.entries()) {
    connection.sendDiagnostics({ uri, diagnostics });
  }
}

type NormalizationAdjustment = {
  sourceStart: number;
  sourceEnd: number;
  normalizedStart: number;
  normalizedEnd: number;
};

type NormalizedParserText = {
  text: string;
  adjustments: NormalizationAdjustment[];
};

const FIXED_COMMENT_INDICATORS = new Set(["*", "/", "D", "d"]);
const FIXED_CODE_INDICATORS = new Set([" ", "-"]);

function hasFixedColumns(line: string): boolean {
  return line.length >= 7;
}

function isFixedCommentIndicator(indicator: string): boolean {
  return FIXED_COMMENT_INDICATORS.has(indicator);
}

function isValidFixedIndicator(indicator: string): boolean {
  return FIXED_CODE_INDICATORS.has(indicator) || isFixedCommentIndicator(indicator);
}

function normalizeForCobol85Parser(text72: string): NormalizedParserText {
  const sourceLines = text72.split("\n");
  const normalizedLines = [...sourceLines];
  const adjustments: NormalizationAdjustment[] = [];

  // Dokumentationsparagraphen in IDENTIFICATION DIVISION
  const docParas = /^(AUTHOR|DATE-COMPILED|DATE-WRITTEN|INSTALLATION|REMARKS|SECURITY)\./i;

  let sourceOff = 0;
  let normalizedOff = 0;

  for (let i = 0; i < sourceLines.length; i++) {
    const sourceLine = sourceLines[i];
    let line = sourceLine;
    const isFixed = hasFixedColumns(line);
    const langTrim = (isFixed ? line.slice(7) : line).trimStart();

    if (isFixed && line.length >= 8 && docParas.test(langTrim)) {
      // Spalte 7 (Index 6) auf '*' setzen => Kommentarzeile
      line = line.slice(0, 6) + "*" + line.slice(7);
    }

    if (isListingControlStatement(langTrim) || isCompilerDirectiveStatement(langTrim)) {
      line = neutralizeListingControlLine(line, isFixed);
    }

    // Parser-Toleranz: "A = ('X' OR 'Y')" -> Klammern entfernen (Laenge bleibt identisch)
    // Einige Dialekte erlauben diese Form, tree-sitter-cobol stolpert darueber.
    line = normalizeParenthesizedOrCondition(line);

    // Dialekt-Alias fuer Parser-Normalisierung: ID DIVISION -> IDENTIFICATION DIVISION
    line = normalizeIdDivisionAliasForParser(line, isFixed, sourceOff, normalizedOff, adjustments);
    normalizedLines[i] = line;

    sourceOff += sourceLine.length;
    normalizedOff += line.length;
    if (i < sourceLines.length - 1) {
      sourceOff += 1;
      normalizedOff += 1;
    }
  }

  return { text: normalizedLines.join("\n"), adjustments };
}

function normalizeParenthesizedOrCondition(line: string): string {
  return line.replace(/=\s*\([^()\n]*\bOR\b[^()\n]*\)/gi, (full) => {
    const open = full.indexOf("(");
    const close = full.lastIndexOf(")");
    if (open < 0 || close <= open) return full;
    return full.slice(0, open) + " " + full.slice(open + 1, close) + " ";
  });
}

function isListingControlStatement(langTrim: string): boolean {
  if (/^(EJECT|SKIP(?:[123])?)\s*\.?\s*$/i.test(langTrim)) return true;
  if (/^TITLE(?:\s+['"][^'"\r\n]*['"])?\s*\.?\s*$/i.test(langTrim)) return true;
  return false;
}

function isCompilerDirectiveStatement(langTrim: string): boolean {
  // Compiler options/directives are not executable COBOL syntax for the parser.
  return /^(CBL|PROCESS)\b/i.test(langTrim);
}

function neutralizeListingControlLine(line: string, isFixed: boolean): string {
  if (isFixed && line.length >= 7) {
    const tail = line.length > 7 ? line.slice(7) : "";
    return line.slice(0, 6) + "*" + tail;
  }

  if (line.length >= 2) return "*>" + " ".repeat(line.length - 2);
  if (line.length === 1) return "*";
  return "";
}

function normalizeIdDivisionAliasForParser(
  line: string,
  isFixed: boolean,
  sourceLineStart: number,
  normalizedLineStart: number,
  adjustments: NormalizationAdjustment[]
): string {
  if (isFixed) {
    if (line.length < 7) return line;
    const indicator = line[6];
    if (isFixedCommentIndicator(indicator)) return line;

    const prefix = line.slice(0, 7);
    const lang = line.slice(7);
    const normalizedLang = rewriteIdDivisionAliasSegment(
      lang,
      sourceLineStart + 7,
      normalizedLineStart + 7,
      adjustments
    );
    return prefix + normalizedLang;
  }

  if (/^\s*\*>/.test(line)) return line;
  return rewriteIdDivisionAliasSegment(line, sourceLineStart, normalizedLineStart, adjustments);
}

function rewriteIdDivisionAliasSegment(
  segment: string,
  sourceBase: number,
  normalizedBase: number,
  adjustments: NormalizationAdjustment[]
): string {
  const re = /\bID\s+DIVISION\b/gi;
  let out = "";
  let sourceCursor = 0;
  let normalizedCursor = 0;

  while (true) {
    const m = re.exec(segment);
    if (!m) break;

    const idx = m.index ?? 0;
    const matched = m[0];
    const replacement = "IDENTIFICATION DIVISION";

    out += segment.slice(sourceCursor, idx);
    normalizedCursor += idx - sourceCursor;

    const sourceStart = sourceBase + idx;
    const sourceEnd = sourceStart + matched.length;
    const normalizedStart = normalizedBase + normalizedCursor;
    const normalizedEnd = normalizedStart + replacement.length;

    adjustments.push({ sourceStart, sourceEnd, normalizedStart, normalizedEnd });

    out += replacement;
    normalizedCursor += replacement.length;
    sourceCursor = idx + matched.length;
  }

  if (sourceCursor === 0) return segment;
  out += segment.slice(sourceCursor);
  return out;
}

function mapNormalizedOffsetToSource(
  normalizedOff: number,
  adjustments: NormalizationAdjustment[],
  sourceLength: number
): number {
  if (adjustments.length === 0) return Math.max(0, Math.min(normalizedOff, sourceLength));

  let delta = 0;
  for (const a of adjustments) {
    if (normalizedOff < a.normalizedStart) break;

    if (normalizedOff <= a.normalizedEnd) {
      const rel = normalizedOff - a.normalizedStart;
      const sourceLen = a.sourceEnd - a.sourceStart;
      return a.sourceStart + Math.min(rel, sourceLen);
    }

    delta += (a.normalizedEnd - a.normalizedStart) - (a.sourceEnd - a.sourceStart);
  }

  return Math.max(0, Math.min(normalizedOff - delta, sourceLength));
}

function lineWithCaret(uri: string, line: number, ch: number): string | undefined {
  const open = documents.get(uri);
  let text: string | undefined;

  if (open) {
    const lines = open.getText().split(/\r?\n/);
    text = lines[line];
  } else {
    const fsPath = fsPathFromUri(uri);
    if (!fsPath) return undefined;
    const fileText = loadTextCached(fsPath);
    if (!fileText) return undefined;
    text = fileText.split(/\r?\n/)[line];
  }

  if (text === undefined) return undefined;
  const caret = " ".repeat(Math.min(ch, 200)) + "^";
  return `${text}\n${caret}`;
}

/* ======================= Preprocessor + Mapping ======================= */

type TextRange = { start: number; end: number };

type CopyParseError = { code: string; message: string; range: TextRange };

type TokKind = "word" | "pseudo" | "string" | "dot";
type Tok = { text: string; upper: string; start: number; end: number; kind: TokKind; closed?: boolean };

type ReplacingPair = {
  from: string;
  to: string;
  fromRange: TextRange;
  toRange: TextRange;
  fromKind: TokKind;
  toKind: TokKind;
};

type CopyStmt = {
  copybookName: string;
  nameRange: TextRange;

  replacing: ReplacingPair[];
  hasReplacingKeyword: boolean;
  replacingKeywordRange: TextRange;

  // neu:
  copyKeywordRange: TextRange;     // Range des "COPY"
  terminatedByDot: boolean;        // ob ein '.' Token gefunden wurde
  dotRange?: TextRange;            // Range des '.' (falls gefunden)

  errors: CopyParseError[];
};

type LineSlice = {
  lineNo: number;
  full: string;
  isFixed: boolean;
  indicator: string;   // col 7 when fixed
  langStart: number;   // char offset in full where lang area starts
  lang: string;        // the extracted â€œparse textâ€ of the line
  isComment: boolean;
};

type StmtSeg = { lineNo: number; langStart: number; text: string; startOffset: number };
type CollectedStmt = { text: string; segs: StmtSeg[]; firstLine: number; lastLine: number; inUri: string };

type Segment =
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

type PreprocessedDoc = {
  text: string;
  segments: Segment[];
};

class PreBuilder {
  private parts: string[] = [];
  private segs: Segment[] = [];
  private off = 0;

  appendSourceLine(sourceUri: string, sourceLine: number, sourceCharStart: number, lineText: string, sourceLineTextLen: number) {
    // content segment (no newline)
    const start = this.off;
    const end = start + lineText.length;
    this.parts.push(lineText);
    this.segs.push({
      kind: "source",
      genStart: start,
      genEnd: end,
      sourceUri,
      sourceLine,
      sourceCharStart,
      sourceLineTextLen,
    });
    this.off = end;

    // newline as generated
    this.appendGeneratedText(sourceUri, Range.create(sourceLine, sourceCharStart, sourceLine, sourceCharStart), "\n");
  }

  appendGeneratedText(anchorUri: string, anchorRange: Range, text: string) {
    if (!text) return;
    const start = this.off;
    const end = start + text.length;
    this.parts.push(text);
    this.segs.push({ kind: "generated", genStart: start, genEnd: end, anchorUri, anchorRange });
    this.off = end;
  }

  appendPreprocessed(pre: PreprocessedDoc) {
    if (!pre.text) return;

    // optional: ensure trailing newline, damit "line world" stabil bleibt
    const text = pre.text.endsWith("\n") ? pre.text : pre.text + "\n";

    const startOff = this.off;
    this.parts.push(text);

    // segments Ã¼bernehmen, offsets shiften
    for (const s of pre.segments) {
      this.segs.push({
        ...s,
        genStart: s.genStart + startOff,
        genEnd: s.genEnd + startOff,
      } as Segment);
    }

    this.off += text.length;
  }

  build(): PreprocessedDoc {
    return { text: this.parts.join(""), segments: this.segs };
  }
}

function normalizeFixedLineForParser(line72: string, indicator: string, isComment: boolean): string {
  if (line72.length < 7) return line72;

  // Spalten 1-6 sind im Fixed-Format Sequenzbereich und duerfen den Parser nicht beeinflussen.
  const prefix = " ".repeat(6);
  if (isComment) {
    return prefix + " " + " ".repeat(Math.max(0, line72.length - 7));
  }

  // Continuation-Indikator '-' (Spalte 7) als Leerzeichen geben, damit der Parser
  // den fortgesetzten Text nicht als Minus-Operator fehlinterpretiert.
  if (indicator === "-") {
    return prefix + " " + line72.slice(7);
  }

  return line72;
}

function preprocessUri(args: {
  uri: string;
  text: string;
  baseDirs: string[];
  diagsByUri: Map<string, Diagnostic[]>;
  includeStack: string[]; // fsPaths of currently included files
  depth: number;
  maxDepth: number;
}): PreprocessedDoc {
  const { uri, text, baseDirs, diagsByUri, includeStack, depth, maxDepth } = args;

  const builder = new PreBuilder();
  const slices = sliceLines(text);

  let i = 0;
  while (i < slices.length) {
    const sl = slices[i];

    // Kommentarzeilen nicht parsen; nur Zeilenstruktur fÃ¼r Mapping erhalten.
    if (sl.isComment) {
      if (sl.isFixed) {
        const line72 = sl.full.slice(0, 72);
        const muted = normalizeFixedLineForParser(line72, sl.indicator, true);
        builder.appendSourceLine(uri, sl.lineNo, 0, muted, line72.length);
      } else {
        builder.appendSourceLine(uri, sl.lineNo, 0, "", sl.full.length);
      }
      i++;
      continue;
    }

    // Strict fixed-mode: indicator in column 7 must be valid.
    if (sl.isFixed && !isValidFixedIndicator(sl.indicator)) {
      builder.appendSourceLine(uri, sl.lineNo, 0, "", sl.full.length);
      i++;
      continue;
    }

    // start COPY only on non-continuation lines (fixed-format rule)
    const isContinuation = sl.isFixed && sl.indicator === "-";

    if (!isContinuation && /\bCOPY\b/i.test(sl.lang)) {
      const stmt = collectCopyStatement(uri, slices, i);

      if (stmt) {
        const copyStmts = parseCopyStatements(stmt.text);

        // Wenn nichts parsebar ist: Originalzeilen NICHT verlieren -> einfach durchreichen
        if (copyStmts.length === 0) {
          for (const seg of stmt.segs) {
            const sl2 = slices[seg.lineNo];
            builder.appendSourceLine(uri, seg.lineNo, seg.langStart, seg.text, sl2.full.length);
          }
          i = stmt.lastLine + 1;
          continue;
        }

        for (const c of copyStmts) {
          // Punkt fehlt? -> Warnung am COPY Keyword (oder an '.' falls du willst)
          // (Wenn du den Fehler schon in parseCopyStatements erzeugst, kannst du das hier weglassen.)
          if (!c.terminatedByDot) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Warning,
              range: mapStmtTextRangeToDocRange(stmt, c.copyKeywordRange),
              message: "COPY-Statement ohne '.' (Punkt). Expansion kann unzuverlÃ¤ssig sein.",
              source: "cobol85",
              code: "COPY_MISSING_PERIOD",
            });

            // Fallback: Originalzeilen durchreichen (damit nichts verschwindet)
            for (const seg of stmt.segs) {
              const sl2 = slices[seg.lineNo];
              builder.appendSourceLine(uri, seg.lineNo, seg.langStart, seg.text, sl2.full.length);
            }
            continue;
          }

          // 1) Resolve copybook (WICHTIG: baseDirs um aktuellen Datei-Ordner ergÃ¤nzen!)
          const uriFs = fsPathFromUri(uri);
          const thisDir = uriFs ? path.dirname(uriFs) : undefined;
          const resolveDirs = thisDir ? [thisDir, ...baseDirs] : baseDirs;
          const uniqueResolveDirs = Array.from(new Set(resolveDirs));

          const resolved = resolveCopybook(c.copybookName, uniqueResolveDirs);
          if (!resolved) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Error,
              range: mapStmtTextRangeToDocRange(stmt, c.nameRange),
              message: `COPY book nicht gefunden: ${c.copybookName}. Gesucht in: ${summarizeDirs(uniqueResolveDirs, 3)}`,
              source: "cobol85",
              code: "COPYBOOK_NOT_FOUND",
            });

            // Copybook fehlt -> Originalzeilen durchreichen (kein â€œText verschluckenâ€)
            for (const seg of stmt.segs) {
              const sl2 = slices[seg.lineNo];
              builder.appendSourceLine(uri, seg.lineNo, seg.langStart, seg.text, sl2.full.length);
            }
            continue;
          }

          // 2) Cycle + depth detection wie bisher
          if (includeStack.includes(resolved)) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Error,
              range: mapStmtTextRangeToDocRange(stmt, c.nameRange),
              message: `Zyklisches COPY erkannt: ${path.basename(resolved)} ist bereits im Include-Stack.`,
              source: "cobol85",
              code: "COPYBOOK_CYCLE",
            });
            // Originalzeilen durchreichen
            for (const seg of stmt.segs) {
              const sl2 = slices[seg.lineNo];
              builder.appendSourceLine(uri, seg.lineNo, seg.langStart, seg.text, sl2.full.length);
            }
            continue;
          }

          if (depth >= maxDepth) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Error,
              range: mapStmtTextRangeToDocRange(stmt, c.nameRange),
              message: `COPY max depth (${maxDepth}) erreicht. Abbruch bei: ${path.basename(resolved)}`,
              source: "cobol85",
              code: "COPYBOOK_MAX_DEPTH",
            });
            for (const seg of stmt.segs) {
              const sl2 = slices[seg.lineNo];
              builder.appendSourceLine(uri, seg.lineNo, seg.langStart, seg.text, sl2.full.length);
            }
            continue;
          }

          // 3) REPLACING Syntax Errors
          for (const err of c.errors) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Error,
              range: mapStmtTextRangeToDocRange(stmt, err.range),
              message: err.message,
              source: "cobol85",
              code: err.code,
            });
          }

          if (c.hasReplacingKeyword && c.replacing.length === 0) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Warning,
              range: mapStmtTextRangeToDocRange(stmt, c.replacingKeywordRange),
              message: "REPLACING gefunden, aber keine gÃ¼ltigen 'FROM BY TO' Paare erkannt.",
              source: "cobol85",
              code: "COPY_REPLACING_EMPTY",
            });
          }

          const copyUri = uriFromFsPath(resolved);
          const copyText = loadTextCached(resolved);
          if (copyText === undefined) {
            pushDiag(diagsByUri, uri, {
              severity: DiagnosticSeverity.Error,
              range: mapStmtTextRangeToDocRange(stmt, c.nameRange),
              message: `COPY book konnte nicht gelesen werden: ${resolved}`,
              source: "cobol85",
              code: "COPYBOOK_READ_ERROR",
            });
            for (const seg of stmt.segs) {
              const sl2 = slices[seg.lineNo];
              builder.appendSourceLine(uri, seg.lineNo, seg.langStart, seg.text, sl2.full.length);
            }
            continue;
          }

          basicFixedFormatChecks(copyUri, copyText, diagsByUri);

          const hasReplacing = c.replacing.length > 0;

          // Wenn REPLACING: apply auf Fixed-Text, damit nested COPY & continuation erhalten bleiben
          let copyTextToProcess = copyText;
          if (hasReplacing) {
            const applied = applyReplacingToFixedText(copyText, c.replacing);
            for (const stat of applied.stats) {
              if (stat.count === 0) {
                pushDiag(diagsByUri, uri, {
                  severity: DiagnosticSeverity.Warning,
                  range: mapStmtTextRangeToDocRange(stmt, stat.fromRange),
                  message: `REPLACING: FROM-Pattern kommt im Copybook nicht vor: ${stat.fromDisplay}`,
                  source: "cobol85",
                  code: "COPY_REPLACING_NO_MATCH",
                });
              }
            }
            copyTextToProcess = applied.text;
          }

          const rec = preprocessUri({
            uri: copyUri,
            text: copyTextToProcess,
            baseDirs: uniqueResolveDirs,
            diagsByUri,
            includeStack: [...includeStack, resolved],
            depth: depth + 1,
            maxDepth,
          });

          if (hasReplacing) {
            // bei transformiertem Text: anchor am Callsite
            builder.appendGeneratedText(uri, mapStmtTextRangeToDocRange(stmt, c.nameRange), rec.text);
          } else {
            // echter Merge mit Segmenten
            builder.appendPreprocessed(rec);
          }
        }

        i = stmt.lastLine + 1;
        continue;
      }
    }

    // Normal line: fixed-format fÃ¼r Parser sanft normalisieren
    if (sl.isFixed) {
      const line72 = sl.full.slice(0, 72);
      const normalized = normalizeFixedLineForParser(line72, sl.indicator, false);
      builder.appendSourceLine(uri, sl.lineNo, 0, normalized, line72.length);
    } else {
      // Strict fixed-mode: invalid short lines are neutralized for parser stability.
      builder.appendSourceLine(uri, sl.lineNo, 0, "", sl.full.length);
    }
    i++;
  }

  return builder.build();
}

function mapOffset(pre: PreprocessedDoc, off: number): { uri: string; line: number; character: number } {
  // Find segment containing off (linear MVP)
  let seg = pre.segments.find((s) => off >= s.genStart && off < s.genEnd);
  if (!seg && off === pre.text.length && pre.segments.length) {
    seg = pre.segments[pre.segments.length - 1];
  }
  if (!seg) return { uri: "file:///", line: 0, character: 0 };

  if (seg.kind === "generated") {
    return {
      uri: seg.anchorUri,
      line: seg.anchorRange.start.line,
      character: seg.anchorRange.start.character,
    };
  }

  const rel = Math.max(0, Math.min(off - seg.genStart, seg.genEnd - seg.genStart));
  const char = Math.min(seg.sourceCharStart + rel, seg.sourceLineTextLen);
  return { uri: seg.sourceUri, line: seg.sourceLine, character: char };
}

function sliceLines(text: string): LineSlice[] {
  const lines = text.split(/\r?\n/);
  const out: LineSlice[] = [];

  for (let i = 0; i < lines.length; i++) {
    const full = lines[i];
    if (hasFixedColumns(full)) {
      const indicator = full[6];
      const isComment = isFixedCommentIndicator(indicator);
      const langStart = 7;
      const langEnd = Math.min(full.length, 72);
      const lang = full.slice(langStart, langEnd);
      out.push({ lineNo: i, full, isFixed: true, indicator, langStart, lang, isComment });
      continue;
    }

    // Strict fixed-mode: short lines are invalid fixed lines and carry no language area.
    out.push({
      lineNo: i,
      full,
      isFixed: false,
      indicator: " ",
      langStart: 0,
      lang: "",
      isComment: false,
    });
  }

  return out;
}

function findFirstCodeAnchor(text: string): Range {
  const slices = sliceLines(text);

  for (const sl of slices) {
    if (sl.isComment) continue;
    const idx = sl.lang.search(/\S/);
    if (idx >= 0) {
      const ch = sl.langStart + idx;
      return Range.create(sl.lineNo, ch, sl.lineNo, ch + 1);
    }
  }
  return Range.create(0, 0, 0, 1);
}

function flattenLang(slices: LineSlice[]): string {
  // keep line structure for later mapping
  return slices.map((s) => (s.isComment ? "" : s.lang)).join("\n");
}

function collectCopyStatement(uri: string, slices: LineSlice[], startLine: number): CollectedStmt | undefined {
  const segs: StmtSeg[] = [];
  let text = "";
  let offset = 0;

  const maxLines = 50;
  let lastLine = startLine;

  for (let k = 0; k < maxLines; k++) {
    const idx = startLine + k;
    if (idx >= slices.length) break;

    const sl = slices[idx];

    // Continuation only if fixed and indicator is '-'
    if (k > 0) {
      if (!(sl.isFixed && sl.indicator === "-")) break;
    }

    segs.push({ lineNo: sl.lineNo, langStart: sl.langStart, text: sl.lang, startOffset: offset });
    text += sl.lang;
    offset += sl.lang.length;

    text += "\n";
    offset += 1;

    lastLine = sl.lineNo;

  }

  if (segs.length === 0) return undefined;
  return { text, segs, firstLine: startLine, lastLine, inUri: uri };
}

function mapStmtTextRangeToDocRange(stmt: CollectedStmt, r: TextRange): Range {
  const s = mapStmtOffset(stmt, r.start);
  const e = mapStmtOffset(stmt, r.end);
  return Range.create(s.line, s.character, e.line, e.character);
}

function mapStmtOffset(stmt: CollectedStmt, off: number): { line: number; character: number } {
  let segIndex = stmt.segs.length - 1;

  for (let i = 0; i < stmt.segs.length; i++) {
    const seg = stmt.segs[i];
    const segEnd = seg.startOffset + seg.text.length;
    if (off <= segEnd) {
      segIndex = i;
      break;
    }
  }

  const seg = stmt.segs[segIndex];
  const segEnd = seg.startOffset + seg.text.length;
  const clamped = Math.max(seg.startOffset, Math.min(off, segEnd));
  const colInLang = clamped - seg.startOffset;

  return { line: seg.lineNo, character: seg.langStart + colInLang };
}

type GenDiag = {
  startOff: number;
  endOff: number;
  severity: DiagnosticSeverity;
  code: string;
  message: string;
};

function mapGenRange(pre: PreprocessedDoc, startOff: number, endOff: number): { uri: string; range: Range } | undefined {
  const a = mapGenPoint(pre, startOff);
  const b = mapGenPoint(pre, Math.max(startOff, endOff));

  if (!a) return undefined;

  // Wenn Endpunkt auf anderes URI mappt, pinnen wir auf Startpunkt (MVP)
  if (!b || a.uri !== b.uri) {
    return {
      uri: a.uri,
      range: Range.create(a.line, a.character, a.line, a.character + Math.max(1, endOff - startOff)),
    };
  }

  return {
    uri: a.uri,
    range: Range.create(a.line, a.character, b.line, b.character),
  };
}

function mapGenPoint(pre: PreprocessedDoc, off: number): { uri: string; line: number; character: number } | undefined {
  // end exklusiv behandeln
  let seg = pre.segments.find((s) => off >= s.genStart && off < s.genEnd);
  if (!seg && off === pre.text.length && pre.segments.length) {
    seg = pre.segments[pre.segments.length - 1];
  }
  if (!seg) return undefined;

  if (seg.kind === "generated") {
    return {
      uri: seg.anchorUri,
      line: seg.anchorRange.start.line,
      character: seg.anchorRange.start.character,
    };
  }

  const rel = Math.max(0, Math.min(off - seg.genStart, seg.genEnd - seg.genStart));
  const char = Math.min(seg.sourceCharStart + rel, seg.sourceLineTextLen);
  return { uri: seg.sourceUri, line: seg.sourceLine, character: char };
}

/* -------------------- COPY parsing -------------------- */

function parseCopyStatements(text: string): CopyStmt[] {
  const toks = tokenizeCobolText(text);
  const out: CopyStmt[] = [];

  for (let i = 0; i < toks.length; i++) {
    if (toks[i].upper !== "COPY") continue;

    const errors: CopyParseError[] = [];
    const replacing: ReplacingPair[] = [];

    const nameTok = toks[i + 1];
    if (!nameTok || nameTok.kind === "dot") {
      errors.push({
        code: "COPY_MISSING_NAME",
        message: "COPY ohne Copybook-Name.",
        range: { start: toks[i].start, end: toks[i].end },
      });
      continue;
    }

    if (nameTok.kind === "pseudo" && nameTok.closed === false) {
      errors.push({
        code: "COPY_PSEUDOTEXT_UNCLOSED",
        message: "Unclosed Pseudotext: erwartet abschlieÃŸendes '=='.",
        range: { start: nameTok.start, end: nameTok.end },
      });
    }

    const copybookName = normalizeCopyName(nameTok.text);

    const stmt: CopyStmt = {
      copybookName,
      nameRange: { start: nameTok.start, end: nameTok.end },
      replacing,
      hasReplacingKeyword: false,
      replacingKeywordRange: { start: nameTok.end, end: nameTok.end },
      copyKeywordRange: { start: toks[i].start, end: toks[i].end },
      terminatedByDot: false,
      dotRange: undefined,

      errors,
    };

    // Terminierung durch '.' finden (erste DOT nach COPY)
    for (let k = i + 1; k < toks.length; k++) {
      if (toks[k].kind === "dot") {
        stmt.terminatedByDot = true;
        stmt.dotRange = { start: toks[k].start, end: toks[k].end };
        break;
      }
    }
    if (!stmt.terminatedByDot) {
      errors.push({
        code: "COPY_MISSING_PERIOD",
        message: "COPY-Statement ohne '.' (Punkt). Expansion kann unzuverlÃ¤ssig sein.",
        range: stmt.copyKeywordRange,
      });
    }

    let j = i + 2;

    while (j < toks.length && toks[j].kind !== "dot") {
      const u = toks[j].upper;
      if (u === "IN" || u === "OF") {
        j += 2;
        continue;
      }
      if (u === "SUPPRESS") {
        j += 1;
        continue;
      }
      if (u === "REPLACING") break;
      j += 1;
    }

    if (j < toks.length && toks[j].upper === "REPLACING") {
      stmt.hasReplacingKeyword = true;
      stmt.replacingKeywordRange = { start: toks[j].start, end: toks[j].end };
      j += 1;

      while (j < toks.length && toks[j].kind !== "dot") {
        if (toks[j].upper === "LEADING" || toks[j].upper === "TRAILING") {
          j += 1;
          if (j >= toks.length || toks[j].kind === "dot") break;
        }

        const fromTok = toks[j];
        if (!fromTok || fromTok.kind === "dot") break;
        j += 1;

        if (fromTok.kind === "pseudo" && fromTok.closed === false) {
          errors.push({
            code: "COPY_PSEUDOTEXT_UNCLOSED",
            message: "Unclosed Pseudotext: erwartet abschlieÃŸendes '=='.",
            range: { start: fromTok.start, end: fromTok.end },
          });
        }

        const byTok = toks[j];
        if (!byTok || byTok.kind === "dot" || byTok.upper !== "BY") {
          errors.push({
            code: "COPY_REPLACING_MISSING_BY",
            message: "COPY REPLACING: erwartet 'BY' nach dem FROM-Teil.",
            range: { start: fromTok.start, end: fromTok.end },
          });

          while (j < toks.length && toks[j].kind !== "dot" && toks[j].upper !== "BY") j++;
          if (j < toks.length && toks[j].upper === "BY") j++;
        } else {
          j += 1;
        }

        const toTok = toks[j];
        if (!toTok || toTok.kind === "dot") {
          errors.push({
            code: "COPY_REPLACING_MISSING_TO",
            message: "COPY REPLACING: erwartet TO-Teil nach 'BY'.",
            range: byTok
              ? { start: byTok.start, end: byTok.end }
              : { start: fromTok.start, end: fromTok.end },
          });
          break;
        }

        if (toTok.kind === "pseudo" && toTok.closed === false) {
          errors.push({
            code: "COPY_PSEUDOTEXT_UNCLOSED",
            message: "Unclosed Pseudotext: erwartet abschlieÃŸendes '=='.",
            range: { start: toTok.start, end: toTok.end },
          });
        }

        replacing.push({
          from: fromTok.text,
          to: toTok.text,
          fromRange: { start: fromTok.start, end: fromTok.end },
          toRange: { start: toTok.start, end: toTok.end },
          fromKind: fromTok.kind,
          toKind: toTok.kind,
        });

        j += 1;
      }
    }

    out.push(stmt);
  }

  return out;
}

function tokenizeCobolText(text: string): Tok[] {
  const toks: Tok[] = [];
  const n = text.length;
  let i = 0;

  const pushTok = (t: Omit<Tok, "upper">) => toks.push({ ...t, upper: t.text.toUpperCase() });

  while (i < n) {
    while (i < n && /\s/.test(text[i])) i++;
    if (i >= n) break;

    const ch = text[i];

    if (ch === ".") {
      pushTok({ text: ".", start: i, end: i + 1, kind: "dot" });
      i += 1;
      continue;
    }

    if (ch === "=" && i + 1 < n && text[i + 1] === "=") {
      const start = i;
      i += 2;
      const close = text.indexOf("==", i);
      if (close === -1) {
        pushTok({ text: text.slice(start), start, end: n, kind: "pseudo", closed: false });
        break;
      } else {
        const end = close + 2;
        pushTok({ text: text.slice(start, end), start, end, kind: "pseudo", closed: true });
        i = end;
        continue;
      }
    }

    if (ch === "\"" || ch === "'") {
      const quote = ch;
      const start = i;
      i += 1;
      while (i < n && text[i] !== quote) i++;
      if (i < n && text[i] === quote) i += 1;
      pushTok({ text: text.slice(start, i), start, end: i, kind: "string" });
      continue;
    }

    {
      const start = i;
      while (i < n && !/\s/.test(text[i]) && text[i] !== ".") i++;
      pushTok({ text: text.slice(start, i), start, end: i, kind: "word" });
      continue;
    }
  }

  return toks;
}

function normalizeCopyName(s: string): string {
  if ((s.startsWith("\"") && s.endsWith("\"")) || (s.startsWith("'") && s.endsWith("'"))) {
    s = s.slice(1, -1);
  }
  if (s.startsWith("==") && s.endsWith("==")) {
    s = s.slice(2, -2);
  }
  return s.replace(/\.+$/, "");
}

/* -------------------- REPLACING apply (MVP) -------------------- */

type ReplaceStat = { fromRange: TextRange; fromDisplay: string; count: number };

function applyReplacingMvp(text: string, pairs: ReplacingPair[]): { text: string; stats: ReplaceStat[] } {
  let out = text;
  const stats: ReplaceStat[] = [];

  for (const p of pairs) {
    const from = unwrapReplacingToken(p.from, p.fromKind);
    const to = unwrapReplacingToken(p.to, p.toKind);

    const { nextText, count } = replaceAllHeuristic(out, from, to, p.fromKind);
    out = nextText;

    stats.push({ fromRange: p.fromRange, fromDisplay: p.from, count });
  }

  return { text: out, stats };
}
function applyReplacingToFixedText(
  original: string,
  pairs: ReplacingPair[]
): { text: string; stats: ReplaceStat[] } {
  const slices = sliceLines(original);

  // counts pro pair
  const stats: ReplaceStat[] = pairs.map((p) => ({
    fromRange: p.fromRange,
    fromDisplay: p.from,
    count: 0,
  }));

  const outLines: string[] = [];

  for (const sl of slices) {
    if (sl.isComment) {
      outLines.push(sl.full);
      continue;
    }

    if (sl.isFixed) {
      const prefix = sl.full.slice(0, 7);          // cols 1-7 inkl indicator
      let lang = sl.lang;

      for (let i = 0; i < pairs.length; i++) {
        const p = pairs[i];
        const from = unwrapReplacingToken(p.from, p.fromKind);
        const to = unwrapReplacingToken(p.to, p.toKind);
        const r = replaceAllHeuristic(lang, from, to, p.fromKind);
        lang = r.nextText;
        stats[i].count += r.count;
      }

      // Language area klassisch 65 chars (8-72). Wir halten das â€œhalbwegsâ€ stabil:
      const langFixed = lang.padEnd(65, " ").slice(0, 65);

      outLines.push(prefix + langFixed);
    } else {
      // free format
      let line = sl.full;
      for (let i = 0; i < pairs.length; i++) {
        const p = pairs[i];
        const from = unwrapReplacingToken(p.from, p.fromKind);
        const to = unwrapReplacingToken(p.to, p.toKind);
        const r = replaceAllHeuristic(line, from, to, p.fromKind);
        line = r.nextText;
        stats[i].count += r.count;
      }
      outLines.push(line);
    }
  }

  return { text: outLines.join("\n"), stats };
}

function unwrapReplacingToken(t: string, kind: TokKind): string {
  if (kind === "pseudo" && t.startsWith("==") && t.endsWith("==")) return t.slice(2, -2);
  if (kind === "string" && ((t.startsWith("\"") && t.endsWith("\"")) || (t.startsWith("'") && t.endsWith("'"))))
    return t.slice(1, -1);
  return t;
}

function replaceAllHeuristic(haystack: string, needle: string, repl: string, needleKind: TokKind): { nextText: string; count: number } {
  if (!needle) return { nextText: haystack, count: 0 };

  if (needleKind === "word" && /^[A-Za-z0-9-]+$/.test(needle)) {
    const re = new RegExp(`(^|[^A-Za-z0-9-])(${escapeRegExp(needle)})(?=[^A-Za-z0-9-]|$)`, "g");
    let count = 0;
    const nextText = haystack.replace(re, (_m, p1) => {
      count++;
      return `${p1}${repl}`;
    });
    return { nextText, count };
  }

  let count = 0;
  let idx = 0;
  let out = "";
  while (true) {
    const j = haystack.indexOf(needle, idx);
    if (j === -1) break;
    out += haystack.slice(idx, j) + repl;
    idx = j + needle.length;
    count++;
  }
  out += haystack.slice(idx);
  return { nextText: count ? out : haystack, count };
}

function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/* -------------------- Copybook resolving + file cache -------------------- */

function resolveCopybook(name: string, baseDirs: string[]): string | undefined {
  const cacheKey = `${name}\u0000${baseDirs.join("\u0001")}`;
  const cached = copybookResolveCache.get(cacheKey);
  if (cached !== undefined) return cached ?? undefined;

  const candidates = copybookCandidates(name);
  for (const dir of baseDirs) {
    for (const cand of candidates) {
      const full = path.isAbsolute(cand) ? cand : path.join(dir, cand);
      if (fs.existsSync(full)) {
        copybookResolveCache.set(cacheKey, full);
        return full;
      }
    }
  }

  copybookResolveCache.set(cacheKey, null);
  return undefined;
}

function copybookCandidates(name: string): string[] {
  const cleaned = name.replace(/\.+$/, "");
  const ext = path.extname(cleaned);
  if (ext) return [cleaned];
  return [
    cleaned,
    `${cleaned}.cpy`,
    `${cleaned}.CPY`,
    `${cleaned}.cob`,
    `${cleaned}.COB`,
    `${cleaned}.cbl`,
    `${cleaned}.CBL`,
  ];
}

function loadTextCached(fullPath: string): string | undefined {
  try {
    const st = fs.statSync(fullPath);
    const prev = fileCache.get(fullPath);
    if (prev && prev.mtimeMs === st.mtimeMs) return prev.text;

    const text = fs.readFileSync(fullPath, "utf8");
    fileCache.set(fullPath, { mtimeMs: st.mtimeMs, text });
    return text;
  } catch {
    return undefined;
  }
}

function summarizeDirs(dirs: string[], max: number): string {
  if (dirs.length <= max) return dirs.join(", ");
  const shown = dirs.slice(0, max).join(", ");
  return `${shown}, â€¦ (+${dirs.length - max} weitere)`;
}

function fsPathFromUri(uri: string): string | undefined {
  try {
    const u = new URL(uri);
    if (u.protocol !== "file:") return undefined;
    return fileURLToPath(u);
  } catch {
    return undefined;
  }
}

function uriFromFsPath(p: string): string {
  // simple file:// URI
  let fixed = p.replace(/\\/g, "/");
  if (!fixed.startsWith("/")) fixed = "/" + fixed;
  return "file://" + fixed;
}

/* ======================= Basic checks ======================= */

function basicFixedFormatChecks(uri: string, text: string, diagsByUri: Map<string, Diagnostic[]>) {
  const lines = text.split(/\r?\n/);
  for (let lineNo = 0; lineNo < lines.length; lineNo++) {
    const full = lines[lineNo];

    const tabIdx = full.indexOf("\t");
    if (tabIdx >= 0) {
      pushDiag(diagsByUri, uri, {
        severity: DiagnosticSeverity.Warning,
        range: Range.create(lineNo, tabIdx, lineNo, tabIdx + 1),
        message: "TAB gefunden: Fixed-Columns koennen kaputt gehen. Besser Spaces verwenden.",
        source: "cobol85",
        code: "TAB_FIXED_FORMAT",
      });
    }

    if (full.length > 80) {
      pushDiag(diagsByUri, uri, {
        severity: DiagnosticSeverity.Warning,
        range: Range.create(lineNo, 80, lineNo, full.length),
        message: "Zeile ueberschreitet 80 Spalten (Fixed-Format).",
        source: "cobol85",
        code: "LINE_OVERFLOW",
      });
    }

    // Strict fixed-mode: every non-empty line must have fixed columns incl. indicator.
    if (full.trim().length === 0) continue;

    if (!hasFixedColumns(full)) {
      pushDiag(diagsByUri, uri, {
        severity: DiagnosticSeverity.Error,
        range: Range.create(lineNo, 0, lineNo, Math.max(1, full.length)),
        message: "Kein gueltiges Fixed-Format: Spalte 7 (Indicator) fehlt.",
        source: "cobol85",
        code: "FIXED_FORMAT_REQUIRED",
      });
      continue;
    }

    const indicator = full[6];
    if (!isValidFixedIndicator(indicator)) {
      const shown = indicator === " " ? "<space>" : indicator;
      pushDiag(diagsByUri, uri, {
        severity: DiagnosticSeverity.Error,
        range: Range.create(lineNo, 6, lineNo, 7),
        message: `Ungueltiger Indicator in Spalte 7: '${shown}'. Erlaubt sind Leerzeichen, '*', '/', '-', 'D'.`,
        source: "cobol85",
        code: "INVALID_FIXED_INDICATOR",
      });
    }
  }
}

function shouldRequireIdentificationDivision(uri: string, text: string): boolean {
  const fsPath = fsPathFromUri(uri);
  if (fsPath && /[\\/](copybook|copybooks)[\\/]/i.test(fsPath)) return false;

  const slices = sliceLines(text);
  const lang = slices
    .filter((s) => !s.isComment)
    .map((s) => s.lang)
    .join("\n");

  // Copybook-like snippets without division/program markers should not get this error.
  if (/\b(PROGRAM-ID|FUNCTION-ID|CLASS-ID|INTERFACE-ID)\s*\./i.test(lang)) return true;
  if (/\b(ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\s*\./i.test(lang)) return true;

  return false;
}

function hasIdentificationDivision(text: string): boolean {
  // quick heuristic on language areas
  const slices = sliceLines(text);
  return slices.some((s) => !s.isComment && /\b(?:IDENTIFICATION|ID)\s+DIVISION\b/i.test(s.lang));
}

function pushDiag(map: Map<string, Diagnostic[]>, uri: string, d: Diagnostic) {
  const arr = map.get(uri);
  if (arr) arr.push(d);
  else map.set(uri, [d]);
}

/* ======================= Symbols (Outline) ======================= */

function buildDocumentSymbols(doc: TextDocument): DocumentSymbol[] {
  const lines = doc.getText().split(/\r?\n/);

  const root: DocumentSymbol[] = [];
  let currentProgram: DocumentSymbol | undefined;
  let currentDivision: DocumentSymbol | undefined;
  let currentSection: DocumentSymbol | undefined;

  for (let lineNo = 0; lineNo < lines.length; lineNo++) {
    const full = lines[lineNo];

    if (!hasFixedColumns(full)) continue;
    const indicator = full[6];
    if (!isValidFixedIndicator(indicator) || isFixedCommentIndicator(indicator)) continue;

    const langStart = 7;
    const langEnd = Math.min(full.length, 72);
    const lang = full.slice(langStart, langEnd);

    {
      const m = /PROGRAM-ID\.\s*([A-Z0-9-]+)\s*\./i.exec(lang);
      if (m) {
        const sym = makeSymbol(
          `PROGRAM-ID ${m[1]}`,
          SymbolKind.Module,
          lineNo,
          langStart + (m.index ?? 0),
          lineNo,
          langStart + (m.index ?? 0) + m[0].length
        );
        root.push(sym);
        currentProgram = sym;
        currentDivision = undefined;
        currentSection = undefined;
        continue;
      }
    }

    {
      const m = /^\s*(IDENTIFICATION|ID|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b.*$/i.exec(lang);
      if (m) {
        const divisionName = m[1].toUpperCase() === "ID" ? "IDENTIFICATION" : m[1].toUpperCase();
        const label = `${divisionName} DIVISION`;
        const sym = makeSymbol(
          label,
          SymbolKind.Namespace,
          lineNo,
          langStart + (m.index ?? 0),
          lineNo,
          langStart + (m.index ?? 0) + m[0].length
        );
        attachSymbol(root, currentProgram, sym);
        currentDivision = sym;
        currentSection = undefined;
        continue;
      }
    }

    {
      const m = /^\s*([A-Z0-9-]+)\s+SECTION\.\s*$/i.exec(lang);
      if (m) {
        const sym = makeSymbol(
          `${m[1].toUpperCase()} SECTION`,
          SymbolKind.Namespace,
          lineNo,
          langStart + (m.index ?? 0),
          lineNo,
          langStart + (m.index ?? 0) + m[0].length
        );
        attachSymbol(root, currentDivision ?? currentProgram, sym);
        currentSection = sym;
        continue;
      }
    }

    {
      const m = /^\s*([A-Z][A-Z0-9-]*)\.\s*$/i.exec(lang);
      if (m) {
        const name = m[1].toUpperCase();
        if (!["END-IF", "END-EVALUATE"].includes(name)) {
          const sym = makeSymbol(
            `${name}.`,
            SymbolKind.Function,
            lineNo,
            langStart + (m.index ?? 0),
            lineNo,
            langStart + (m.index ?? 0) + m[0].length
          );
          attachSymbol(root, currentSection ?? currentDivision ?? currentProgram, sym);
        }
      }
    }
  }

  return root;
}

function attachSymbol(root: DocumentSymbol[], parent: DocumentSymbol | undefined, child: DocumentSymbol) {
  if (parent) parent.children = [...(parent.children ?? []), child];
  else root.push(child);
}

function makeSymbol(
  name: string,
  kind: SymbolKind,
  startLine: number,
  startChar: number,
  endLine: number,
  endChar: number
): DocumentSymbol {
  const r = Range.create(startLine, startChar, endLine, endChar);
  return { name, kind, range: r, selectionRange: r, children: [] };
}

function lintPreprocessed(text: string): GenDiag[] {
  const diags: GenDiag[] = [];

  type Frame = { kind: "IF" | "EVALUATE" | "EXEC"; startOff: number; tokenLen: number };
  type ExecTok = { upper: string; start: number; end: number };
  type ExecLintState = {
    execStartOff: number;
    execTokenLen: number;
    subtypeTok?: ExecTok;
    firstArgTok?: ExecTok;
    tokens: ExecTok[];
  };

  const stack: Frame[] = [];
  let execState: ExecLintState | undefined;

  for (const tok of scanLintTokens(text)) {
    if (tok.kind === "dot") {
      if (execState) continue;
      closeBlocksByPeriod(stack, diags);
      continue;
    }

    const u = tok.upper;
    const start = tok.start;
    const end = tok.end;

    if (execState) {
      if (u === "END-EXEC") {
        if (!popExpected(stack, "EXEC")) {
          diags.push({
            startOff: start,
            endOff: end,
            severity: DiagnosticSeverity.Error,
            code: "END_EXEC_WITHOUT_EXEC",
            message: "END-EXEC ohne passendes EXEC.",
          });
        }
        lintExecBlock(execState, diags);
        execState = undefined;
        continue;
      }

      if (!execState.subtypeTok) {
        execState.subtypeTok = { upper: u, start, end };
      } else if (!execState.firstArgTok) {
        execState.firstArgTok = { upper: u, start, end };
      }
      if (execState.tokens.length < 200) {
        execState.tokens.push({ upper: u, start, end });
      }
      continue;
    }

    // ---- Opens
    if (u === "IF") {
      stack.push({ kind: "IF", startOff: start, tokenLen: end - start });
      continue;
    }
    if (u === "EVALUATE") {
      stack.push({ kind: "EVALUATE", startOff: start, tokenLen: end - start });
      continue;
    }
    if (u === "EXEC") {
      // MVP: treat any EXEC ... END-EXEC
      stack.push({ kind: "EXEC", startOff: start, tokenLen: end - start });
      execState = { execStartOff: start, execTokenLen: end - start, tokens: [] };
      continue;
    }

    // ---- Closes
    if (u === "END-IF") {
      if (!popExpected(stack, "IF")) {
        diags.push({
          startOff: start,
          endOff: end,
          severity: DiagnosticSeverity.Error,
          code: "END_IF_WITHOUT_IF",
          message: "END-IF ohne passendes IF.",
        });
      }
      continue;
    }

    if (u === "END-EVALUATE") {
      if (!popExpected(stack, "EVALUATE")) {
        diags.push({
          startOff: start,
          endOff: end,
          severity: DiagnosticSeverity.Error,
          code: "END_EVALUATE_WITHOUT_EVALUATE",
          message: "END-EVALUATE ohne passendes EVALUATE.",
        });
      }
      continue;
    }

    if (u === "END-EXEC") {
      if (!popExpected(stack, "EXEC")) {
        diags.push({
          startOff: start,
          endOff: end,
          severity: DiagnosticSeverity.Error,
          code: "END_EXEC_WITHOUT_EXEC",
          message: "END-EXEC ohne passendes EXEC.",
        });
      }
      continue;
    }
  }

  if (execState) lintExecBlock(execState, diags);

  lintDataDivisionEntryPeriods(text, diags);
  lintProcedureVerbTypos(text, diags);

  // ---- Unclosed blocks at EOF
  for (let i = stack.length - 1; i >= 0; i--) {
    const f = stack[i];
    const expected =
      f.kind === "IF" ? "END-IF" : f.kind === "EVALUATE" ? "END-EVALUATE" : "END-EXEC";

    diags.push({
      startOff: f.startOff,
      endOff: f.startOff + f.tokenLen,
      severity: DiagnosticSeverity.Error,
      code: "UNCLOSED_BLOCK",
      message: `${f.kind} erÃ¶ffnet, aber kein ${expected} gefunden.`,
    });
  }

  return diags;
}

function lintDataDivisionEntryPeriods(text: string, diags: GenDiag[]): void {
  type PendingEntry = { startOff: number; tokenLen: number };
  const lines = text.split("\n");

  let inDataDivision = false;
  let pending: PendingEntry | undefined;
  let off = 0;

  const flushPending = () => {
    if (!pending) return;
    diags.push({
      startOff: pending.startOff,
      endOff: pending.startOff + pending.tokenLen,
      severity: DiagnosticSeverity.Error,
      code: "DATA_ENTRY_MISSING_PERIOD",
      message: "Data-Description-Entry ohne abschliessenden Punkt (Separator Period).",
    });
    pending = undefined;
  };

  for (const line of lines) {
    const langStart = line.length >= 7 ? 7 : 0;
    const lang = line.slice(langStart);
    const trimmedStartLen = lang.length - lang.trimStart().length;
    const trimmed = lang.trim();
    const lineCodeStart = off + langStart + trimmedStartLen;

    if (/^(IDENTIFICATION|ENVIRONMENT|PROCEDURE)\s+DIVISION\b/i.test(trimmed)) {
      flushPending();
      inDataDivision = false;
    } else if (/^DATA\s+DIVISION\b/i.test(trimmed)) {
      flushPending();
      inDataDivision = true;
    }

    if (inDataDivision && trimmed.length > 0) {
      const m = /^(FD|SD|RD|CD|0?[1-9]|[1-4][0-9]|66|77|88)\b/i.exec(trimmed);
      const startsEntry = !!m;

      if (pending && startsEntry) {
        // A new entry starts before the previous one has seen a separator period.
        flushPending();
      }

      if (startsEntry && m) {
        pending = { startOff: lineCodeStart, tokenLen: m[0].length };
      }

      if (pending && hasSeparatorPeriodOutsideLiterals(trimmed)) {
        pending = undefined;
      }
    }

    off += line.length + 1;
  }

  flushPending();
}

function lintProcedureVerbTypos(text: string, diags: GenDiag[]): void {
  const lines = text.split("\n");
  let inProcedureDivision = false;
  let off = 0;

  const knownStatementLeaders = new Set([
    "ACCEPT", "ADD", "ALTER", "CALL", "CANCEL", "CLOSE", "COMPUTE", "CONTINUE",
    "COPY", "DELETE", "DISPLAY", "DIVIDE", "ELSE", "END-ACCEPT", "END-ADD",
    "END-CALL", "END-COMPUTE", "END-DELETE", "END-DISPLAY", "END-DIVIDE",
    "END-EVALUATE", "END-IF", "END-MULTIPLY", "END-PERFORM", "END-READ",
    "END-RECEIVE", "END-RETURN", "END-REWRITE", "END-SEARCH", "END-START",
    "END-STRING", "END-SUBTRACT", "END-UNSTRING", "END-WRITE", "ENTRY",
    "EVALUATE", "EXEC", "EXIT", "GOBACK", "GO", "IF", "INITIALIZE", "INITIATE",
    "INSPECT", "MERGE", "MOVE", "MULTIPLY", "NEXT", "OPEN", "PERFORM",
    "READ", "RELEASE", "RETURN", "REWRITE", "SEARCH", "SET", "SORT", "START",
    "STOP", "STRING", "SUBTRACT", "UNSTRING", "USE", "WHEN", "WRITE",
  ]);

  const maybeClauseLeaders = new Set([
    "AT", "NOT", "ON", "INVALID", "SIZE", "WITH", "OR", "AND",
  ]);

  const knownVerbsForSuggestion = [
    "ACCEPT", "ADD", "ALTER", "CALL", "CANCEL", "CLOSE", "COMPUTE", "CONTINUE",
    "COPY", "DELETE", "DISPLAY", "DIVIDE", "EVALUATE", "EXIT", "GOBACK", "GO",
    "IF", "INITIALIZE", "INITIATE", "INSPECT", "MERGE", "MOVE", "MULTIPLY",
    "OPEN", "PERFORM", "READ", "RELEASE", "RETURN", "REWRITE", "SEARCH", "SET",
    "SORT", "START", "STOP", "STRING", "SUBTRACT", "UNSTRING", "WRITE",
  ];

  for (const line of lines) {
    const langStart = line.length >= 7 ? 7 : 0;
    const lang = line.slice(langStart);
    const trimmedStartLen = lang.length - lang.trimStart().length;
    const trimmed = lang.trim();
    const lineCodeStart = off + langStart + trimmedStartLen;

    if (/^(IDENTIFICATION|ENVIRONMENT|DATA)\s+DIVISION\b/i.test(trimmed)) {
      inProcedureDivision = false;
    } else if (/^PROCEDURE\s+DIVISION\b/i.test(trimmed)) {
      inProcedureDivision = true;
      off += line.length + 1;
      continue;
    }

    if (inProcedureDivision && trimmed.length > 0) {
      // Paragraph/Section labels are not executable statement verbs.
      if (!/^[A-Z0-9-]+\s*(SECTION)?\s*\.$/i.test(trimmed)) {
        const m = /^([A-Z][A-Z0-9-]*)\b/i.exec(trimmed);
        if (m) {
          const token = m[1].toUpperCase();

          const isKnown =
            knownStatementLeaders.has(token) ||
            maybeClauseLeaders.has(token);

          if (!isKnown && /^[A-Z]+$/.test(token)) {
            const suggestion = suggestProcedureVerb(token, knownVerbsForSuggestion);
            if (suggestion) {
              diags.push({
                startOff: lineCodeStart,
                endOff: lineCodeStart + token.length,
                severity: DiagnosticSeverity.Error,
                code: "PROCEDURE_VERB_UNKNOWN",
                message: `Unbekanntes COBOL-Statement: ${token}. Meintest du ${suggestion}?`,
              });
            }
          }
        }
      }
    }

    off += line.length + 1;
  }
}

function suggestProcedureVerb(token: string, knownVerbs: string[]): string | undefined {
  if (token.length < 4) return undefined;
  if (!/^[A-Z]+$/.test(token)) return undefined;

  let bestKeyword: string | undefined;
  let bestDistance = Number.POSITIVE_INFINITY;

  for (const kw of knownVerbs) {
    const limit = kw.length >= 8 ? 2 : 1;
    const dist = levenshteinDistanceWithinLimit(token, kw, 2);
    if (dist > limit) continue;
    if (dist < bestDistance) {
      bestDistance = dist;
      bestKeyword = kw;
    }
  }

  return bestKeyword;
}

function hasSeparatorPeriodOutsideLiterals(s: string): boolean {
  let inSingle = false;
  let inDouble = false;

  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch === "'" && !inDouble) {
      inSingle = !inSingle;
      continue;
    }
    if (ch === "\"" && !inSingle) {
      inDouble = !inDouble;
      continue;
    }
    if (ch !== "." || inSingle || inDouble) continue;

    const prev = i > 0 ? s[i - 1] : " ";
    const next = i + 1 < s.length ? s[i + 1] : " ";
    const isDecimalDot = /[0-9]/.test(prev) && /[0-9]/.test(next);
    if (!isDecimalDot) return true;
  }

  return false;
}

function lintExecBlock(
  exec: {
    execStartOff: number;
    execTokenLen: number;
    subtypeTok?: { upper: string; start: number; end: number };
    firstArgTok?: { upper: string; start: number; end: number };
    tokens: { upper: string; start: number; end: number }[];
  },
  diags: GenDiag[]
) {
  if (!exec.subtypeTok) {
    diags.push({
      startOff: exec.execStartOff,
      endOff: exec.execStartOff + exec.execTokenLen,
      severity: DiagnosticSeverity.Error,
      code: "EXEC_MISSING_TYPE",
      message: "EXEC ohne Typ. Erwartet z. B. DLI, CICS oder SQL.",
    });
    return;
  }

  const subtype = exec.subtypeTok.upper;

  if (subtype === "DLI") {
    if (!exec.firstArgTok) {
      diags.push({
        startOff: exec.subtypeTok.start,
        endOff: exec.subtypeTok.end,
        severity: DiagnosticSeverity.Error,
        code: "EXEC_DLI_MISSING_CALL",
        message: "EXEC DLI ohne IMS-Aufruf (z. B. GU, GN, GNP, GHU, ISRT, REPL).",
      });
      return;
    }

    // IBM DLI request names (batch/BMP + DBCTL/CICS variants).
    const knownDliCalls = new Set([
      "GU", "GN", "GNP", "GHU", "GHN", "GHNP",
      "ISRT", "REPL", "DLET", "PURG", "LOAD",
      "CHKP", "SYMCHKP", "XRST", "SYNC", "DEQ",
      "ROLL", "ROLB", "ROLS", "STAT", "LOG",
      "INIT", "TERM", "PCB", "SETS", "POS",
      "GMSG", "ICMD", "RCMD", "CMD", "GCMD",
      "CHNG", "GSCD", "RETRIEVE", "QUERY",
      "ACCEPT", "REFRESH", "SCHD"
    ]);

    const req = resolveDliRequest(exec.tokens);
    const requestForLookup = req.request ?? exec.firstArgTok.upper;
    if (!knownDliCalls.has(requestForLookup)) {
      diags.push({
        startOff: exec.firstArgTok.start,
        endOff: exec.firstArgTok.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_UNKNOWN_CALL",
        message: `EXEC DLI mit unbekanntem IMS-Aufruf: ${exec.firstArgTok.upper}.`,
      });
    }

    if (req.request) {
      validateDliClausesByRequest(req.request, req.requestTokenCount, exec.tokens, diags);
    }

    const usingIdx = exec.tokens.findIndex((t) => t.upper === "USING");
    if (usingIdx >= 0) {
      const afterUsing = exec.tokens[usingIdx + 1];
      if (!afterUsing) {
        diags.push({
          startOff: exec.tokens[usingIdx].start,
          endOff: exec.tokens[usingIdx].end,
          severity: DiagnosticSeverity.Error,
          code: "EXEC_DLI_USING_MISSING_TARGET",
          message: "EXEC DLI USING ohne Ziel. Erwartet z. B. PCB(n).",
        });
      } else if (afterUsing.upper === "PSB") {
        diags.push({
          startOff: afterUsing.start,
          endOff: afterUsing.end,
          severity: DiagnosticSeverity.Error,
          code: "EXEC_DLI_USING_PSB_INVALID",
          message: "USING PSB(...) ist in EXEC DLI ungueltig. Verwende USING PCB(...) oder AIB(...).",
        });
      } else if (afterUsing.upper !== "PCB" && afterUsing.upper !== "AIB") {
        diags.push({
          startOff: afterUsing.start,
          endOff: afterUsing.end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_USING_UNKNOWN_TARGET",
          message: `EXEC DLI USING mit unbekanntem Ziel: ${afterUsing.upper}. Erwartet typischerweise PCB oder AIB.`,
        });
      }
    }
    return;
  }

  if (subtype === "CICS" && !exec.firstArgTok) {
    diags.push({
      startOff: exec.subtypeTok.start,
      endOff: exec.subtypeTok.end,
      severity: DiagnosticSeverity.Error,
      code: "EXEC_CICS_MISSING_COMMAND",
      message: "EXEC CICS ohne Kommando (z. B. LINK, XCTL, READ, WRITE, RETURN).",
    });
  }
}

function resolveDliRequest(tokens: { upper: string; start: number; end: number }[]): { request?: string; requestTokenCount: number } {
  // tokens layout in EXEC block: [DLI, <request...>, <clauses...>]
  const t1 = tokens[1]?.upper;
  const t2 = tokens[2]?.upper;
  const t3 = tokens[3]?.upper;
  const t4 = tokens[4]?.upper;
  const t5 = tokens[5]?.upper;

  if (!t1) return { requestTokenCount: 0 };

  if (t1 === "GET") {
    if (t2 === "UNIQUE") return { request: "GU", requestTokenCount: 2 };
    if (t2 === "NEXT" && t3 === "IN" && t4 === "PARENT") return { request: "GNP", requestTokenCount: 4 };
    if (t2 === "NEXT") return { request: "GN", requestTokenCount: 2 };
    if (t2 === "HOLD" && t3 === "UNIQUE") return { request: "GHU", requestTokenCount: 3 };
    if (t2 === "HOLD" && t3 === "NEXT" && t4 === "IN" && t5 === "PARENT") {
      return { request: "GHNP", requestTokenCount: 5 };
    }
    if (t2 === "HOLD" && t3 === "NEXT") return { request: "GHN", requestTokenCount: 3 };
    return { requestTokenCount: 1 };
  }

  if (t1 === "INSERT") return { request: "ISRT", requestTokenCount: 1 };
  if (t1 === "REPLACE") return { request: "REPL", requestTokenCount: 1 };
  if (t1 === "DELETE") return { request: "DLET", requestTokenCount: 1 };
  if (t1 === "CHECKPOINT") return { request: "CHKP", requestTokenCount: 1 };
  if (t1 === "SYMBOLIC" && t2 === "CHECKPOINT") return { request: "SYMCHKP", requestTokenCount: 2 };

  return { request: t1, requestTokenCount: 1 };
}

function validateDliClausesByRequest(
  request: string,
  requestTokenCount: number,
  tokens: { upper: string; start: number; end: number }[],
  diags: GenDiag[]
) {
  const clauseKeywords = new Set([
    "USING",
    "VARIABLE",
    "SEGMENT",
    "INTO",
    "FROM",
    "SEGLENGTH",
    "WHERE",
    "FIELDLENGTH",
    "KEYS",
    "KEYLENGTH",
    "SETPARENT",
    "VSAM",
    "FORMATTED",
    "LENGTH",
  ]);

  const requestSpecs: Record<string, { allowed: Set<string>; required: Set<string> }> = {
    GU: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    GN: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    GNP: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    GHU: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    GHN: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    GHNP: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "INTO", "SEGLENGTH", "WHERE", "FIELDLENGTH", "KEYS", "KEYLENGTH", "SETPARENT"]),
      required: new Set(["USING"]),
    },
    ISRT: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "FROM", "SEGLENGTH", "KEYS", "KEYLENGTH", "WHERE", "FIELDLENGTH"]),
      required: new Set(["USING", "SEGMENT"]),
    },
    REPL: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "FROM", "SEGLENGTH"]),
      required: new Set(["USING", "SEGMENT"]),
    },
    DLET: {
      allowed: new Set(["USING", "VARIABLE", "SEGMENT", "FROM", "SEGLENGTH", "KEYS", "KEYLENGTH", "WHERE", "FIELDLENGTH"]),
      required: new Set(["USING", "SEGMENT"]),
    },
    STAT: {
      allowed: new Set(["USING", "INTO", "VSAM", "FORMATTED", "LENGTH"]),
      required: new Set(["USING"]),
    },
    QUERY: {
      allowed: new Set(["USING"]),
      required: new Set(["USING"]),
    },
    TERM: {
      allowed: new Set<string>(),
      required: new Set<string>(),
    },
  };

  const spec = requestSpecs[request];
  if (!spec) return;

  const clauseStart = Math.min(tokens.length, 1 + Math.max(0, requestTokenCount));
  const seen = new Set<string>();
  const clauseKeywordList = Array.from(clauseKeywords);

  for (let i = clauseStart; i < tokens.length; i++) {
    const t = tokens[i];
    if (clauseKeywords.has(t.upper)) {
      seen.add(t.upper);

      if (!spec.allowed.has(t.upper)) {
        diags.push({
          startOff: t.start,
          endOff: t.end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_CLAUSE_NOT_ALLOWED",
          message: `Klausel ${t.upper} ist fuer EXEC DLI ${request} nicht vorgesehen.`,
        });
      }
      continue;
    }

    const suggestion = suggestDliClauseKeyword(t.upper, clauseKeywordList);
    if (suggestion && isLikelyDliClausePosition(tokens, i, clauseStart, clauseKeywords)) {
      diags.push({
        startOff: t.start,
        endOff: t.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_CLAUSE_UNKNOWN",
        message: `Unbekannte DLI-Klausel: ${t.upper}. Meintest du ${suggestion}?`,
      });
    }
  }

  for (const needed of spec.required) {
    if (!seen.has(needed)) {
      diags.push({
        startOff: tokens[1]?.start ?? tokens[0]?.start ?? 0,
        endOff: tokens[1]?.end ?? tokens[0]?.end ?? 1,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_CLAUSE_MISSING",
        message: `Bei EXEC DLI ${request} fehlt die erwartete Klausel ${needed}.`,
      });
    }
  }

  validateDliWhereClause(request, clauseStart, tokens, clauseKeywords, diags);
}

function isLikelyDliClausePosition(
  tokens: { upper: string; start: number; end: number }[],
  idx: number,
  clauseStart: number,
  clauseKeywords: Set<string>
): boolean {
  if (idx <= clauseStart) return true;

  const next = tokens[idx + 1]?.upper;

  const win = 3;
  const left = Math.max(clauseStart, idx - win);
  const right = Math.min(tokens.length - 1, idx + win);

  for (let i = idx - 1; i >= left; i--) {
    if (clauseKeywords.has(tokens[i].upper)) return true;
  }
  for (let i = idx + 1; i <= right; i++) {
    if (clauseKeywords.has(tokens[i].upper)) return true;
  }

  if (next && /^[0-9]+$/.test(next)) return true;

  return false;
}

function suggestDliClauseKeyword(token: string, clauseKeywords: string[]): string | undefined {
  if (token.length < 4) return undefined;
  if (!/^[A-Z]+$/.test(token)) return undefined;
  if (token.includes("-")) return undefined;
  if (/^[0-9]+$/.test(token)) return undefined;

  let bestKeyword: string | undefined;
  let bestDistance = Number.POSITIVE_INFINITY;

  for (const kw of clauseKeywords) {
    const limit = kw.length >= 8 ? 2 : 1;
    const dist = levenshteinDistanceWithinLimit(token, kw, 2);
    if (dist > limit) continue;
    if (dist < bestDistance) {
      bestDistance = dist;
      bestKeyword = kw;
    }
  }

  return bestKeyword;
}

function levenshteinDistanceWithinLimit(a: string, b: string, limit: number): number {
  if (a === b) return 0;

  const al = a.length;
  const bl = b.length;
  if (Math.abs(al - bl) > limit) return limit + 1;

  let prev = new Array<number>(bl + 1);
  let curr = new Array<number>(bl + 1);

  for (let j = 0; j <= bl; j++) prev[j] = j;

  for (let i = 1; i <= al; i++) {
    curr[0] = i;
    let minRow = curr[0];

    const ca = a.charCodeAt(i - 1);
    for (let j = 1; j <= bl; j++) {
      const cost = ca === b.charCodeAt(j - 1) ? 0 : 1;
      curr[j] = Math.min(
        prev[j] + 1,
        curr[j - 1] + 1,
        prev[j - 1] + cost
      );
      if (curr[j] < minRow) minRow = curr[j];
    }

    if (minRow > limit) return limit + 1;
    [prev, curr] = [curr, prev];
  }

  return prev[bl];
}

function validateDliWhereClause(
  request: string,
  clauseStart: number,
  tokens: { upper: string; start: number; end: number }[],
  clauseKeywords: Set<string>,
  diags: GenDiag[]
) {
  type SegmentWhereState = {
    whereIdxs: number[];
    firstFieldLengthIdx: number;
  };

  const segmentStates: SegmentWhereState[] = [{ whereIdxs: [], firstFieldLengthIdx: -1 }];
  let currentState = segmentStates[0];

  for (let i = clauseStart; i < tokens.length; i++) {
    const u = tokens[i].upper;

    if (u === "SEGMENT" && i > clauseStart) {
      currentState = { whereIdxs: [], firstFieldLengthIdx: -1 };
      segmentStates.push(currentState);
      continue;
    }

    if (u === "WHERE") currentState.whereIdxs.push(i);
    if (u === "FIELDLENGTH" && currentState.firstFieldLengthIdx < 0) currentState.firstFieldLengthIdx = i;
  }

  const allWhereIdxs: number[] = [];

  for (const state of segmentStates) {
    allWhereIdxs.push(...state.whereIdxs);

    if (state.whereIdxs.length === 0) {
      if (state.firstFieldLengthIdx >= 0) {
        const t = tokens[state.firstFieldLengthIdx];
        diags.push({
          startOff: t.start,
          endOff: t.end,
          severity: DiagnosticSeverity.Warning,
          code: "EXEC_DLI_FIELDLENGTH_WITHOUT_WHERE",
          message: `FIELDLENGTH ohne WHERE bei EXEC DLI ${request}.`,
        });
      }
      continue;
    }

    if (state.firstFieldLengthIdx >= 0 && state.firstFieldLengthIdx < state.whereIdxs[0]) {
      const t = tokens[state.firstFieldLengthIdx];
      diags.push({
        startOff: t.start,
        endOff: t.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_FIELDLENGTH_BEFORE_WHERE",
        message: `FIELDLENGTH steht vor WHERE bei EXEC DLI ${request}.`,
      });
    }

    for (let i = 1; i < state.whereIdxs.length; i++) {
      const t = tokens[state.whereIdxs[i]];
      diags.push({
        startOff: t.start,
        endOff: t.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_WHERE_DUPLICATE",
        message: `Mehrfaches WHERE bei EXEC DLI ${request}.`,
      });
    }
  }

  for (const whereIdx of allWhereIdxs) {
    const whereTok = tokens[whereIdx];
    const nextClauseIdx = findNextClauseKeywordIndex(tokens, whereIdx + 1, clauseKeywords);
    if (!hasNonClauseToken(tokens, whereIdx + 1, nextClauseIdx, clauseKeywords)) {
      diags.push({
        startOff: whereTok.start,
        endOff: whereTok.end,
        severity: DiagnosticSeverity.Warning,
        code: "EXEC_DLI_WHERE_EMPTY",
        message: `WHERE ohne erkennbaren Suchausdruck bei EXEC DLI ${request}.`,
      });
    }
  }
}

function findNextClauseKeywordIndex(
  tokens: { upper: string; start: number; end: number }[],
  startIdx: number,
  clauseKeywords: Set<string>
): number {
  for (let i = startIdx; i < tokens.length; i++) {
    if (clauseKeywords.has(tokens[i].upper)) return i;
  }
  return tokens.length;
}

function hasNonClauseToken(
  tokens: { upper: string; start: number; end: number }[],
  startIdx: number,
  endIdx: number,
  clauseKeywords: Set<string>
): boolean {
  for (let i = startIdx; i < endIdx; i++) {
    if (!clauseKeywords.has(tokens[i].upper)) return true;
  }
  return false;
}

type LintTok =
  | { kind: "word"; upper: string; start: number; end: number }
  | { kind: "dot"; start: number; end: number };
type OffsetRange = { start: number; end: number };

function scanLintTokens(text: string): LintTok[] {
  const toks: LintTok[] = [];
  const n = text.length;
  let i = 0;

  const isWordChar = (ch: string) => /[A-Za-z0-9-]/.test(ch);
  const isDigit = (ch: string) => /[0-9]/.test(ch);

  while (i < n) {
    const ch = text[i];

    // Inline-Kommentar im free-format: *>
    if (ch === "*" && i + 1 < n && text[i + 1] === ">") {
      i += 2;
      while (i < n && text[i] !== "\n") i++;
      continue;
    }

    // Stringliterale fÃ¼r die Keyword-Erkennung auslassen.
    if (ch === "'" || ch === "\"") {
      const quote = ch;
      i++;
      while (i < n) {
        if (text[i] === quote) {
          // COBOL escaped quote: '' oder ""
          if (i + 1 < n && text[i + 1] === quote) {
            i += 2;
            continue;
          }
          i++;
          break;
        }
        i++;
      }
      continue;
    }

    if (ch === ".") {
      const prev = i > 0 ? text[i - 1] : "";
      const next = i + 1 < n ? text[i + 1] : "";
      // Dezimalpunkt nicht als Statement-Terminierung werten.
      if (!(isDigit(prev) && isDigit(next))) {
        toks.push({ kind: "dot", start: i, end: i + 1 });
      }
      i++;
      continue;
    }

    if (isWordChar(ch)) {
      const start = i;
      i++;
      while (i < n && isWordChar(text[i])) i++;
      const upper = text.slice(start, i).toUpperCase();
      toks.push({ kind: "word", upper, start, end: i });
      continue;
    }

    i++;
  }

  return toks;
}

function collectExecDliRanges(text: string): OffsetRange[] {
  const toks = scanLintTokens(text);
  const out: OffsetRange[] = [];

  let execStart: number | undefined;
  let subtype: string | undefined;

  for (const t of toks) {
    if (t.kind !== "word") continue;
    const u = t.upper;

    if (execStart === undefined) {
      if (u === "EXEC") {
        execStart = t.start;
        subtype = undefined;
      }
      continue;
    }

    if (!subtype) {
      subtype = u;
      continue;
    }

    if (u === "END-EXEC") {
      if (subtype === "DLI") out.push({ start: execStart, end: t.end });
      execStart = undefined;
      subtype = undefined;
    }
  }

  if (execStart !== undefined && subtype === "DLI") out.push({ start: execStart, end: text.length });
  return out;
}

function overlapsAnyRange(start: number, end: number, ranges: OffsetRange[]): boolean {
  for (const r of ranges) {
    if (start < r.end && end > r.start) return true;
  }
  return false;
}

function closeBlocksByPeriod(
  stack: { kind: "IF" | "EVALUATE" | "EXEC"; startOff: number; tokenLen: number }[],
  diags: GenDiag[]
) {
  while (stack.length > 0) {
    const top = stack[stack.length - 1];
    if (top.kind === "EXEC") return;

    const f = stack.pop()!;
    const expected = f.kind === "IF" ? "END-IF" : "END-EVALUATE";

    diags.push({
      startOff: f.startOff,
      endOff: f.startOff + f.tokenLen,
      severity: DiagnosticSeverity.Warning,
      code: "BLOCK_CLOSED_BY_PERIOD",
      message: `${f.kind} endet durch '.' statt ${expected}. Bitte ${expected} verwenden.`,
    });
  }
}

function popExpected(stack: { kind: string }[], expected: string): boolean {
  if (stack.length === 0) return false;
  const top = stack[stack.length - 1];
  if (top.kind === expected) {
    stack.pop();
    return true;
  }
  return false;
}

function utf8ByteOffsetToUtf16Index(text: string, byteOff: number): number {
  if (byteOff <= 0) return 0;
  const buf = Buffer.from(text, "utf8");
  if (byteOff >= buf.length) return text.length;
  return Buffer.from(buf.subarray(0, byteOff)).toString("utf8").length;
}

function computeTreeEdit(oldText: string, newText: string): TSTreeEdit | undefined {
  if (oldText === newText) return undefined;

  const oldLen = oldText.length;
  const newLen = newText.length;
  const minLen = Math.min(oldLen, newLen);

  let start = 0;
  while (start < minLen && oldText.charCodeAt(start) === newText.charCodeAt(start)) start++;

  let oldEnd = oldLen;
  let newEnd = newLen;
  while (
    oldEnd > start &&
    newEnd > start &&
    oldText.charCodeAt(oldEnd - 1) === newText.charCodeAt(newEnd - 1)
  ) {
    oldEnd--;
    newEnd--;
  }

  return {
    startIndex: utf16IndexToUtf8ByteOffset(oldText, start),
    oldEndIndex: utf16IndexToUtf8ByteOffset(oldText, oldEnd),
    newEndIndex: utf16IndexToUtf8ByteOffset(newText, newEnd),
    startPosition: utf16IndexToTreeSitterPoint(oldText, start),
    oldEndPosition: utf16IndexToTreeSitterPoint(oldText, oldEnd),
    newEndPosition: utf16IndexToTreeSitterPoint(newText, newEnd),
  };
}

function utf16IndexToUtf8ByteOffset(text: string, utf16Index: number): number {
  const lim = Math.max(0, Math.min(utf16Index, text.length));
  let bytes = 0;

  for (let i = 0; i < lim; i++) {
    const c = text.charCodeAt(i);
    if (c <= 0x7f) {
      bytes += 1;
      continue;
    }
    if (c <= 0x7ff) {
      bytes += 2;
      continue;
    }

    if (c >= 0xd800 && c <= 0xdbff && i + 1 < lim) {
      const next = text.charCodeAt(i + 1);
      if (next >= 0xdc00 && next <= 0xdfff) {
        bytes += 4;
        i++;
        continue;
      }
    }

    bytes += 3;
  }

  return bytes;
}

function utf16IndexToTreeSitterPoint(text: string, utf16Index: number): TSTreePoint {
  const lim = Math.max(0, Math.min(utf16Index, text.length));
  let row = 0;
  let column = 0;

  for (let i = 0; i < lim; i++) {
    const c = text.charCodeAt(i);

    if (c === 0x0a) {
      row++;
      column = 0;
      continue;
    }

    if (c <= 0x7f) {
      column += 1;
      continue;
    }
    if (c <= 0x7ff) {
      column += 2;
      continue;
    }

    if (c >= 0xd800 && c <= 0xdbff && i + 1 < lim) {
      const next = text.charCodeAt(i + 1);
      if (next >= 0xdc00 && next <= 0xdfff) {
        column += 4;
        i++;
        continue;
      }
    }

    column += 3;
  }

  return { row, column };
}

type TsErr = { startByte: number; endByte: number; row: number; col: number; };

function isLikelyGlobalParserFailure(err: TsErr, textLength: number): boolean {
  const span = Math.max(0, err.endByte - err.startByte);
  return err.startByte <= 16 && span >= Math.max(512, Math.floor(textLength * 0.7));
}

function filterTreeSitterErrors(errs: TsErr[], textLength: number): TsErr[] {
  if (errs.length === 0) return errs;
  return errs.filter((e, idx) => !(idx === 0 && isLikelyGlobalParserFailure(e, textLength)));
}

function collectTreeSitterErrors(root: any, max = 25): TsErr[] {
  const out: TsErr[] = [];
  const seenRows = new Set<number>();

  const visit = (node: any) => {
    if (!node || out.length >= max) return;

    if (node.isError || node.type === "ERROR" || node.isMissing) {
      const row = node.startPosition?.row ?? 0;
      if (!seenRows.has(row)) {
        seenRows.add(row);
        out.push({
          startByte: node.startIndex,
          endByte: Math.max(node.endIndex, node.startIndex + 1),
          row,
          col: node.startPosition?.column ?? 0
        });
      }
      return; // nicht tiefer absteigen -> weniger Spam
    }

    for (const c of node.children ?? []) visit(c);
  };

  visit(root);
  return out;
}

function excerpt(s: string, max: number): string {
  return (s || "").replace(/\s+/g, " ").slice(0, max);
}

function clampRangeToSingleLine(uri: string, r: Range): Range {
  const start = r.start;
  let end = r.end;

  if (end.line !== start.line) {
    const lineLen = getLineLength(uri, start.line);
    end = { line: start.line, character: lineLen };
  }

  // falls Range aus Versehen leer wird: mindestens 1 Zeichen markieren
  if (end.character <= start.character) {
    end = { line: start.line, character: start.character + 1 };
  }

  return Range.create(start.line, start.character, end.line, end.character);
}

function getLineLength(uri: string, line: number): number {
  const open = documents.get(uri);
  if (open) {
    const lines = open.getText().split(/\r?\n/);
    return lines[line]?.length ?? 0;
  }

  const fsPath = fsPathFromUri(uri);
  if (!fsPath) return 0;
  const t = loadTextCached(fsPath);
  if (!t) return 0;
  return t.split(/\r?\n/)[line]?.length ?? 0;
}

documents.listen(connection);
connection.listen();
