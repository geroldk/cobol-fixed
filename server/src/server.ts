/**
 * COBOL-85 Fixed-Format Language Server — LSP orchestration.
 *
 * This module wires up the LSP connection and delegates to focused modules:
 *   types.ts, utils.ts, normalizer.ts, preprocessor.ts,
 *   lint.ts, symbols.ts, parser-bridge.ts
 */
import { Parser as TSParser, Language as TSLanguage } from "web-tree-sitter";
import {
  createConnection,
  Diagnostic,
  DiagnosticSeverity,
  DidChangeWatchedFilesNotification,
  InitializeParams,
  InitializeResult,
  ProposedFeatures,
  Range,
  TextDocumentSyncKind,
} from "vscode-languageserver/node";
import { TextDocuments } from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import * as path from "path";
import { performance } from "perf_hooks";

// ---- Module imports ----
import {
  Cobol85Settings,
  DEFAULT_SETTINGS,
  ForceValidateParams,
  ValidateProfile,
} from "./types";

import {
  fsPathFromUri,
  loadTextCached,
  pushDiag,
  fileCache,
  copybookResolveCache,
  findFirstCodeAnchor,
} from "./utils";

import {
  normalizeForCobol85Parser,
  mapNormalizedOffsetToSource,
} from "./normalizer";

import {
  preprocessUri,
  mapGenRange,
  mapGenPoint,
} from "./preprocessor";

import {
  basicFixedFormatChecks,
  shouldRequireIdentificationDivision,
  hasIdentificationDivision,
  lintPreprocessed,
  collectExecDliRanges,
  overlapsAnyRange,
} from "./lint";

import { buildDocumentSymbols } from "./symbols";

import {
  computeTreeEdit,
  utf8ByteOffsetToUtf16Index,
  collectTreeSitterErrors,
  filterTreeSitterErrors,
} from "./parser-bridge";

import {
  findCopybookDefinition,
  findSymbolInIndex,
  wordAtPosition,
  buildDefinitionIndex,
} from "./definition";

import { buildHover } from "./hover";

import { buildCompletionItems } from "./completion";

import { buildReferences } from "./references";

import {
  TOKEN_TYPES,
  TOKEN_MODIFIERS,
  buildSemanticTokens,
  encodeSemanticTokens,
} from "./semantic-tokens";

import { prepareRename, performRename } from "./rename";

import { lintDeadCode } from "./dead-code";

// ======================= Globals =======================

const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let workspaceFolderPaths: string[] = [];
let hasConfigurationCapability = false;
let currentSettings: Cobol85Settings = DEFAULT_SETTINGS;

// ======================= Tree-sitter state =======================

let tsInitPromise: Promise<void> | null = null;
let tsParser: TSParser | null = null;
type TSTree = NonNullable<ReturnType<TSParser["parse"]>>;
type ParserState = { tree: TSTree; parseText: string };
const parserStateByUri = new Map<string, ParserState>();

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
      tsInitPromise = null; // allow retry on next call
      throw e;
    }
  })();

  return tsInitPromise;
}

// ======================= Scheduling state =======================

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

// "published uris" clearing -- per document, so multiple open docs don't clobber each other
const publishedUrisByDoc = new Map<string, Set<string>>();

// ======================= Connection events =======================

connection.onInitialize((params: InitializeParams): InitializeResult => {
  hasConfigurationCapability = !!params.capabilities.workspace?.configuration;

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
      definitionProvider: true,
      hoverProvider: true,
      completionProvider: {
        triggerCharacters: [" ", "."],
        resolveProvider: false,
      },
      referencesProvider: true,
      semanticTokensProvider: {
        legend: {
          tokenTypes: [...TOKEN_TYPES],
          tokenModifiers: [...TOKEN_MODIFIERS],
        },
        full: true,
      },
      renameProvider: {
        prepareProvider: true,
      },
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

  await refreshSettings();

  await connection.client.register(DidChangeWatchedFilesNotification.type, {
    watchers: [
      { globPattern: "**/*.{cpy,CPY,cob,COB,cbl,CBL}" },
      { globPattern: "**/copybooks/**/*.{cpy,CPY,cob,COB,cbl,CBL}" },
      { globPattern: "**/COPYBOOKS/**/*.{cpy,CPY,cob,COB,cbl,CBL}" },
    ],
  });
});

connection.onDidChangeConfiguration(async () => {
  await refreshSettings();
  for (const d of documents.all()) scheduleValidate(d.uri, 0, true);
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

  // Clear diagnostics this doc published (only those not claimed by other docs)
  const closing = publishedUrisByDoc.get(uri);
  if (closing) {
    const claimedByOthers = new Set<string>();
    for (const [otherDoc, otherUris] of publishedUrisByDoc) {
      if (otherDoc === uri) continue;
      for (const u of otherUris) claimedByOthers.add(u);
    }
    for (const pubUri of closing) {
      if (!claimedByOthers.has(pubUri)) {
        connection.sendDiagnostics({ uri: pubUri, diagnostics: [] });
      }
    }
    publishedUrisByDoc.delete(uri);
  }
});

// ======================= Parser state management =======================

function disposeParserState(uri: string): void {
  const prev = parserStateByUri.get(uri);
  if (!prev) return;
  parserStateByUri.delete(uri);
  try { prev.tree.delete(); } catch { }
}

function disposeAllParserStates(): void {
  for (const uri of parserStateByUri.keys()) disposeParserState(uri);
}

// ======================= Document symbols =======================

connection.onDocumentSymbol((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return [];
  return buildDocumentSymbols(doc);
});

// ======================= Go-to-Definition =======================

connection.onDefinition((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return undefined;

  const docFsPath = fsPathFromUri(doc.uri);
  const docDir = docFsPath ? path.dirname(docFsPath) : undefined;
  const baseDirs: string[] = [];
  if (docDir) baseDirs.push(docDir);
  for (const wf of workspaceFolderPaths) {
    baseDirs.push(wf);
    for (const cp of currentSettings.copybookPaths) {
      baseDirs.push(path.isAbsolute(cp) ? cp : path.join(wf, cp));
    }
  }
  const uniqueBaseDirs = Array.from(new Set(baseDirs));

  // 1) Try COPY book definition
  const copyDef = findCopybookDefinition(doc, params.position, baseDirs);
  if (copyDef) return copyDef;

  // 2) Resolve symbol references against preprocessed text (includes COPY expansion)
  const lineText = doc.getText(Range.create(params.position.line, 0, params.position.line + 1, 0)).replace(/\r?\n$/, "");
  const wordInfo = wordAtPosition(lineText, params.position.character);
  if (!wordInfo) return undefined;

  const pre = preprocessUri({
    uri: doc.uri,
    text: doc.getText(),
    baseDirs: uniqueBaseDirs,
    diagsByUri: new Map<string, Diagnostic[]>(),
    includeStack: [],
    depth: 0,
    maxDepth: 20,
  });
  const preDoc = TextDocument.create(doc.uri, "cobol85", doc.version, pre.text);
  const preIndex = buildDefinitionIndex(preDoc);
  const target = findSymbolInIndex(wordInfo.word, preIndex);
  if (target) {
    const startOff = preDoc.offsetAt({ line: target.line, character: target.character });
    const endOff = preDoc.offsetAt({ line: target.line, character: target.endCharacter });
    const mapped = mapGenRange(pre, startOff, endOff);
    if (mapped) return mapped;
  }

  // 3) Fallback to current-document index only
  const index = buildDefinitionIndex(doc);
  const local = findSymbolInIndex(wordInfo.word, index);
  if (!local) return undefined;
  return {
    uri: doc.uri,
    range: Range.create(local.line, local.character, local.line, local.endCharacter),
  };
});

// ======================= Hover =======================

connection.onHover((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return undefined;

  const docFsPath = fsPathFromUri(doc.uri);
  const docDir = docFsPath ? path.dirname(docFsPath) : undefined;
  const baseDirs: string[] = [];
  if (docDir) baseDirs.push(docDir);
  for (const wf of workspaceFolderPaths) {
    baseDirs.push(wf);
    for (const cp of currentSettings.copybookPaths) {
      baseDirs.push(path.isAbsolute(cp) ? cp : path.join(wf, cp));
    }
  }
  const uniqueBaseDirs = Array.from(new Set(baseDirs));

  // Use COPY-expanded index so symbols from copybooks show hover info
  const pre = preprocessUri({
    uri: doc.uri,
    text: doc.getText(),
    baseDirs: uniqueBaseDirs,
    diagsByUri: new Map<string, Diagnostic[]>(),
    includeStack: [],
    depth: 0,
    maxDepth: 20,
  });
  const preDoc = TextDocument.create(doc.uri, "cobol85", doc.version, pre.text);
  const preIndex = buildDefinitionIndex(preDoc);

  return buildHover(doc, params.position, uniqueBaseDirs, preIndex);
});

// ======================= Completion =======================

connection.onCompletion((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return [];

  const docFsPath = fsPathFromUri(doc.uri);
  const docDir = docFsPath ? path.dirname(docFsPath) : undefined;
  const baseDirs: string[] = [];
  if (docDir) baseDirs.push(docDir);
  for (const wf of workspaceFolderPaths) {
    baseDirs.push(wf);
    for (const cp of currentSettings.copybookPaths) {
      baseDirs.push(path.isAbsolute(cp) ? cp : path.join(wf, cp));
    }
  }
  const uniqueBaseDirs = Array.from(new Set(baseDirs));

  // Use COPY-expanded index so Data-Names from copybooks are suggested, too.
  const pre = preprocessUri({
    uri: doc.uri,
    text: doc.getText(),
    baseDirs: uniqueBaseDirs,
    diagsByUri: new Map<string, Diagnostic[]>(),
    includeStack: [],
    depth: 0,
    maxDepth: 20,
  });
  const preDoc = TextDocument.create(doc.uri, "cobol85", doc.version, pre.text);
  const preIndex = buildDefinitionIndex(preDoc);
  const resolveSourceLocation = (line: number, character: number) => {
    const off = preDoc.offsetAt({ line, character });
    return mapGenPoint(pre, off);
  };

  return buildCompletionItems(doc, params.position, baseDirs, preIndex, resolveSourceLocation);
});

// ======================= References =======================

connection.onReferences((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return [];

  const docFsPath = fsPathFromUri(doc.uri);
  const docDir = docFsPath ? path.dirname(docFsPath) : undefined;
  const baseDirs: string[] = [];
  if (docDir) baseDirs.push(docDir);
  for (const wf of workspaceFolderPaths) {
    baseDirs.push(wf);
    for (const cp of currentSettings.copybookPaths) {
      baseDirs.push(path.isAbsolute(cp) ? cp : path.join(wf, cp));
    }
  }
  const uniqueBaseDirs = Array.from(new Set(baseDirs));

  const pre = preprocessUri({
    uri: doc.uri,
    text: doc.getText(),
    baseDirs: uniqueBaseDirs,
    diagsByUri: new Map<string, Diagnostic[]>(),
    includeStack: [],
    depth: 0,
    maxDepth: 20,
  });
  const preDoc = TextDocument.create(doc.uri, "cobol85", doc.version, pre.text);
  const preIndex = buildDefinitionIndex(preDoc);
  const mapToSource = (startOff: number, endOff: number) => mapGenRange(pre, startOff, endOff);

  return buildReferences(
    doc,
    params.position,
    params.context.includeDeclaration,
    preDoc,
    preIndex,
    mapToSource,
  );
});

// ======================= Semantic Tokens =======================

connection.languages.semanticTokens.on((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return { data: [] };

  const docFsPath = fsPathFromUri(doc.uri);
  const docDir = docFsPath ? path.dirname(docFsPath) : undefined;
  const baseDirs: string[] = [];
  if (docDir) baseDirs.push(docDir);
  for (const wf of workspaceFolderPaths) {
    baseDirs.push(wf);
    for (const cp of currentSettings.copybookPaths) {
      baseDirs.push(path.isAbsolute(cp) ? cp : path.join(wf, cp));
    }
  }
  const uniqueBaseDirs = Array.from(new Set(baseDirs));

  // Use COPY-expanded index so copybook-defined symbols are classified correctly
  const pre = preprocessUri({
    uri: doc.uri,
    text: doc.getText(),
    baseDirs: uniqueBaseDirs,
    diagsByUri: new Map<string, Diagnostic[]>(),
    includeStack: [],
    depth: 0,
    maxDepth: 20,
  });
  const preDoc = TextDocument.create(doc.uri, "cobol85", doc.version, pre.text);
  const preIndex = buildDefinitionIndex(preDoc);

  const tokens = buildSemanticTokens(doc, preIndex);
  const data = encodeSemanticTokens(tokens);

  return { data };
});

// ======================= Rename =======================

connection.onPrepareRename((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return undefined;

  const docFsPath = fsPathFromUri(doc.uri);
  const docDir = docFsPath ? path.dirname(docFsPath) : undefined;
  const baseDirs: string[] = [];
  if (docDir) baseDirs.push(docDir);
  for (const wf of workspaceFolderPaths) {
    baseDirs.push(wf);
    for (const cp of currentSettings.copybookPaths) {
      baseDirs.push(path.isAbsolute(cp) ? cp : path.join(wf, cp));
    }
  }
  const uniqueBaseDirs = Array.from(new Set(baseDirs));

  // Use preprocessed index so copybook-defined symbols are found
  const pre = preprocessUri({
    uri: doc.uri,
    text: doc.getText(),
    baseDirs: uniqueBaseDirs,
    diagsByUri: new Map<string, Diagnostic[]>(),
    includeStack: [],
    depth: 0,
    maxDepth: 20,
  });
  const preDoc = TextDocument.create(doc.uri, "cobol85", doc.version, pre.text);
  const preIndex = buildDefinitionIndex(preDoc);

  return prepareRename(doc, params.position, preIndex);
});

connection.onRenameRequest((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return undefined;

  const docFsPath = fsPathFromUri(doc.uri);
  const docDir = docFsPath ? path.dirname(docFsPath) : undefined;
  const baseDirs: string[] = [];
  if (docDir) baseDirs.push(docDir);
  for (const wf of workspaceFolderPaths) {
    baseDirs.push(wf);
    for (const cp of currentSettings.copybookPaths) {
      baseDirs.push(path.isAbsolute(cp) ? cp : path.join(wf, cp));
    }
  }
  const uniqueBaseDirs = Array.from(new Set(baseDirs));

  const pre = preprocessUri({
    uri: doc.uri,
    text: doc.getText(),
    baseDirs: uniqueBaseDirs,
    diagsByUri: new Map<string, Diagnostic[]>(),
    includeStack: [],
    depth: 0,
    maxDepth: 20,
  });
  const preDoc = TextDocument.create(doc.uri, "cobol85", doc.version, pre.text);
  const preIndex = buildDefinitionIndex(preDoc);
  const mapToSource = (startOff: number, endOff: number) => mapGenRange(pre, startOff, endOff);

  return performRename(doc, params.position, params.newName, preDoc, preIndex, mapToSource);
});

// ======================= Scheduling =======================

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

// ======================= Epoch tracking =======================

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

// ======================= Profiling =======================

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

// ======================= validateDocument =======================

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

  // base search dirs (document dir + workspace folders + configured copybookPaths)
  const baseDirs: string[] = [];
  if (docDir) baseDirs.push(docDir);
  for (const wf of workspaceFolderPaths) {
    baseDirs.push(wf);
    for (const cp of currentSettings.copybookPaths) {
      baseDirs.push(path.isAbsolute(cp) ? cp : path.join(wf, cp));
    }
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
              message: "Viele Syntaxfehler (Tree-sitter). Anzeige wurde begrenzt -- erster Fehler ist weiter unten markiert.",
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
    if (gd.code === "BLOCK_CLOSED_BY_PERIOD" && !currentSettings.warnings.blockClosedByPeriod) continue;

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

  // ----- Dead-code analysis (unreferenced paragraphs/sections)
  const preDocForDeadCode = TextDocument.create(doc.uri, "cobol85", doc.version, pre.text);
  const deadCodeDiags = lintDeadCode(preDocForDeadCode);
  for (const gd of deadCodeDiags) {
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

  // ----- Publish diagnostics per URI (per-document tracking)
  const newUris = new Set(diagsByUri.keys());
  if (isValidateStale(doc.uri, epoch)) return;

  const publishStart = performance.now();
  const prevUris = publishedUrisByDoc.get(doc.uri) ?? new Set<string>();

  // Collect all URIs still claimed by OTHER open documents
  const claimedByOthers = new Set<string>();
  for (const [otherDoc, otherUris] of publishedUrisByDoc) {
    if (otherDoc === doc.uri) continue;
    for (const u of otherUris) claimedByOthers.add(u);
  }

  // Clear URIs that this doc previously published but no longer does,
  // and that are not claimed by any other open document
  for (const pubUri of prevUris) {
    if (!newUris.has(pubUri) && !claimedByOthers.has(pubUri)) {
      connection.sendDiagnostics({ uri: pubUri, diagnostics: [] });
    }
  }

  publishDiagnostics(diagsByUri);
  profile.publishMs += performance.now() - publishStart;

  publishedUrisByDoc.set(doc.uri, newUris);
  profile.totalMs = performance.now() - totalStart;
  maybeLogValidateProfile(
    doc.uri,
    profile,
    countDiags(diagsByUri),
    runParser ? parserNote : "parser-skipped-typing"
  );
}

// ======================= Publish =======================

function publishDiagnostics(diagsByUri: Map<string, Diagnostic[]>) {
  for (const [uri, diagnostics] of diagsByUri.entries()) {
    connection.sendDiagnostics({ uri, diagnostics });
  }
}

// ======================= Settings =======================

function coerceSettings(raw: any): Cobol85Settings {
  const fromNested = raw?.warnings?.blockClosedByPeriod;
  const fromFlat = raw?.["warnings.blockClosedByPeriod"];
  const blockClosedByPeriod =
    typeof fromNested === "boolean" ? fromNested :
      typeof fromFlat === "boolean" ? fromFlat :
        DEFAULT_SETTINGS.warnings.blockClosedByPeriod;

  let copybookPaths = raw?.copybookPaths;
  if (!Array.isArray(copybookPaths) || !copybookPaths.every((p: unknown) => typeof p === "string")) {
    copybookPaths = DEFAULT_SETTINGS.copybookPaths;
  }

  return {
    warnings: {
      blockClosedByPeriod,
    },
    copybookPaths,
  };
}

async function refreshSettings(): Promise<void> {
  if (!hasConfigurationCapability) {
    currentSettings = DEFAULT_SETTINGS;
    return;
  }

  try {
    const raw = await connection.workspace.getConfiguration("cobol85");
    currentSettings = coerceSettings(raw);
  } catch {
    currentSettings = DEFAULT_SETTINGS;
  }
}

// ======================= Text helpers (need `documents`) =======================

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

// ======================= Start =======================

documents.listen(connection);
connection.listen();
