# Copilot Instructions — cobol-fixed

## Architecture

VS Code extension for COBOL 85 fixed-format. Language ID: `cobol85`. Two main components:

- **Client** (`client/src/extension.ts`): Thin LSP client + column-7 comment toggle command.
- **Language Server** (`server/src/server.ts`): LSP orchestration delegating to focused modules:
  - `lint.ts` — diagnostics (fixed-format checks, EXEC DLI/CICS/SQL validation, undefined identifiers, verb typo suggestions)
  - `definition.ts` — go-to-definition + `DefinitionIndex` (paragraphs, sections, data items)
  - `completion.ts` — context-aware completions (division-aware: DATA vs PROCEDURE)
  - `semantic-tokens.ts` — token classification (keywords, variables, CICS verbs/options/conditions)
  - `hover.ts`, `references.ts`, `rename.ts`, `symbols.ts`, `dead-code.ts`
  - `preprocessor.ts` — COPY expansion with REPLACING, segment offset mapping (`GenDiag` → source positions)
  - `normalizer.ts` — text normalization before tree-sitter parsing (ID DIVISION alias, compiler directives)
  - `parser-bridge.ts` — tree-sitter WASM bridge (UTF-8/UTF-16 offset conversion)
  - `types.ts` — shared types (`LineSlice`, `GenDiag`, `LintTok`, `Segment`, `PreprocessedDoc`)
  - `utils.ts` — fixed-format helpers, file I/O, caching

- **TextMate Grammar** (`cobol85.tmLanguage.json`): Active grammar (scope `source.cobol85`). Per-line structure: `fixedCommentLine` → `fixedNormalLine` → `languageAreaNormal` → `cobolTokens`. **Note:** `cobol_fixed.tmLanguage.json` exists but is NOT used (old `source.cobol.fixed` scope).

## Fixed-Format Column Layout

All code assumes COBOL reference format. Column conventions (0-indexed in code):

| Columns | Index | Purpose | Code pattern |
|---------|-------|---------|--------------|
| 1–6 | `[0..6)` | Sequence area | `line.slice(0, 6)` — ignored |
| 7 | `[6]` | Indicator | `*`/`/`=comment, `D`/`d`=debug, `-`=continuation, ` `=code |
| 8–72 | `[7..72)` | Language area | `langStart = 7; langEnd = Math.min(len, 72)` |
| 73–80 | `[72..80)` | ID area | Ignored metadata |
| 81+ | `[80..)` | Overflow | `invalid.illegal` |

Key helper: `hasFixedColumns(line)` → `line.length >= 7`. Extract code: `lang = full.slice(7, Math.min(full.length, 72))`.

## Build & Test

```bash
npm install              # install dependencies
npm run compile          # build client + server (tsc -p client/ && tsc -p server/)
npx vitest run           # run all tests (~405 tests across 13 files)
npx vitest run server/src/lint.test.ts  # run single test file
npm run build:cobol-wasm # rebuild tree-sitter WASM (requires Docker)
```

Press `F5` in VS Code to launch Extension Development Host for debugging.

## Test Conventions

Tests use **Vitest** with co-located `*.test.ts` files in `server/src/`. Every test file uses these helpers:

```typescript
function makeDoc(lines: string[]): TextDocument {
  return TextDocument.create("file:///test.cob", "cobol85", 1, lines.join("\n"));
}
function fixedLine(lang: string, indicator: string = " "): string {
  return "000000" + indicator + lang.padEnd(65, " "); // produces 72-char fixed-format line
}
```

- Build documents as arrays of `fixedLine(...)` calls with proper COBOL structure (ID DIVISION → DATA DIVISION → PROCEDURE DIVISION)
- Lint tests operate on preprocessed text (plain strings, not fixed-format wrapped)
- Each test file duplicates helpers locally — no shared test-utils module
- Assert diagnostics by checking `code` field (e.g., `"TAB_FIXED_FORMAT"`, `"EXEC_CICS_UNKNOWN_COMMAND"`)

## Key Conventions

- **Diagnostic messages are in German** — all `message:` strings use German text (e.g., "Unbekannter CICS-Befehl", "Zeile ueberschreitet 80 Spalten"). Diagnostic `code` fields are English SCREAMING_SNAKE_CASE.
- **EXEC CICS handling**: Lint validates commands via `CICS_COMMAND_DEFS` map (verb → allowed/required options with synonyms). Semantic tokens classify verbs (`function`), options (`property`), conditions (`type`), and data names in parentheses (`variable`). Paren depth tracked across lines.
- **EXEC DLI handling**: DLI verbs (GU, GN, ISRT, DLET, etc.) validated against `DLI_VERBS` set. USING targets checked against index.
- **Preprocessor pipeline**: Source → `normalizeForCobol85Parser()` → `preprocessUri()` (COPY expansion) → tree-sitter parse → `lintPreprocessed()`. Offset mapping via `Segment[]` and `mapGenRange()`.
- **Two grammar files exist**: Only `cobol85.tmLanguage.json` is active (registered in `package.json`). `cobol_fixed.tmLanguage.json` is legacy/unused.
- **Semantic tokens override TextMate** for EXEC CICS blocks — the `emitCicsTokens()` function handles all classification inside `EXEC CICS ... END-EXEC`, tracking `inExecCics` state and `cicsParenDepth` across lines.

## Adding a New EXEC CICS Command

1. Add command definition to `CICS_COMMAND_DEFS` in `lint.ts` (allowed options, required options, synonyms)
2. Add the verb to `CICS_VERBS` set in `semantic-tokens.ts` and any new options to `CICS_OPTIONS`
3. Add lint tests in `lint.test.ts` under `describe("lintPreprocessed")` — test valid usage + missing required options + unknown options
4. Run `npx vitest run` to verify

## Specification 
### DLI
- `doc/DLI_CALL_EXEC_SYNTAX_SPEC.md`
### CICS
- `doc/EXEC_CICS_STATEMENTS.md`
- `doc/EXEC_CICS_SYNTAX_SPEC.md`
