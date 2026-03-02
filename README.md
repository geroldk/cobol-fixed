# COBOL Fixed Format (VS Code Extension)

COBOL Fixed Format is a VS Code extension for COBOL 85 fixed-format workflows.
Current package version: `0.0.6`.

It combines TextMate highlighting, a Tree-sitter based language server, and fixed-format editing helpers.

## Highlights

- Language id: `cobol85`
- File extensions: `.cob`, `.cbl`, `.cpy`
- Fixed-format command: `COBOL 85: Toggle Fixed-Format Comment (Column 7)` (`Ctrl+/`)
- Tree-sitter parser backend: `server/assets/tree-sitter-cobol.wasm`
- Copybook-aware analysis pipeline (COPY expansion + source mapping)
- Context-aware support for `EXEC DLI` and `EXEC CICS` in:
  - diagnostics
  - completion
  - semantic tokens
  - hover behavior
- LSP features:
  - document symbols
  - go to definition (including COPY books)
  - hover
  - references
  - rename (with qualifier-aware resolution)
  - semantic tokens

## Diagnostics and Analysis

The server runs a multi-stage validation pipeline:

1. fixed-format baseline checks
2. COPY/REPLACE preprocessing
3. parser normalization
4. Tree-sitter parse + syntax diagnostics
5. lint diagnostics
6. dead-code analysis (unreferenced paragraphs/sections)
7. undefined-identifier diagnostics

Implemented linting covers, among others:

- COBOL procedure verb typo detection
- Data Division structure checks
- `EXEC DLI` command/clause validation
- `EXEC CICS` command/option/condition validation
- optional warning `BLOCK_CLOSED_BY_PERIOD`

## Settings

### `cobol85.warnings.blockClosedByPeriod`

- Type: `boolean`
- Default: `true`
- Controls warning `BLOCK_CLOSED_BY_PERIOD` when IF/EVALUATE blocks are closed by `.` instead of `END-IF` / `END-EVALUATE`.

### `cobol85.copybookPaths`

- Type: `string[]`
- Default: `["copybooks", "COPYBOOKS"]`
- Additional relative or absolute directories used for COPY book resolution.
- Relative paths are resolved from each workspace folder.

Example `settings.json`:

```json
{
  "cobol85.warnings.blockClosedByPeriod": false,
  "cobol85.copybookPaths": [
    "copybooks",
    "COPYBOOKS",
    "C:/host/copylib"
  ]
}
```

## Requirements

- VS Code `^1.104.0`
- Node.js + npm (current LTS recommended)
- Docker (only needed for rebuilding the COBOL Tree-sitter WASM)

## Installation

### Install from VSIX

If you already have `cobol-fixed-0.0.6.vsix`:

1. Open VS Code `Extensions`.
2. Open the `...` menu.
3. Choose `Install from VSIX...`.
4. Select `cobol-fixed-0.0.6.vsix`.

CLI alternative:

```bash
code --install-extension cobol-fixed-0.0.6.vsix
```

### Install from source

1. Clone with submodules:

```bash
git clone --recurse-submodules <your-repo-url>
cd cobol-fixed
```

If already cloned without submodules:

```bash
git submodule update --init --recursive
```

2. Install dependencies:

```bash
npm install
```

3. Build:

```bash
npm run compile
```

4. Run tests:

```bash
npm run test
```

5. Optional: rebuild COBOL parser WASM (after grammar changes):

```bash
npm run build:cobol-wasm
```

6. Start extension debugging in VS Code:
- Open this folder in VS Code
- Press `F5` to launch an Extension Development Host

## Developer Workflow

Useful scripts:

- `npm run compile` - build client + server
- `npm run test` - run Vitest suite
- `npm run test:watch` - watch mode tests
- `npm run build:cobol-wasm` - regenerate parser WASM
- `npm run changelog:update` - refresh auto snapshot in `CHANGELOG.md`

Key code locations:

- Client entry: `client/src/extension.ts`
- Server entry/orchestration: `server/src/server.ts`
- Core modules: `server/src/*.ts`
- TextMate grammars:
  - `cobol85.tmLanguage.json`
  - `cobol_fixed.tmLanguage.json`
- Tree-sitter grammar source: `vendor/tree-sitter-cobol/grammar.js`

## Documentation

- [ANALYSIS.md](./ANALYSIS.md) - technical architecture and verification status
- [CHANGELOG.md](./CHANGELOG.md) - canonical changelog
- [CHANGLOG.md](./CHANGLOG.md) - spelling alias forwarding to `CHANGELOG.md`
- `docs/EXEC_CICS_SYNTAX_SPEC.md` - CICS syntax reference used for lint/completion rules
- `docs/DLI_CALL_EXEC_SYNTAX_SPEC.md` - DLI syntax reference used for lint/completion rules

## License

MIT
