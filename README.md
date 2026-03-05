# COBOL Fixed Format (VS Code Extension)

COBOL Fixed Format is a VS Code extension for COBOL 85 fixed-format workflows.
Current package version: `0.0.10`.

It combines TextMate highlighting, a Tree-sitter based language server, and fixed-format editing helpers.

## Highlights

- Language id: `cobol85`
- File extensions: `.cob`, `.cbl`, `.cpy`
- Fixed-format command: `COBOL 85: Toggle Fixed-Format Comment (Column 7)` (`Ctrl+/`)
- VSE submit shortcut: `F5` (when `editorLangId == cobol85`)
- Tree-sitter parser backend: `server/assets/tree-sitter-cobol.wasm`
- Runtime workaround for VS Code `< 1.107.0` to avoid V8 WASM `Zone` out-of-memory crashes
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

## VSE Compile Submit (`.conf` Driven)

The extension now supports VSE compile submits based on member-local `.conf` files.

Commands:

- `COBOL 85: Submit Compile Job (VSE)` (`cobol85.vseSubmitCompileJob`)
- `COBOL 85: Create Member .conf (VSE)` (`cobol85.vseCreateMemberConf`)
- `COBOL 85: Set VSE Password` (`cobol85.vseSetPassword`)
- `COBOL 85: Clear VSE Password` (`cobol85.vseClearPassword`)
- Shortcut: `F5` submits VSE compile jobs in COBOL editors.

Workflow summary:

1. Open a COBOL member file (for example `DBGMIF6.cbl`).
2. Submit command resolves `<member>.conf` in the same folder.
3. If missing and `cobol85.vse.conf.autoCreateOnMissing=true`, a wizard creates a full `.conf` (new types only: `2/3/4`).
4. Choose build mode `T` or `P` per submit.
5. Job is assembled from `tjcl/pjcl`, `toptions/poptions`, `txopts/pxopts`, source content, and placeholder settings.
6. If `cobol85.vse.submit.previewBeforeSubmit=true`, an expanded job preview is shown before submit.
7. Submit runs via `vseconnector-ts` and opens combined LST output.

Supported existing member types in read mode: `0/1/2/3/4`.
New `.conf` creation offers only `2/3/4`.

### `.conf` Keys Used

Required keys under `[General]`:

- `source`, `phase`, `type`
- `tjcl`, `pjcl`
- `toptions`, `poptions`
- `txopts`, `pxopts`

`tjcl`/`pjcl` are stored quoted with escaped newlines (`\n`), compatible with existing host files.

### VSE Settings

```json
{
  "cobol85.vse.host": "vse-host",
  "cobol85.vse.port": 2893,
  "cobol85.vse.user": "USER1",
  "cobol85.vse.charset": "utf8",
  "cobol85.vse.submit.executionMode": "blocking",
  "cobol85.vse.submit.timeoutSec": 120,
  "cobol85.vse.submit.previewBeforeSubmit": true,
  "cobol85.vse.conf.autoCreateOnMissing": true,
  "cobol85.vse.conf.previewBeforeCreate": true,
  "cobol85.vse.placeholders.catalogTest": "USRWMT.TEST",
  "cobol85.vse.placeholders.catalogProd": "USRWMT.PROD",
  "cobol85.vse.placeholders.id": "MYID",
  "cobol85.vse.placeholders.lnkstep": "// EXEC LNKEDT",
  "cobol85.vse.output.baseDir": ".vse/out"
}
```

Password is not stored in settings. It is stored in VS Code SecretStorage via `Set VSE Password`.

## Requirements

- VS Code `^1.104.0`
- Compatibility note: for VS Code `1.98` to `1.106`, the language server starts with conservative V8 WASM flags; behavior on `>= 1.107.0` is unchanged.
- Node.js + npm (current LTS recommended)
- Docker (only needed for rebuilding the COBOL Tree-sitter WASM)

## Installation

### Install from VSIX

If you already have `cobol-fixed-0.0.8.vsix`:

1. Open VS Code `Extensions`.
2. Open the `...` menu.
3. Choose `Install from VSIX...`.
4. Select `cobol-fixed-0.0.8.vsix`.

CLI alternative:

```bash
code --install-extension cobol-fixed-0.0.8.vsix
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
