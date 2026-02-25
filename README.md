# COBOL Fixed Format (VS Code Extension)

COBOL Fixed Format is a Visual Studio Code extension focused on COBOL 85 fixed-format workflows.
Current version: `0.0.2`.

It combines:
- COBOL 85 syntax highlighting (TextMate grammar)
- A language server for diagnostics (Tree-sitter based)
- A fixed-format helper command to toggle column-7 comments

## Features

- Language id: `cobol85`
- File extensions: `.cob`, `.cbl`, `.cpy`
- Command: `COBOL 85: Toggle Fixed-Format Comment (Column 7)`
- Default keybinding: `Ctrl+/` (when `editorLangId == cobol85`)
- Parser backend: Tree-sitter COBOL WASM (`server/assets/tree-sitter-cobol.wasm`)

## Settings

- `cobol85.warnings.blockClosedByPeriod` (boolean, default: `true`)
  Controls warning `BLOCK_CLOSED_BY_PERIOD` for blocks closed by `.` instead of `END-IF`/`END-EVALUATE`.

Example (`settings.json`):

```json
{
  "cobol85.warnings.blockClosedByPeriod": false
}
```

## Requirements

- VS Code `^1.109.0`
- Node.js and npm (recommended: current LTS)
- Docker (only needed when rebuilding the COBOL Tree-sitter WASM)

## Installation

### Install from VSIX (recommended for users)

If you already have `cobol-fixed-0.0.2.vsix`:

1. In VS Code, open `Extensions` view.
2. Click `...` (top-right menu) -> `Install from VSIX...`.
3. Select `cobol-fixed-0.0.2.vsix`.

CLI alternative:

```bash
code --install-extension cobol-fixed-0.0.2.vsix
```

### Install from source (development)

1. Clone the repository with submodules:

```bash
git clone --recurse-submodules <your-repo-url>
cd cobol-fixed-0.0.2
```

If already cloned without submodules:

```bash
git submodule update --init --recursive
```

2. Install dependencies:

```bash
npm install
```

3. Build client and server:

```bash
npm run compile
```

4. Optional: rebuild COBOL parser WASM (needed after grammar changes):

```bash
npm run build:cobol-wasm
```

5. Start extension debugging in VS Code:
- Open this folder in VS Code
- Press `F5` to launch an Extension Development Host

## Development Notes

- Main extension entry: `client/src/extension.ts`
- Language server: `server/src/server.ts`
- Tree-sitter grammar source: `vendor/tree-sitter-cobol/grammar.js`
- TextMate grammar: `cobol85.tmLanguage.json`

When changing Tree-sitter grammar:

1. Update `vendor/tree-sitter-cobol/grammar.js`
2. Regenerate grammar files:

```bash
cd vendor/tree-sitter-cobol
npx tree-sitter generate
```

3. Rebuild WASM:

```bash
cd ../..
npm run build:cobol-wasm
```

## License

MIT
