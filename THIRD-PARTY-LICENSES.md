# Third-Party Licenses

This document lists all direct and transitive dependencies of **cobol-fixed**, along with
their respective licenses and the obligations that arise when distributing the extension.

---

## TL;DR – License Obligations at a Glance

| Scenario | Obligation |
|---|---|
| Use the extension privately | None |
| Distribute / publish the extension (.vsix) | Include copyright notices of all runtime dependencies (MIT / ISC) |
| Modify and redistribute | Keep original copyright notices; document your changes |
| TypeScript (build-only) | No distribution obligation – TypeScript itself is not shipped |

All runtime dependencies are under **MIT** or **ISC** licenses.  Both are permissive:
they allow free use, modification, and distribution as long as the original copyright
notice and permission text are preserved.  No copyleft (GPL-like) obligations apply.

---

## Runtime Dependencies (shipped with the extension)

### vscode-languageclient · v9.0.1 · MIT

- Repository: <https://github.com/Microsoft/vscode-languageserver-node>
- Copyright: © Microsoft Corporation

### vscode-languageserver · v9.0.1 · MIT

- Repository: <https://github.com/Microsoft/vscode-languageserver-node>
- Copyright: © Microsoft Corporation

### vscode-languageserver-textdocument · v1.0.12 · MIT

- Repository: <https://github.com/Microsoft/vscode-languageserver-node>
- Copyright: © Microsoft Corporation

### vscode-languageserver-protocol · v3.17.5 · MIT

- Repository: <https://github.com/Microsoft/vscode-languageserver-node>
- Copyright: © Microsoft Corporation

### vscode-languageserver-types · v3.17.5 · MIT

- Repository: <https://github.com/Microsoft/vscode-languageserver-node>
- Copyright: © Microsoft Corporation

### vscode-jsonrpc · v8.2.0 · MIT

- Repository: <https://github.com/Microsoft/vscode-languageserver-node>
- Copyright: © Microsoft Corporation

### web-tree-sitter · v0.25.10 · MIT

- Repository: <https://github.com/tree-sitter/tree-sitter>
- Copyright: © 2018–2024 Max Brunsfeld

### balanced-match · v1.0.2 · MIT

- Repository: <https://github.com/juliangruber/balanced-match>
- Copyright: © 2013 Julian Gruber

### brace-expansion · v2.0.2 · MIT

- Repository: <https://github.com/juliangruber/brace-expansion>
- Copyright: © 2013 Julian Gruber

### minimatch · v5.1.7 · ISC

- Repository: <https://github.com/isaacs/minimatch>
- Copyright: © 2011–2023 Isaac Z. Schlueter and Contributors

### semver · v7.7.4 · ISC

- Repository: <https://github.com/npm/node-semver>
- Copyright: © Isaac Z. Schlueter and Contributors

---

## Bundled Binary Asset

### server/assets/tree-sitter-cobol.wasm

This WebAssembly file is compiled from the
[`vendor/tree-sitter-cobol`](https://github.com/geroldk/tree-sitter-cobol) grammar (a
submodule of this repository) using the **tree-sitter** toolchain.

- The grammar sources belong to this project and are released under the same license as
  **cobol-fixed** itself.
- The `web-tree-sitter` runtime (MIT, see above) is embedded in the WASM binary.

---

## Development-Only Dependencies (not shipped)

The following packages are used only during the build process.  They are **not** bundled
into the published extension and therefore impose **no distribution obligations**.

| Package | Version | License | Used for |
|---|---|---|---|
| typescript | 5.9.3 | Apache-2.0 | TypeScript compiler |
| tree-sitter-cli | 0.25.10 | MIT | Building the COBOL WASM grammar |
| @types/node | 25.2.3 | MIT | Node.js type declarations |
| @types/vscode | 1.109.0 | MIT | VS Code API type declarations |

> **Note on Apache-2.0 (TypeScript):** The Apache 2.0 license requires that the copyright
> notice and a copy of the license accompany any **distribution** of covered software.
> Because TypeScript itself is never part of the distributed extension artefact (only the
> compiled JavaScript output is), this obligation does not apply to consumers of
> **cobol-fixed**.  Developers who redistribute a **modified build toolchain** that
> includes TypeScript sources would need to comply with Apache-2.0.

---

## MIT License Text

The following applies to all packages listed under the MIT license above:

```
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## ISC License Text

The following applies to all packages listed under the ISC license above:

```
Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
```
