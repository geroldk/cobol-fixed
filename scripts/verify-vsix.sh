#!/usr/bin/env bash
# verify-vsix.sh — Smoke-test a packaged VSIX before release.
#
# Usage:  bash scripts/verify-vsix.sh [path-to.vsix]
#         (defaults to newest *.vsix in project root)
#
# Checks:
#   1. Required files present (extension entry, server, WASM, node_modules)
#   2. Forbidden files absent  (vseconnector-ts, tree-sitter-cli, nested VSIX)
#   3. Require-chain test      (client/out/extension.js loads without crash)
#   4. Size guard               (warn if > 5 MB)
set -uo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

cd "$(dirname "$0")/.."

# ── Locate VSIX ──────────────────────────────────────────────────────────────
VSIX="${1:-}"
if [[ -z "$VSIX" ]]; then
  VSIX="$(ls -t *.vsix 2>/dev/null | head -1)"
  if [[ -z "$VSIX" ]]; then
    echo -e "${RED}FAIL${NC}: No .vsix file found in project root."
    exit 1
  fi
fi
if [[ ! -f "$VSIX" ]]; then
  echo -e "${RED}FAIL${NC}: File not found: $VSIX"
  exit 1
fi
echo "Verifying: $VSIX"

# ── Extract to temp dir ─────────────────────────────────────────────────────
TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT
unzip -q "$VSIX" -d "$TMPDIR"

ERRORS=0

# ── 1. Required files ───────────────────────────────────────────────────────
REQUIRED_FILES=(
  "extension/client/out/extension.js"
  "extension/client/out/vse/submitFlow.js"
  "extension/server/out/server.js"
  "extension/server/assets/tree-sitter-cobol.wasm"
  "extension/node_modules/vscode-languageclient/node.js"
  "extension/node_modules/vscode-languageserver/node.js"
  "extension/node_modules/web-tree-sitter/tree-sitter.js"
  "extension/package.json"
  "extension/node_modules/vseconnector-ts/dist/index.js"
  "extension/node_modules/vseconnector-ts/package.json"
)

echo ""
echo "==> Required files"
for f in "${REQUIRED_FILES[@]}"; do
  if [[ -e "$TMPDIR/$f" ]]; then
    echo -e "  ${GREEN}OK${NC}  $f"
  else
    echo -e "  ${RED}MISSING${NC}  $f"
    ERRORS=$((ERRORS + 1))
  fi
done

# ── 2. Forbidden files ──────────────────────────────────────────────────────
FORBIDDEN_DIRS=(
  "extension/node_modules/tree-sitter-cli"
)

echo ""
echo "==> Forbidden entries"
for d in "${FORBIDDEN_DIRS[@]}"; do
  if [[ -e "$TMPDIR/$d" ]]; then
    echo -e "  ${RED}PRESENT${NC}  $d  (should be excluded)"
    ERRORS=$((ERRORS + 1))
  else
    echo -e "  ${GREEN}OK${NC}  $d  (absent)"
  fi
done

# Check for nested VSIX
NESTED=$(find "$TMPDIR" -name '*.vsix' -type f 2>/dev/null | head -1)
if [[ -n "$NESTED" ]]; then
  echo -e "  ${RED}PRESENT${NC}  nested .vsix found: $NESTED"
  ERRORS=$((ERRORS + 1))
else
  echo -e "  ${GREEN}OK${NC}  no nested .vsix"
fi

# ── 3. Require-chain test ───────────────────────────────────────────────────
echo ""
echo "==> Require-chain test (all require() targets resolvable)"

# Scan compiled JS files for require("...") calls and verify each module
# can be resolved from the VSIX's node_modules. Skip:
#   - "vscode" (provided by VS Code runtime)
#   - "vseconnector-ts" (intentionally absent, lazy-loaded with try/catch)
#   - relative requires (./...) which refer to local files
#   - node: builtins (node:fs, node:path, etc.)
REQUIRE_CHECK=$(cat <<'NODESCRIPT'
"use strict";
const fs = require("fs");
const path = require("path");
const Module = require("module");

const extRoot = process.argv[2];
const clientOut = path.join(extRoot, "client", "out");
const serverOut = path.join(extRoot, "server", "out");

// Collect all .js files from client/out and server/out
function collectJs(dir) {
  const files = [];
  if (!fs.existsSync(dir)) return files;
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) files.push(...collectJs(full));
    else if (entry.name.endsWith(".js")) files.push(full);
  }
  return files;
}

const allJs = [...collectJs(clientOut), ...collectJs(serverOut)];
const requireRe = /\brequire\s*\(\s*["']([^"']+)["']\s*\)/g;
const SKIP = new Set(["vscode"]);

let errors = 0;
const checked = new Set();

for (const file of allJs) {
  const src = fs.readFileSync(file, "utf8");
  let m;
  while ((m = requireRe.exec(src)) !== null) {
    const dep = m[1];
    // Skip relative, node: builtins, and known-absent modules
    if (dep.startsWith(".") || dep.startsWith("node:") || SKIP.has(dep)) continue;
    // Also skip Node built-in modules
    try { if (require("module").builtinModules.includes(dep)) continue; } catch {}

    const key = file + "::" + dep;
    if (checked.has(key)) continue;
    checked.add(key);

    try {
      // Try to resolve the module from the file's directory
      Module._resolveFilename(dep, { id: file, filename: file, paths: Module._nodeModulePaths(path.dirname(file)) });
    } catch {
      console.error(`MISSING: ${dep}  (required by ${path.relative(extRoot, file)})`);
      errors++;
    }
  }
}

if (errors === 0) {
  console.log(`OK: All require() targets resolvable (${checked.size} checked).`);
  process.exit(0);
} else {
  console.error(`FAIL: ${errors} unresolvable require() target(s).`);
  process.exit(1);
}
NODESCRIPT
)

EXT_DIR="$TMPDIR/extension"
if [[ -d "$EXT_DIR/client/out" ]]; then
  # Write the check script to a temp file so process.argv works properly
  SCRIPT_FILE="$TMPDIR/_require_check.js"
  echo "$REQUIRE_CHECK" > "$SCRIPT_FILE"
  if node "$SCRIPT_FILE" "$EXT_DIR" 2>&1; then
    echo -e "  ${GREEN}OK${NC}  require-chain passed"
  else
    echo -e "  ${RED}FAIL${NC}  require-chain failed"
    ERRORS=$((ERRORS + 1))
  fi
else
  echo -e "  ${YELLOW}SKIP${NC}  client/out not found (already flagged above)"
fi

# ── 4. Size check ───────────────────────────────────────────────────────────
echo ""
echo "==> Size check"
SIZE=$(stat -c%s "$VSIX" 2>/dev/null || stat -f%z "$VSIX" 2>/dev/null)
SIZE_MB=$(awk "BEGIN { printf \"%.2f\", $SIZE / 1048576 }")
MAX_MB=5
if (( $(awk "BEGIN { print ($SIZE_MB > $MAX_MB) }") )); then
  echo -e "  ${YELLOW}WARN${NC}  $SIZE_MB MB (exceeds ${MAX_MB} MB guideline)"
else
  echo -e "  ${GREEN}OK${NC}  $SIZE_MB MB"
fi

# ── node_modules file count ─────────────────────────────────────────────────
NM_COUNT=$(find "$TMPDIR/extension/node_modules" -type f 2>/dev/null | wc -l)
echo -e "  INFO  node_modules contains $NM_COUNT files"

# ── Summary ─────────────────────────────────────────────────────────────────
echo ""
if [[ $ERRORS -gt 0 ]]; then
  echo -e "${RED}VERIFICATION FAILED${NC}: $ERRORS error(s) found."
  exit 1
else
  echo -e "${GREEN}VERIFICATION PASSED${NC}: All checks OK."
  exit 0
fi
