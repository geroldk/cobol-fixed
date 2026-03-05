#!/usr/bin/env bash
# package-vsix.sh — Build VSIX with clean dependency tree.
#
# Problem: vseconnector-ts uses a file: link to ../vseconnector which causes
# npm list --production to fail (broken transitive deps in the linked project).
# vsce aborts when npm list returns non-zero, so no node_modules get packaged.
#
# Solution: compile first (while the link exists), then replace the symlink with
# a self-contained copy of the built dist/ files so vsce packages them.
set -uo pipefail
cd "$(dirname "$0")/.."

VSE_SRC="$(readlink -f node_modules/vseconnector-ts 2>/dev/null || echo ../vseconnector)"

cleanup() {
  echo "==> Restoring package.json and symlink..."
  if [[ -f package.json.bak ]]; then
    mv package.json.bak package.json
  fi
  rm -rf node_modules/vseconnector-ts
  ln -sf ../../vseconnector node_modules/vseconnector-ts
}
trap cleanup EXIT

echo "==> Compiling (with vseconnector-ts available)..."
npm run compile

echo "==> Preparing package.json for packaging..."
cp package.json package.json.bak

# Remove the file: link from dependencies (keeps npm list happy)
# but keep the actual files in node_modules so vsce can bundle them.
node -e "
  const pkg = require('./package.json');
  delete pkg.dependencies['vseconnector-ts'];
  pkg.scripts['vscode:prepublish'] = 'echo skipped — already compiled';
  require('fs').writeFileSync('package.json', JSON.stringify(pkg, null, 2) + '\n');
"

# Replace symlink with a self-contained copy (dist/ only, no devDeps)
rm -f node_modules/vseconnector-ts
mkdir -p node_modules/vseconnector-ts
cp -r "$VSE_SRC/dist" node_modules/vseconnector-ts/
# Minimal package.json — no dependencies (commander is CLI-only, not needed)
node -e "
  const src = require('$VSE_SRC/package.json');
  const minimal = {
    name: src.name,
    version: src.version,
    type: src.type,
    main: src.main,
    types: src.types
  };
  require('fs').writeFileSync(
    'node_modules/vseconnector-ts/package.json',
    JSON.stringify(minimal, null, 2) + '\n'
  );
"
echo "==> Bundled vseconnector-ts dist/ ($(du -sh node_modules/vseconnector-ts/dist | cut -f1))"

echo "==> Packaging VSIX..."
npx @vscode/vsce package --no-update-package-json "$@"

BUILT_VSIX="$(ls -t *.vsix 2>/dev/null | head -1)"
echo "==> Built: $BUILT_VSIX"
ls -la "$BUILT_VSIX"

echo ""
echo "==> Running VSIX verification..."
bash scripts/verify-vsix.sh "$BUILT_VSIX"
