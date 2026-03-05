#!/usr/bin/env bash
# package-vsix.sh — Build VSIX with clean dependency tree.
#
# Problem: vseconnector-ts uses a file: link to ../vseconnector which causes
# npm list --production to fail (broken transitive deps in the linked project).
# vsce aborts when npm list returns non-zero, so no node_modules get packaged.
#
# Solution: compile first (while the link exists), then temporarily remove the
# link and the dependency from package.json so vsce can run npm list cleanly.
set -uo pipefail
cd "$(dirname "$0")/.."

cleanup() {
  echo "==> Restoring package.json and symlink..."
  if [[ -f package.json.bak ]]; then
    mv package.json.bak package.json
  fi
  ln -sf ../../vseconnector node_modules/vseconnector-ts
}
trap cleanup EXIT

echo "==> Compiling (with vseconnector-ts available)..."
npm run compile

echo "==> Preparing package.json for packaging..."
cp package.json package.json.bak

# Remove vseconnector-ts from dependencies and neutralize prepublish
node -e "
  const pkg = require('./package.json');
  delete pkg.dependencies['vseconnector-ts'];
  pkg.scripts['vscode:prepublish'] = 'echo skipped — already compiled';
  require('fs').writeFileSync('package.json', JSON.stringify(pkg, null, 2) + '\n');
"

# Remove the symlink so npm list won't see it
rm -f node_modules/vseconnector-ts

echo "==> Packaging VSIX..."
npx @vscode/vsce package --no-update-package-json "$@"

BUILT_VSIX="$(ls -t *.vsix 2>/dev/null | head -1)"
echo "==> Built: $BUILT_VSIX"
ls -la "$BUILT_VSIX"

echo ""
echo "==> Running VSIX verification..."
bash scripts/verify-vsix.sh "$BUILT_VSIX"
