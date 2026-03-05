## Plan: v0.0.13 — Error message, CBL format, Catalog split, Tests

**TL;DR**: Five changes for v0.0.13: (1) Fix error message text for code 196611, (2) reformat COMPILEROPTIONS.opt with ` CBL ` prefix and 72-char wrapping, (3) split `#CATALOG#` into 4 settings (CICS/Batch × Test/Prod) with defaults, (4) comprehensive tests for all new logic, (5) release. The skeleton files bake in `.BATCH`/`.CICS` suffixes today — the new approach moves the full sublib name into the settings and removes the suffix from skeletons.

**Steps**

1. **Error message** — In [submitFlow.ts](client/src/vse/submitFlow.ts#L145), change:
   ```
   196611: "Eintrag konnte nicht erstellt werden (POWER-Queue voll oder Ressource belegt)"
   ```
   to:
   ```
   196611: "Eintrag konnte in Reader Queue nicht erstellt werden"
   ```

2. **COMPILEROPTIONS.opt** — Replace the single line in [COMPILEROPTIONS.opt](skel/COMPILEROPTIONS.opt) with two lines, each starting with ` CBL ` and staying under 72 chars:
   ```
    CBL LIB, APOST, NOADV, NODYNAM, RENT, BUF(4096), NOSEQ
    CBL DATA(24), TRUNC(BIN), ZWB, NOOPTIMIZE, TEST, NOSSRANGE
   ```
   Line 1 = 55 chars, Line 2 = 59 chars — both under 72. ✓

3. **confGenerator.ts** — In [confGenerator.ts](client/src/vse/confGenerator.ts#L39), change `normalizeForConf(...)` (which calls `.trim()`) to use a variant that preserves leading spaces. The `normalizeForConf` function strips leading whitespace, which would destroy the ` CBL ` prefix. Change it to only trim trailing whitespace, or change the call at line 39 to `opts.trimEnd()` so the leading ` ` of ` CBL` survives.

4. **Skeleton files** — Remove the hardcoded `.BATCH`/`.CICS` suffix so `#CATALOG#` carries the full sublib name:
   - [COBOL85.skel](skel/COBOL85.skel#L6): `#CATALOG#.BATCH` → `#CATALOG#`
   - [COBOL85DLI.skel](skel/COBOL85DLI.skel#L20): `#CATALOG#.BATCH` → `#CATALOG#`
   - [COBOL85DLICICS.skel](skel/COBOL85DLICICS.skel#L18): `#CATALOG#.CICS` → `#CATALOG#`  (keep the ` INTO` after `#CATALOG#`)

5. **types.ts** — In [types.ts](client/src/vse/types.ts#L38-L39), replace `catalogTest`/`catalogProd` with four fields:
   ```
   catalogBatchTest: string;
   catalogBatchProd: string;
   catalogCicsTest: string;
   catalogCicsProd: string;
   ```

6. **settings.ts** — In [settings.ts](client/src/vse/settings.ts#L41-L42), replace the two `readString` calls with four, each with its default:
   - `catalogBatchTest` → default `"USRWMT.BATCH"`
   - `catalogBatchProd` → default `"USRWMP.BATCH"`
   - `catalogCicsTest` → default `"USRWMT.CICS"`
   - `catalogCicsProd` → default `"USRWMP.CICS"`

7. **package.json** — In [package.json](package.json#L169-L177), replace the two `catalogTest`/`catalogProd` settings with four:
   - `cobol85.vse.placeholders.catalogBatchTest` — default `"USRWMT.BATCH"`, description `"#CATALOG# fuer Non-CICS im Testmodus (T)."`
   - `cobol85.vse.placeholders.catalogBatchProd` — default `"USRWMP.BATCH"`, description `"#CATALOG# fuer Non-CICS im Produktionsmodus (P)."`
   - `cobol85.vse.placeholders.catalogCicsTest` — default `"USRWMT.CICS"`, description `"#CATALOG# fuer CICS im Testmodus (T)."`
   - `cobol85.vse.placeholders.catalogCicsProd` — default `"USRWMP.CICS"`, description `"#CATALOG# fuer CICS im Produktionsmodus (P)."`

8. **jobAssembler.ts** — In [jobAssembler.ts](client/src/vse/jobAssembler.ts#L82-L85), replace the 2-way catalog selection with 4-way based on `conf.type` and `mode`:
   ```typescript
   const isCics = conf.type === 4;
   const catalog = mode === "T"
     ? (isCics
       ? requirePlaceholderValue(settings.placeholders.catalogCicsTest, "cobol85.vse.placeholders.catalogCicsTest")
       : requirePlaceholderValue(settings.placeholders.catalogBatchTest, "cobol85.vse.placeholders.catalogBatchTest"))
     : (isCics
       ? requirePlaceholderValue(settings.placeholders.catalogCicsProd, "cobol85.vse.placeholders.catalogCicsProd")
       : requirePlaceholderValue(settings.placeholders.catalogBatchProd, "cobol85.vse.placeholders.catalogBatchProd"));
   ```

9. **vitest.config.ts** — In [vitest.config.ts](vitest.config.ts#L5), extend `include` to also cover client tests:
   ```
   include: ["server/src/**/*.test.ts", "client/src/**/*.test.ts"]
   ```

10. **New test file: `client/src/vse/confParser.test.ts`** — No vscode mock needed (confParser has no vscode import). Test cases:
    - `parseMemberConf` parses valid conf with all fields
    - `parseMemberConf` handles multi-line quoted `toptions` (` CBL ` format with newlines encoded as `\n`)
    - `parseMemberConf` throws on missing required keys
    - `parseMemberConf` handles optional `txopts`/`pxopts` (defaults to `""`)
    - `serializeMemberConf` produces valid output
    - Roundtrip: `serializeMemberConf` → `parseMemberConf` preserves all fields
    - Roundtrip with multi-line toptions preserves ` CBL ` prefix and newlines

11. **New test file: `client/src/vse/jobAssembler.test.ts`** — Mock `vscode` with `vi.mock("vscode", ...)`. Test cases for `assembleJobText`:
    - Type 2 (COBOL85) + mode T → uses `catalogBatchTest` value
    - Type 2 + mode P → uses `catalogBatchProd` value
    - Type 3 (DLI) + mode T → uses `catalogBatchTest` value
    - Type 3 + mode P → uses `catalogBatchProd` value
    - Type 4 (DLI+CICS) + mode T → uses `catalogCicsTest` value
    - Type 4 + mode P → uses `catalogCicsProd` value
    - Missing catalog setting → throws with setting key in message
    - `#COMPILEOPTIONS#` replacement works with multi-line ` CBL ` content
    - `#LNKSTEP#` expansion preserves line breaks
    - All `REQUIRED_TOKENS` are resolved (no unresolved placeholders error)
    - Unresolved placeholder triggers error

12. **Version bump + CHANGELOG** — Bump `package.json` version to `0.0.13`. Add CHANGELOG entry for v0.0.13 with all three user-visible changes.

13. **Build, test, package, release** — Compile, run full test suite (535+ existing + new tests), `bash scripts/package-vsix.sh`, commit, tag `v0.0.13`, push, `gh release create`.

**Verification**
- `npm run compile` — clean compilation
- `npx vitest run` — all tests pass (existing 535 + new ~20 tests)
- `bash scripts/package-vsix.sh` — VSIX builds and passes verification
- Manual: check that COMPILEROPTIONS.opt lines start with ` CBL ` and are ≤72 chars
- Manual: verify settings in VS Code show 4 catalog fields with correct defaults

**Decisions**
- `#CATALOG#` now carries the **full** sublib name (e.g., `USRWMT.BATCH`) instead of just the base (`USRWMT`). The `.BATCH`/`.CICS` suffixes are removed from skeletons. This is cleaner because the user controls the exact library name.
- CICS detection uses `conf.type === 4` (the same logic used by `confGenerator.ts` to select skeletons).
- `normalizeForConf` needs adjustment to preserve leading whitespace for ` CBL ` lines — using `trimEnd()` instead of `trim()`.
- Tests go in `client/src/vse/` with vscode mock via `vi.mock("vscode", ...)`.
