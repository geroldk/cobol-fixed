# Changelog

Kompletter Verlauf von "Stunde 0" bis jetzt.

## [Unreleased]

- In Arbeit.

<!-- AUTO-CHANGELOG-START -->
### Snapshot (auto)
- Fuehre `npm run changelog:update` aus, um den technischen Snapshot zu aktualisieren.
<!-- AUTO-CHANGELOG-END -->

## [0.0.5] - 2026-03-02

### Changed
- Versionserhoehung von `0.0.4` auf `0.0.5` (`package.json`, `package-lock.json`).
- Packaging-Filter in `.vscodeignore` angepasst, damit benoetigte Runtime-Abhaengigkeiten wieder im VSIX enthalten sind.

### Fixed
- VSIX-Paket enthielt durch zu aggressives Exclude (`node_modules/**`) keine LSP-/Tree-sitter-Runtime-Module mehr.
- Build-Artefakte aus Tests werden fuer das VSIX jetzt gezielt ausgeschlossen (`server/out/**/*.test.js`, `server/out/**/*.test.js.map`).

## [0.0.4] - 2026-03-01

### Added
- Kontext-sensitive Completion fuer `EXEC DLI` und `EXEC CICS` (Verben, Klauseln/Optionen, Bedingungen, Datennamen in Klammerargumenten).
- Umfangreiche `EXEC CICS` Lint-Spezifikation mit Kommando-/Optionsregeln und Condition-Validierung (`CICS_COMMAND_SPECS`, `CICS_KNOWN_CONDITIONS`).
- Erweiterte `EXEC DLI` Lint-Validierung fuer Kurz-/Langformen, Klausel-Reihenfolgen und Kombinationsregeln.
- Neuer Undefined-Identifier-Lint (`UNDEFINED_IDENTIFIER`) auf Basis des Definition-Index.
- Hover-Fallbacks fuer Systemfelder (`EIB_*`, `DIB_*`) inkl. PIC/Beschreibung.
- Neue Utilities fuer BMS (`isBmsSource`, `parseBmsMapset`, `generateCobolFromBms`) inkl. Tests.
- Neue/fortgefuehrte Spezifikations- und Planungsdokumente unter `docs/` und Referenz-PDFs unter `specs/`.
- Neue Dev-Dependencies: `vscode-oniguruma`, `vscode-textmate`.
- Skript `npm run changelog:update` zur Snapshot-Aktualisierung von `CHANGELOG.md`.

### Changed
- Versionserhoehung von `0.0.3` auf `0.0.4` (`package.json`, `package-lock.json`).
- Symbolauflosung fuer Definition/References/Rename um Qualifier-Logik erweitert (`OF`/`IN`, exakte Definition-Sites, Ambiguitaetsbehandlung).
- Definition-Index ausgebaut: `INDEXED BY`-Namen, `SELECT`-Dateinamen (ENVIRONMENT DIVISION), `FD`/`SD`-Namen, Parent-Hierarchien.
- Dead-Code-Analyse auf Batch-Occurrence-Scan umgestellt (`findAllOccurrencesBatch`) inkl. Definition-Site-Filter.
- Validation-Profil um `deadCodeMs` erweitert; Server validiert jetzt zusaetzlich undefinierte Identifier.
- Compiler-Directive-Erkennung erweitert (`BASIS`, `INSERT`, `SERVICE`, `*CONTROL`, `*CBL`).
- Grammatik/Highlighting erweitert:
  - `cobol_fixed.tmLanguage.json`: eigener `EXEC CICS ... END-EXEC`-Scope, CICS-Verben/Optionen/Conditions.
  - `cobol85.tmLanguage.json`: zusaetzliche DLI-Keywords/Klauseln und Intrinsic-Functions.
- `README.md` und `ANALYSIS.md` auf aktuellen technischen Stand gebracht.

### Fixed
- Hover fuer CICS-/DLI-Schluesselwoerter wird innerhalb `EXEC ... END-EXEC` ausserhalb von Klammerargumenten unterdrueckt (weniger Fehl-Hover).
- Undefinierte Identifier in `EXEC`-Bloecken werden differenziert behandelt (Host-Variablen in Argumenten werden geprueft).
- Dead-Code-Analyse vermeidet False-Positives an Definition-Sites und reduziert Suchaufwand.
- Rename/References behandeln qualifizierte Datennamen robuster und lehnen mehrdeutige unqualifizierte Treffer ab.

## [0.0.3] - 2026-02-26

### Added
- Neue Architektur- und Umsetzungsdokumentation in `ANALYSIS.md`.
- Modularisierte Server-Implementierung mit dedizierten LSP-Modulen (`completion.ts`, `definition.ts`, `hover.ts`, `references.ts`, `rename.ts`, `semantic-tokens.ts`, `dead-code.ts`, `symbols.ts`, `lint.ts`, `preprocessor.ts`, `normalizer.ts`, `utils.ts`, `parser-bridge.ts`).
- Umfangreiche Unit-Tests fuer alle Kernmodule (`13` Testdateien, `158` Tests).

### Changed
- Versionserhoehung von `0.0.2` auf `0.0.3` (`package.json`, `package-lock.json`).
- `server/src/server.ts` auf Orchestrierung reduziert; Fachlogik in Module ausgelagert.
- `README.md` erweitert und um Doku-Links auf `ANALYSIS.md` sowie `CHANGLOG.md` ergaenzt.
- Parser- und Highlighting-Artefakte aktualisiert (`server/assets/tree-sitter-cobol.wasm`, `cobol85.tmLanguage.json`, `language-configuration.json`).

### Fixed
- Hover und Semantic Tokens nutzen jetzt konsistent den COPY-expandierten Analysekontext.
- Dead-Code-Analyse behandelt `PERFORM ... THRU/THROUGH` ohne False-Positives.
- Symbol-Ranges fuer Outline folgen der LSP-Erwartung (Containerbereich inkl. Kinder).

## [0.0.2] - 2026-02-25

### Added
- Neues VS Code Setting `cobol85.warnings.blockClosedByPeriod` zum Abschalten der Warnung `BLOCK_CLOSED_BY_PERIOD`.
- Neue `README.md` mit Projektbeschreibung, Installation, Version und Settings.

### Changed
- Versionserhoehung von `0.0.1` auf `0.0.2` (`package.json`, `package-lock.json`).
- Client-Server-Konfig-Synchronisierung fuer `cobol85` aktiviert.
- Server-Settings-Handling ergaenzt (Config laden, auf Aenderungen reagieren, Re-Validate).
- Parser/WASM und Grammatiken aktualisiert (`server/assets/tree-sitter-cobol.wasm`, TextMate-Grammatiken).
- Submodul `vendor/tree-sitter-cobol` auf Commit `f766bdd` aktualisiert.

### Commits
- `013edb5` Add configurable BLOCK_CLOSED_BY_PERIOD warning, bump version to 0.0.2, and refresh parser/docs

## [0.0.1] - 2026-02-25

### Added
- Initiale Projektstruktur fuer die VS Code Extension.
- COBOL-Quellen fuer Client/Server, Grammatiken und Build-Setup.
- `THIRD-PARTY-LICENSES.md`.
- Lizenzfeld `MIT` in `package.json`.

### Changed
- Submodul `vendor/tree-sitter-cobol` initial eingebunden (`e99dbdc`) und spaeter auf `7a813b` aktualisiert.
- Submodul-URL von Upstream auf Fork umgestellt:
  `https://github.com/yutaro-sakamoto/tree-sitter-cobol` -> `https://github.com/geroldk/tree-sitter-cobol`.
- `package-lock.json` mit aktualisierter VS Code Engine-Anforderung (`^1.109.0`).
- Merge von PR #1 (`copilot/check-license-requirements`).

### Commits
- `90c6c3e` inital
- `c7b32c4` Add COBOL extension sources and .gitignore
- `aa90795` Update vendor/tree-sitter-cobol submodule
- `791a307` new  url
- `ed52f03` Initial plan
- `8d2c621` Add THIRD-PARTY-LICENSES.md initial plan
- `fe555e2` Add THIRD-PARTY-LICENSES.md and set license field in package.json
- `46ac949` Merge pull request #1 from geroldk/copilot/check-license-requirements
