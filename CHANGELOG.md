# Changelog

Kompletter Verlauf von "Stunde 0" bis jetzt.

## [Unreleased]

- In Arbeit.

## [0.0.13] - 2026-03-05

### Fixed
- **VSE-Fehlermeldung 196611**: Text geaendert zu "Eintrag konnte in Reader Queue nicht erstellt werden".
- **Doppelter Catalog-Suffix behoben**: `#CATALOG#` ersetzt jetzt nur den Basisnamen (z.B. `USRWMT`), der `.BATCH`/`.CICS`-Suffix bleibt im Skeleton. Keine doppelten Suffixe mehr (z.B. `USRWMT.BATCH.BATCH`).
- **CBL-Zeilenumbruch ohne Komma am Ende**: `formatCompileOptions()` bricht Zeilen jetzt so um, dass kein Komma am Zeilenende steht. Komma wird als Separator zwischen Optionen verwendet, nicht als Trailing-Zeichen.

### Changed
- **CBL-Formatierung bei Job-Expansion**: Compiler-Optionen werden in der `.conf`-Datei weiterhin als einfacher Komma-separierter String gespeichert (rueckwaertskompatibel). Erst bei der Job-Expansion (`#COMPILEOPTIONS#`) werden sie automatisch mit ` CBL `-Praefix formatiert und bei > 72 Zeichen umgebrochen.
- **#CATALOG# aufgeteilt in 4 Settings** (Basisname ohne Suffix):
  - `cobol85.vse.placeholders.catalogBatchTest` (Default: `USRWMT`)
  - `cobol85.vse.placeholders.catalogBatchProd` (Default: `USRWMP`)
  - `cobol85.vse.placeholders.catalogCicsTest` (Default: `USRWMT`)
  - `cobol85.vse.placeholders.catalogCicsProd` (Default: `USRWMP`)
- CICS-Erkennung ueber `conf.type === 4` (Member-Typ DLI+CICS).

### Added
- **`formatCompileOptions()`**: Neue Funktion im Job-Assembler, die Compiler-Optionen zur Laufzeit in ` CBL `-Zeilen (max 72 Zeichen) formatiert.
- **41 neue Testfaelle** in 2 neuen Testdateien:
  - `confParser.test.ts` (19 Tests): Parsing, Serialisierung, Roundtrip, Fehlerbehandlung.
  - `jobAssembler.test.ts` (22 Tests): Catalog-Auswahl nach Typ×Modus, CBL-Formatierung, Double-Suffix-Pruefung, LNKSTEP, XOPTS.
- Versionserhoehung von `0.0.12` auf `0.0.13`.

### Fixed
- **vseconnector-ts in VSIX gebundelt**: Das Modul `vseconnector-ts` wird jetzt komplett in die VSIX-Datei integriert (nur `dist/`, ohne unnoetige CLI-Abhaengigkeiten). VSE-Funktionen (Submit, Password, etc.) funktionieren jetzt direkt nach der Installation ohne zusaetzliche Schritte.

### Changed
- **package-vsix.sh**: Statt das Modul zu entfernen, wird jetzt eine eigenstaendige Kopie von `vseconnector-ts/dist/` in `node_modules/` angelegt und mitverpackt.
- **verify-vsix.sh**: `vseconnector-ts` von der Verbotsliste in die Pflichtliste verschoben. Require-Chain-Test prueft jetzt auch die Aufloesbarkeit von `vseconnector-ts`.
- Versionserhoehung von `0.0.11` auf `0.0.12`.

## [0.0.11] - 2026-03-05

### Fixed
- **PROCEDURE_VERB_UNKNOWN false positives**: Rein-alphabetische Datennamen wie `CKZ` und `AVALUE` mit `OF`/`IN`-Qualifier auf Folgezeilen von mehrzeiligen Statements (z.B. DISPLAY, CALL USING) wurden faelschlicherweise als unbekannte COBOL-Verben gemeldet. Ursache: Datennamen mit Ziffern/Bindestrichen (z.B. `WX10`, `BIS-KZ`, `CLOSE-DATUM`) auf den Zwischenzeilen brachen die Continuation-Chain ab, sodass nachfolgende rein-alphabetische Tokens (`CKZ`, `AVALUE`) nicht mehr als Continuation erkannt wurden. Die Lint-Logik propagiert jetzt den Continuation-Status korrekt durch nicht-rein-alphabetische Datennamen.

### Added
- **13 neue Testfaelle** fuer `PROCEDURE_VERB_UNKNOWN` / `isLikelyProcedureContinuationLine`:
  - Multi-line DISPLAY mit OF-qualifizierten Datennamen (CKZ- und AVALUE-Szenario)
  - Chain-Propagation durch gemischt-alphanumerische Datennamen
  - MOVE-Continuation mit IN-Qualifier
  - CALL USING Continuation
  - Echte unbekannte Verben werden weiterhin korrekt erkannt (nach Period)
  - Unit-Tests fuer `isLikelyProcedureContinuationLine` (Indent, OF, IN, Period, Kontext)

### Changed
- Versionserhoehung von `0.0.10` auf `0.0.11`.

## [0.0.10] - 2026-03-05

### Fixed
- **VSE-Fehlermeldung**: `Cannot find module 'vseconnector-ts'` beim Druecken von F5 wird jetzt sauber abgefangen. Statt dem rohen Node.js-Stack wird eine klare deutsche Meldung angezeigt: *"Das Modul 'vseconnector-ts' ist nicht installiert. VSE-Funktionen sind nicht verfuegbar."* Betrifft alle VSE-Kommandos (Submit, Password, etc.).

### Added
- **VSIX-Verifikation**: Neues Script `scripts/verify-vsix.sh` prueft automatisch nach dem Bauen:
  - Pflicht-Dateien vorhanden (extension.js, server.js, WASM, node_modules)
  - Verbotene Dateien absent (vseconnector-ts, tree-sitter-cli, nested VSIX)
  - Require-Chain-Test (extension.js laesst sich ohne Crash laden)
  - Groessen-Check (Warnung bei > 5 MB)
- `npm run verify-vsix` als eigenstaendiges Script verfuegbar.
- `scripts/package-vsix.sh` fuehrt die Verifikation jetzt automatisch nach dem Bauen aus.

### Changed
- Versionserhoehung von `0.0.9` auf `0.0.10`.

## [0.0.9] - 2026-03-05

### Fixed
- **Tree-sitter Grammar**: `linkage_section` und `local_storage_section` erlauben jetzt leere Bodies (keine Data-Descriptions). Vorher erzwang die Grammatik mindestens einen Eintrag (`repeat1`), was bei COPY-Books die nicht aufgeloest werden konnten zu einem falschen `TS_PARSE_ERROR` an `LINKAGE SECTION.` fuehrte.
- **VSIX-Packaging**: `vseconnector-ts` (`file:`-Link) verhinderte, dass `vsce` die `node_modules` ins VSIX einpackte (`npm list --production` schlug fehl). Neues Package-Script (`scripts/package-vsix.sh`) entfernt die lokale Abhaengigkeit temporaer beim Bauen und stellt sie danach wieder her.
- **VSIX-Groesse**: `tree-sitter` CLI-Binary (18 MB), `build_err.log` und `package.json.bak` werden jetzt via `.vscodeignore` ausgeschlossen.

### Changed
- Versionserhoehung von `0.0.8` auf `0.0.9` (`package.json`, `package-lock.json`).
- Tree-sitter WASM neu gebaut mit der korrigierten Grammatik (`server/assets/tree-sitter-cobol.wasm`).
- `tree-sitter-cli` Dev-Dependency von `^0.25.0` auf `^0.25.10` aktualisiert.

### Added
- Neues Script `scripts/package-vsix.sh` fuer robustes VSIX-Packaging (kompiliert, bereinigt `package.json` temporaer, packt, stellt wieder her).
- Neues npm-Script `package` in `package.json`.

## [0.0.8] - 2026-03-04

### Added
- VSE compile submit workflow auf `.conf`-Basis inkl. neuer Commands:
  - `cobol85.vseSubmitCompileJob`
  - `cobol85.vseCreateMemberConf`
  - `cobol85.vseSetPassword`
  - `cobol85.vseClearPassword`
- Neue VSE-Module unter `client/src/vse/` fuer Parsing/Resolver/Generator/Assembler/Submit-Orchestrierung.
- Submit-Preview vor dem Senden mit Setting `cobol85.vse.submit.previewBeforeSubmit`.
- Neue Keybinding-Zuordnung `F5` fuer `cobol85.vseSubmitCompileJob` im COBOL-Editor.
- Skeleton- und Default-Dateien unter `skel/` fuer Typen `2/3/4` (`COBOL85*`, `*.xopt`, `COMPILEROPTIONS.opt`).

### Changed
- Versionserhoehung von `0.0.7` auf `0.0.8` (`package.json`, `package-lock.json`).
- README um VSE-Submit-Dokumentation, Settings, Workflow und Shortcut-Hinweis erweitert.
- Package-Konfiguration um VSE-Settings/Commands erweitert und lokale Abhaengigkeit `vseconnector-ts` aufgenommen.

### Fixed
- `#LNKSTEP#`-Expansion erzeugt jetzt bei Inline-Verwendung einen erzwungenen Zeilenumbruch fuer nachfolgende JCL.
- Zusaeztliche Safety-Net-Korrektur fuer `...#LNKSTEP#// ...` in bestehendem Legacy-Conf-Inhalt.
- Skeletons korrigiert, sodass `#LNKSTEP#` nicht mehr mit nachfolgenden Karten (`// EXEC ...`, `/&`) verklebt ist.

## [0.0.7] - 2026-03-02

### Changed
- Versionserhoehung von `0.0.6` auf `0.0.7` (`package.json`, `package-lock.json`).

### Fixed
- Mitigation fuer den V8-WASM-Crash `Fatal process out of memory: Zone` in VS Code `1.98` bis `1.106` durch sichere `execArgv`-Flags fuer den Language-Server-Prozess.

### Compatibility
- Verhalten auf VS Code `>=1.107.0` bleibt unveraendert; die zusaetzlichen V8-Flags sind dort weiterhin unkritisch.

## [0.0.6] - 2026-03-02

### Changed
- Versionserhoehung von `0.0.5` auf `0.0.6` (`package.json`, `package-lock.json`).
- VS Code Engine-Anforderung auf `^1.104.0` abgesenkt (`package.json`, `README.md`).
- Dev-Typdefinitionen auf `@types/vscode@^1.104.0` abgesenkt.

### Fixed
- Kompatibilitaet fuer Installationen mit VS Code `1.104.x` bis `<1.109.0` wiederhergestellt.

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
