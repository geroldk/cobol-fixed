# COBOL Fixed Format Extension - Analyse und aktueller Stand

> Letzte Aktualisierung: 2026-03-01
> Verifiziert in diesem Check:
> - `npm run compile`: erfolgreich
> - `npm run test`: erfolgreich (`13` Testdateien, `522` Tests, alle bestanden)

## 1. Executive Summary

Der aktuelle Code-Stand ist deutlich weiter als der vorherige Analyse-Snapshot.
Die Erweiterung deckt heute nicht nur klassische COBOL-85-Checks ab, sondern hat
eine breite, kontext-sensitive Unterstuetzung fuer `EXEC DLI` und `EXEC CICS`
in Diagnostics, Completion, Hover und Semantic Tokens.

Die Kernarchitektur ist stabil:
- modulare LSP-Implementierung
- copybook- und replace-faehige Preprocessor-Pipeline
- Tree-sitter Parse mit Retry-faehiger Initialisierung
- qualifier-aware Symbolauflosung fuer Definition/References/Rename
- Dead-Code- und Undefined-Identifier-Analyse

## 2. Verifizierter Build- und Teststatus

### 2.1 Tooling-Status
- `npm run compile`: erfolgreich
- `npm run test`: erfolgreich

### 2.2 Test-Snapshot (Vitest)

| Testdatei | Anzahl Tests |
|---|---|
| `server/src/utils.test.ts` | 29 |
| `server/src/hover.test.ts` | 33 |
| `server/src/parser-bridge.test.ts` | 14 |
| `server/src/definition.test.ts` | 23 |
| `server/src/references.test.ts` | 13 |
| `server/src/dead-code.test.ts` | 10 |
| `server/src/symbols.test.ts` | 10 |
| `server/src/completion.test.ts` | 67 |
| `server/src/preprocessor.test.ts` | 23 |
| `server/src/semantic-tokens.test.ts` | 72 |
| `server/src/normalizer.test.ts` | 11 |
| `server/src/rename.test.ts` | 13 |
| `server/src/lint.test.ts` | 204 |
| **Summe** | **522** |

## 3. Architektur (Ist-Zustand)

### 3.1 High-Level Pipeline

1. `basicFixedFormatChecks` (Raw-Text-Pruefungen)
2. `preprocessUri` (COPY/REPLACE Expansion + Mapping-Segmente)
3. `normalizeForCobol85Parser` (Parser-kompatible Normalisierung)
4. Tree-sitter Parse + Parse-Fehlerabbildung auf Quelltext
5. `lintPreprocessed`
6. `lintDeadCode`
7. `lintUndefinedIdentifiers`
8. Publish pro URI mit sauberem Multi-Dokument-Clearing

### 3.2 Wichtige Server-Eigenschaften

- LSP Capabilities:
  - Document Symbols
  - Definition
  - Hover
  - Completion
  - References
  - Rename (`prepareProvider`)
  - Semantic Tokens (full)
- Konfigurations-Handling:
  - `cobol85.warnings.blockClosedByPeriod`
  - `cobol85.copybookPaths`
- Debounce + Rerun-Mechanismus fuer Validierung
- Retry-faehige Tree-sitter Initialisierung bei Init-Fehlern

## 4. Fachliche Abdeckung

### 4.1 Symbolik und Navigation

- `buildDefinitionIndex` extrahiert:
  - Paragraphs
  - Sections
  - Data Items
  - `SELECT` Dateinamen (ENVIRONMENT DIVISION)
  - `FD`/`SD` Dateinamen
  - `INDEXED BY` Namen
- Qualifier-Support:
  - `OF` / `IN` Erkennung
  - qualifizierte Symbolauflosung
  - Ambiguitaetsbehandlung bei gleichnamigen Datenfeldern
- References/Rename nutzen `resolveSymbolAtOccurrence` fuer robuste Trefferauflosung.

### 4.2 Preprocessor und Copybook-Integration

- COPY Statement Parsing mit `REPLACING`
- REPLACE/REPLACE OFF Parsing und Anwendung
- Rueckabbildung generierter Offsets/Range auf Originaldateien (`mapGenRange`)
- Cache fuer Datei- und Copybook-Aufloesung

### 4.3 Diagnostics (Lint + Analyse)

- COBOL-Basisregeln:
  - Data-Division-Periodenregeln
  - Procedure-Verben und Tippfehler
  - Block-Closure (`BLOCK_CLOSED_BY_PERIOD`)
  - VSAM-nahe Checks
- IBM/VSE-Naehe:
  - Directive-Erkennung (`CBL`, `PROCESS`, `BASIS`, `INSERT`, `SERVICE`, `*CONTROL`, `*CBL`)
  - IBM-Extension-Warnings (`READY`, `RESET`, `TRACE`)
- EXEC DLI:
  - Request-Resolution (Kurz-/Langformen)
  - Klausel-Matrix/Regeln je Request
  - Reihenfolge- und Kombinationsregeln
- EXEC CICS:
  - umfangreiche Command/Option-Spezifikation
  - Pflichtoptionen, ungueltige Optionen, Condition-Pruefung
- Erweiterte Analyse:
  - Dead-Code fuer unreferenzierte Paragraphs/Sections
  - Undefined-Identifier-Warnungen auf Basis Definition-Index

### 4.4 Completion, Hover und Semantic Tokens

- Completion:
  - Kontext im `EXEC DLI` Block (Function/Clause/Argument)
  - Kontext im `EXEC CICS` Block (Verb/Option/Condition/Argument)
- Hover:
  - symbolischer Hover (Data/Paragraph/Section)
  - suppress fuer DLI/CICS Keywords ausserhalb Argumentklammern
  - Systemfeld-Fallbacks fuer `EIB_*` und `DIB_*`
- Semantic Tokens:
  - differenzierte Klassifizierung fuer COBOL + DLI + CICS Kontexte
  - umfangreiche Testabdeckung

## 5. Modul-Mapping (Server)

| Modul | Verantwortung |
|---|---|
| `server.ts` | LSP Orchestrierung, Scheduling, Diagnostics-Publishing |
| `preprocessor.ts` | COPY/REPLACE Verarbeitung, Segment-Mapping |
| `normalizer.ts` | parserfreundliche Textnormalisierung |
| `lint.ts` | Kernlinting inkl. DLI/CICS/Undefined-Identifier |
| `definition.ts` | Indexaufbau, Symbolauflosung, Definition |
| `references.ts` | Referenzsuche und Definition-Site-Filter |
| `rename.ts` | Prepare/Perform Rename mit Ambiguitaetskontrolle |
| `hover.ts` | Hover-Inhalte, Kontextfilter, EIB/DIB-Fallback |
| `completion.ts` | kontext-sensitive Completion |
| `semantic-tokens.ts` | Tokenisierung und LSP-Encoding |
| `dead-code.ts` | Dead-Code-Erkennung fuer Paragraph/Section |
| `utils.ts` | Fixed-format Helfer, Copybook-Aufloesung, BMS Utilities |
| `parser-bridge.ts` | Tree-sitter Edit/Fehlerbruecke |

## 6. Risiken und offene Punkte

### 6.1 Technische Restthemen

- TextMate-Grammatik hat weiterhin keine dedizierten automatisierten Regex/Snapshot-Tests.
- Workspace-weite Symbolsuche (`workspace/symbol`) ist nicht implementiert.
- Code Actions (z. B. Auto-Fix fuer bestimmte Warnings) sind nicht implementiert.
- Kein expliziter E2E-Satz ueber VS Code Extension Host in diesem Repo-Stand.

### 6.2 Release-Hinweis

- Paketversion ist weiterhin `0.0.3`, waehrend der Working Tree bereits deutlich ueber
  den letzten Tag hinaus entwickelt wurde (siehe `CHANGELOG.md` / `Unreleased`).

## 7. Empfohlene naechste Schritte

1. TextMate-Grammatiktests einfuehren (mindestens Smoke/Snapshot).
2. Kleine E2E-Suite fuer zentrale LSP-Flows (Definition/Hover/Completion).
3. Code Actions fuer haeufige Lint-Hinweise priorisieren.
4. Danach Release-Schnitt vorbereiten (`0.0.4`) mit sauberem Changelog-Freeze.
