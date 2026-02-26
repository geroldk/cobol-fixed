# COBOL Fixed Format Extension — Analyse, Bugs & Verbesserungsplan

> **Letzte Aktualisierung:** 2026-02-26
> **Test-Status:** 13 Dateien, 158 Tests — alle bestanden
> **Compile-Status:** fehlerfrei

---

## 1. Architektur-Übersicht

### 1.1 Gesamtstruktur

Die Extension folgt dem **LSP-Pattern** (Language Server Protocol) mit Client-Server-Trennung:

```
┌───────────────────────────────┐     ┌──────────────────────────────────────────┐
│  CLIENT (extension.ts)        │     │  SERVER (server.ts + Module)             │
│                               │ IPC │                                          │
│  • VS Code Extension Host     │◄───►│  • Eigener Node.js Prozess              │
│  • Command Registration       │     │  • Tree-sitter WASM Parser              │
│  • LSP LanguageClient         │     │  • Preprocessor (COPY-Expansion)        │
│  • Force-Validate Notification│     │  • Linting & Diagnostics                │
│                               │     │  • Document Symbols (Outline)           │
│                               │     │  • Go-to-Definition / Hover / Completion│
│                               │     │  • References / Rename / Semantic Tokens│
│                               │     │  • Dead-Code-Erkennung                 │
└───────────────────────────────┘     └──────────────────────────────────────────┘
```

### 1.2 Dateien & Verantwortlichkeiten

Die Codebasis ist modular in fokussierte Dateien aufgeteilt:

| Datei | Zweck | Tests |
|---|---|---|
| `package.json` | Extension-Manifest: Language-ID `cobol85`, Extensions `.cob/.cbl/.cpy`, Commands, Settings | — |
| `client/src/extension.ts` | VS Code Client: LSP-Client-Start, Toggle-Comment-Command (`Ctrl+/`), Force-Validate | — |
| `server/src/server.ts` | LSP-Orchestrierung (~995 Zeilen): Wiring, Scheduling, Validation-Pipeline | — |
| `server/src/types.ts` | Shared Type-Definitionen (Settings, Profiling, Segments, Lint, Parser) | — |
| `server/src/utils.ts` | Utility-Funktionen: Fixed-Format-Helfer, File-I/O, Caching, URI-Handling | 20 Tests |
| `server/src/normalizer.ts` | Parser-Normalisierung: ID DIVISION-Alias, Listing-Control, Compiler-Direktiven | 11 Tests |
| `server/src/preprocessor.ts` | COPY-Expansion, REPLACING, Segment-Mapping, Statement-Parsing | 20 Tests |
| `server/src/lint.ts` | Lint-Regeln: IF/EVALUATE-Matching, EXEC DLI/CICS, Verb-Typos, VSAM-Status | 16 Tests |
| `server/src/symbols.ts` | Document-Symbols fuer Outline-Ansicht (PROGRAM-ID, DIVISIONs, SECTIONs) | 10 Tests |
| `server/src/parser-bridge.ts` | Tree-sitter-Integration: UTF-8/16-Konvertierung, Incremental Edit, Error Collection | 14 Tests |
| `server/src/definition.ts` | Go-to-Definition: Copybooks, Paragraphen, Sections, Data-Names | 12 Tests |
| `server/src/hover.ts` | Hover-Informationen: Paragraphen, Data-Items (PIC/USAGE), Copybook-Pfade | 4 Tests |
| `server/src/completion.ts` | Auto-Completion: Context-aware Verben, Symbole, Copybook-Namen | 9 Tests |
| `server/src/references.ts` | Find All References: Paragraphen, Sections, Data-Names | 12 Tests |
| `server/src/semantic-tokens.ts` | Semantic Tokens: Keywords, Variables, Functions, Namespaces, etc. | 10 Tests |
| `server/src/rename.ts` | Rename-Provider: COBOL-Identifier-Validierung, Cross-Occurrence-Rename | 10 Tests |
| `server/src/dead-code.ts` | Dead-Code-Erkennung: Unreferenzierte Paragraphen/Sections, PERFORM-THRU-aware | 10 Tests |
| `cobol85.tmLanguage.json` | TextMate-Grammatik fuer Syntax-Highlighting (Fixed-Format-aware) | — |
| `language-configuration.json` | Klammern, Folding, Indent-Regeln, Auto-Closing-Pairs | — |
| `vendor/tree-sitter-cobol/` | Git-Submodul fuer die Tree-sitter COBOL-Grammatik | — |
| `server/assets/tree-sitter-cobol.wasm` | Vorkompilierter WASM-Parser | — |

### 1.3 Verarbeitungs-Pipeline (Validation)

```
             Dokument-Text
                  │
                  ▼
        ┌─────────────────┐
        │ Basic Checks     │  TABs, Ueberlauf >80, Indicator-Spalte, ID DIVISION
        └────────┬────────┘
                  ▼
        ┌─────────────────┐
        │ Preprocessor     │  COPY-Expansion, REPLACING, Segment-Mapping
        └────────┬────────┘
                  ▼
        ┌─────────────────┐
        │ Normalisierung   │  ID→IDENTIFICATION, EJECT/SKIP neutralisieren,
        │                  │  Klammer-OR normalisieren, Doc-Paragraphen ausblenden
        └────────┬────────┘
                  ▼
        ┌─────────────────┐
        │ Tree-sitter Parse│  Inkrementell, WASM-basiert, Fehler-Mapping zurueck
        └────────┬────────┘
                  ▼
        ┌─────────────────┐
        │ Lint             │  IF/EVALUATE-Matching, EXEC DLI/CICS/SQL,
        │                  │  Data-Entry-Periods, Verb-Typos, VSAM-Status
        └────────┬────────┘
                  ▼
        ┌─────────────────┐
        │ Dead-Code        │  Unreferenzierte Paragraphen/Sections (Hint-Severity)
        └────────┬────────┘
                  ▼
         Diagnostics → VS Code
```

### 1.4 Debounce- und Scheduling-Logik

- **Typing:** 180ms Debounce, dann Validierung *ohne* Parser (nur Basic + Lint)
- **Idle:** Nach 900ms Inaktivitaet wird der Parser zusaetzlich gestartet
- **Force:** Bei Tab-Wechsel, Datei-oeffnen, Config-Aenderung: sofortige Vollvalidierung
- **Epoch-Tracking:** Verhindert veraltete Ergebnisse bei schnellem Tippen
- **Rerun-Queue:** Laufende Validierung wird nicht abgebrochen, sondern erneut gescheduled

### 1.5 LSP Capabilities (registriert)

| Capability | Provider | COPY-expanded |
|---|---|---|
| `textDocumentSync` | Incremental | — |
| `documentSymbolProvider` | `symbols.ts` | Nein |
| `definitionProvider` | `definition.ts` | Ja |
| `hoverProvider` | `hover.ts` | Ja |
| `completionProvider` | `completion.ts` | Ja |
| `referencesProvider` | `references.ts` | Ja |
| `semanticTokensProvider` | `semantic-tokens.ts` | Ja |
| `renameProvider` | `rename.ts` | Ja |

---

## 2. Feature-Dokumentation

### 2.1 Syntax Highlighting (TextMate + Semantic Tokens)

**TextMate-Grammatik** (`cobol85.tmLanguage.json`):
- Erkennung der Fixed-Format-Bereiche: Sequenz (1-6), Indicator (7), Language (8-72), Identification (73-80), Overflow (>80)
- Separate Behandlung von Comment (`*`/`/`), Debug (`D`/`d`), Continuation (`-`), Normal (` `) und Invalid Indicator Zeilen
- COBOL-Keywords, Datentypen, Figurative Konstanten, Operatoren, String-Literale

**Semantic Tokens** (`semantic-tokens.ts`):
- Feinere Klassifizierung basierend auf Definition-Index:
  - `keyword` (COBOL-Verben, Division/Section-Keywords)
  - `variable` (Data-Names)
  - `function` (Paragraphen)
  - `namespace` (Sections)
  - `string`, `number`, `comment`, `operator`
  - `macro` (COPY/EXEC)
  - `type` (Level-Nummern)
  - `parameter` (FILLER, Spezialregister)
- Definition-Sites mit `declaration`-Modifier markiert

### 2.2 Toggle Fixed Comment (`Ctrl+/`)
- Togglet Spalte 7 zwischen `*` und ` ` (Space)
- Multi-Cursor/Multi-Selection-faehig
- Tab-Expansion fuer konsistente Spaltenberechnung
- Warnung bei Tabs im Dokument

### 2.3 Diagnostiken

| Code | Schwere | Beschreibung |
|---|---|---|
| `TAB_FIXED_FORMAT` | Warning | Tab-Zeichen gefunden |
| `LINE_OVERFLOW` | Warning | Zeile >80 Spalten |
| `FIXED_FORMAT_REQUIRED` | Error | Zeile zu kurz fuer Fixed-Format |
| `INVALID_FIXED_INDICATOR` | Error | Ungueltiges Zeichen in Spalte 7 |
| `MISSING_IDENTIFICATION_DIVISION` | Error | ID DIVISION fehlt (wenn Nicht-Copybook) |
| `TS_PARSE_ERROR` | Error | Tree-sitter Syntaxfehler |
| `TS_ERROR_THROTTLED` | Warning | Zu viele Syntaxfehler (>25) |
| `COPYBOOK_NOT_FOUND` | Error | COPY-Book nicht aufgeloest |
| `COPYBOOK_CYCLE` | Error | Zyklisches COPY erkannt |
| `COPYBOOK_MAX_DEPTH` | Error | COPY-Tiefe >20 |
| `COPYBOOK_READ_ERROR` | Error | Copybook nicht lesbar |
| `COPY_MISSING_PERIOD` | Warning | COPY ohne `.` |
| `COPY_REPLACING_EMPTY` | Warning | REPLACING ohne Paare |
| `COPY_REPLACING_NO_MATCH` | Warning | FROM-Pattern nicht im Copybook |
| `BLOCK_CLOSED_BY_PERIOD` | Warning | IF/EVALUATE durch `.` beendet statt END-IF/END-EVALUATE |
| `UNCLOSED_BLOCK` | Error | IF/EVALUATE/EXEC ohne End-Keyword |
| `END_IF_WITHOUT_IF` | Error | END-IF ohne vorangehendes IF |
| `END_EVALUATE_WITHOUT_EVALUATE` | Error | END-EVALUATE ohne EVALUATE |
| `END_EXEC_WITHOUT_EXEC` | Error | END-EXEC ohne EXEC |
| `DATA_ENTRY_MISSING_PERIOD` | Error | Data-Entry ohne abschliessenden Punkt |
| `PROCEDURE_VERB_UNKNOWN` | Error | Unbekanntes Statement-Verb (mit Levenshtein-Suggestion) |
| `COBOL74_KEYWORD_REMOVED` | Error | In COBOL 85 entferntes Schluesselwort (TRANSFORM, EXAMINE, etc.) |
| `VSAM_RETURN_CODE_74_CHECK` | Warning | VSAM Status `'00'` statt `'00' THRU '09'` |
| `EXEC_MISSING_TYPE` | Error | EXEC ohne Typ |
| `EXEC_DLI_*` | Error/Warning | Diverse EXEC DLI Validierungen (Klauseln, USING, WHERE) |
| `EXEC_CICS_MISSING_COMMAND` | Error | EXEC CICS ohne Kommando |
| `DEAD_CODE` | Hint | Unreferenzierter Paragraph/Section |

### 2.4 Go-to-Definition
- **COPY-Books:** Cursor auf Copybook-Name → springt zur aufgeloesten Datei
- **Paragraphen/Sections:** Cursor auf Name in `PERFORM`, `GO TO` etc. → springt zur Definition
- **Data-Names:** Cursor auf Data-Name → springt zur DATA DIVISION Definition
- Nutzt COPY-expandierten Text (Symbole aus Copybooks werden gefunden)

### 2.5 Hover-Informationen
- **Paragraphen:** Name und Definitionszeile
- **Data-Names:** Level, PIC, USAGE und Definitionszeile (mehrere Definitionen moeglich)
- **COPY-Books:** Aufgeloester Dateipfad oder "Nicht gefunden"

### 2.6 Auto-Completion
- **PROCEDURE DIVISION:** COBOL-Verben, Paragraphen, Sections, Data-Names
- **DATA DIVISION:** Data-Clauses (PIC, VALUE, USAGE, OCCURS, etc.)
- **Nach COPY:** Copybook-Dateinamen aus Suchdirs
- **Allgemein:** Division- und Section-Keywords
- Nutzt COPY-expandierten Index fuer Symbole aus Copybooks

### 2.7 Find All References
- Findet alle Vorkommen von Paragraphen, Sections und Data-Names
- Nutzt COPY-expandierten Text fuer Cross-File-Referenzen
- Definition-Site optional einschliessbar

### 2.8 Rename
- Umbennung von Paragraphen, Sections und Data-Names ueber alle Vorkommen
- Validierung: Gueltiger COBOL-Identifier (1-30 Zeichen, Start mit Buchstabe, kein End-Hyphen)
- Automatische Konvertierung in Grossbuchstaben
- COPY-expandierter Text wird beruecksichtigt

### 2.9 Dead-Code-Erkennung
- Identifiziert unreferenzierte Paragraphen und Sections
- Entry-Paragraph (erster Paragraph) wird ausgenommen
- Hint-Severity (erscheint als verblasster Text)

### 2.10 Document Symbols (Outline)
- PROGRAM-ID, DIVISIONs, SECTIONs, Paragraphen
- Hierarchische Verschachtelung

### 2.11 COPY-Expansion
- Rekursive Copybook-Aufloesung (max. 20 Ebenen)
- Suchpfade: Dokumentverzeichnis → Workspace-Root → konfigurierte `copybookPaths`
- REPLACING-Support (word, pseudo-text, string)
- Segment-Mapping fuer praezise Fehlerlokalisierung in Copybooks
- File-Watcher loesat Re-Validation bei Copybook-Aenderungen aus

### 2.12 Einstellungen

| Setting | Typ | Default | Beschreibung |
|---|---|---|---|
| `cobol85.warnings.blockClosedByPeriod` | `boolean` | `true` | Warnung bei IF/EVALUATE-Block durch `.` geschlossen |
| `cobol85.copybookPaths` | `string[]` | `["copybooks", "COPYBOOKS"]` | Suchverzeichnisse fuer Copybooks (relativ oder absolut) |

---

## 3. Behobene Bugs (Phase 1-4)

Die folgenden Bugs wurden in frueheren Iterationen identifiziert und vollstaendig behoben:

| Bug | Schwere | Beschreibung | Status |
|---|---|---|---|
| BUG-1 | KRITISCH | `context.subscriptions.push` ohne Argument (Memory Leak) | ✅ Behoben |
| BUG-2 | WICHTIG | `language-configuration.json` wurde nie geladen | ✅ Behoben |
| BUG-3 | WICHTIG | Tree-sitter Init-Failure permanent gecacht | ✅ Behoben (Retry) |
| BUG-4 | MITTEL | Diagnostik-Flackern bei Multi-File (globales Published-Set) | ✅ Behoben (per-Doc) |
| BUG-5 | KOSMETISCH | UTF-8 Mojibake in Diagnose-Messages | ✅ Behoben (ASCII) |
| BUG-6 | MITTEL | `collectCopyStatement` bei Nicht-Continuation-Zeilen | ✅ Behoben |
| BUG-7 | GERING | Streundes `S` in `tsconfig.json` | ✅ Behoben |
| BUG-8 | GERING | Continuation nur fuer `"` geprueft, nicht `'` | ✅ Behoben |

---

## 4. Aktuell offene Bugs

### BUG-9: Hover nutzte keinen COPY-expandierten Index — ✅ BEHOBEN

**Datei:** `server/src/server.ts` / `server/src/hover.ts`

`buildHover()` akzeptiert jetzt einen optionalen `preIndex`-Parameter. Der `onHover`-Handler in `server.ts` fuehrt die COPY-Expansion durch und uebergibt den COPY-expandierten Index. Copybook-Symbole zeigen jetzt Hover-Info.

### BUG-10: Semantic Tokens nutzten keinen COPY-expandierten Index — ✅ BEHOBEN

**Datei:** `server/src/server.ts`

Der `onSemanticTokens`-Handler fuehrt jetzt die COPY-Expansion durch und uebergibt `preIndex` an `buildSemanticTokens()`. Data-Names, Paragraphen und Sections aus Copybooks werden korrekt als `variable`/`function`/`namespace` klassifiziert.

### BUG-11: Dead-Code False Positives bei PERFORM ... THRU — ✅ BEHOBEN

**Datei:** `server/src/dead-code.ts`

Die Dead-Code-Analyse erkennt jetzt `PERFORM para-1 THRU/THROUGH para-2` und markiert alle Paragraphen **zwischen** `para-1` und `para-2` als referenziert. Neue Funktion `findPerformThruRanges()` und `collectThruReferencedParagraphs()` in `dead-code.ts`. 2 neue Tests verifizieren das Verhalten.

### BUG-12: Document-Symbol-Ranges deckten keine Kinder ab — ✅ BEHOBEN

**Datei:** `server/src/symbols.ts`

```typescript
function makeSymbol(...): DocumentSymbol {
  const r = Range.create(startLine, startChar, endLine, endChar);
  return { name, kind, range: r, selectionRange: r, children: [] };
}
```

Laut LSP-Spezifikation soll `range` den **gesamten** Bereich des Symbols einschliesslich aller Kinder umfassen, waehrend `selectionRange` nur den Label-Bereich markiert. Neue Funktion `fixSymbolRanges()` in `symbols.ts` berechnet nach dem Aufbau des Symbol-Baums die korrekten `range`-Werte: Jedes Symbol reicht von seiner Definitionszeile bis zum Start des naechsten Geschwister-Symbols (oder Dokumentende). 10 neue Tests verifizieren das Verhalten.

### BUG-13: language-configuration.json `lineComment: "*"` — ✅ BEHOBEN

**Datei:** `language-configuration.json`

```json
"comments": {
  "lineComment": "*"
}
```

`lineComment` wurde entfernt. Der Custom-Command `toggleFixedComment` bleibt fuer `Ctrl+/` zustaendig und steuert korrekt Spalte 7. Ohne `lineComment`-Eintrag kann VS Code nicht versehentlich `*` an der falschen Stelle einfuegen.

### BUG-14: Redundante text.split("\n") in Lint-Funktionen — ✅ BEHOBEN

**Datei:** `server/src/lint.ts`

`lintPreprocessed()` splittet den Text jetzt einmalig mit `text.split("\n")` und uebergibt das Ergebnis an `lintDataDivisionEntryPeriods()`, `lintProcedureVerbTypos()` und `lintVsamStatusCobol85Checks()`. Funktionssignaturen akzeptieren jetzt `lines: string[]` statt `text: string`.

---

## 5. Verbesserungsmoeglichkeiten

### 5.1 Hohe Prioritaet

_Alle V-26 bis V-31 wurden in Phase 5 umgesetzt._

### 5.2 Mittlere Prioritaet

| # | Verbesserung | Aufwand | Begruendung |
|---|---|---|---|
| V-14 | **Konfigurierbare Diagnostik-Sprache** | 4-8 Std | Deutsch/Englisch per Setting. Alle Messages sind aktuell Deutsch. |
| V-21 | **CodeLens** fuer Paragraphen-Referenzzaehlung | 4-8 Std | "3 references" ueber Paragraphen-Label. Sehr nuetzlich fuer Navigation. |
| V-22 | **CodeAction: Tab→Space-Konvertierung** | 2-4 Std | Quick-Fix statt nur Warnung bei TABs. |
| V-23 | **language-configuration.json verbessern** | 2 Std | `lineComment` entfernen/aendern; bessere Folding-Regeln. |
| V-25 | **EXEC SQL Validierung** | 4-8 Std | Aktuell nur DLI und CICS. SQL-Statements sind nicht validiert. |
| V-32 | **Qualified Name Resolution** | 4-8 Std | `WS-X OF WS-RECORD` Syntax in Go-to-Definition und References. |
| V-33 | **88-Level Condition-Names** | 2-4 Std | Aktuell nur in Definition-Index, keine spezielle Hover/Completion-Behandlung. |
| V-34 | **Inline-Paragraph-Labels in Definition-Index** | 2-4 Std | `MY-PARA. MOVE A TO B.` — Paragraph-Name wird nicht als Definition erkannt. |
| V-15 | **Free-Format-Unterstuetzung** | 8-16 Std | Vorbereitung fuer `>>SOURCE FORMAT IS FREE`. |

### 5.3 Niedrige Prioritaet / Nice-to-have

| # | Verbesserung | Aufwand | Begruendung |
|---|---|---|---|
| V-35 | **Binary Search fuer Segment-Mapping** | 2-4 Std | `mapGenPoint`/`mapOffset` lineare Suche → Binary Search fuer Perf bei grossen Dateien. |
| V-36 | **Magic Numbers als Konstanten** | 1-2 Std | `72`, `80`, `7`, `6` ueberall hardcodiert → `FIXED_SEQ_END`, `FIXED_LANG_END`, etc. |
| V-37 | **Scope-aware Rename** | 4-8 Std | Rename sollte pruefen, ob Spalte-72-Grenze nach Umbenennung ueberschritten wird. |
| V-38 | **Group-Level Hover** | 4-8 Std | Hierarchische Anzeige der Unterfelder bei Hover auf Group-Items. |
| V-39 | **Cross-Compilation-Test (E2E)** | 8-16 Std | VS Code Extension Host Tests fuer das Zusammenspiel Client ↔ Server. |
| V-40 | **Workspace-weites Symbol-Scanning** | 8-16 Std | LSP `workspaceSymbolProvider`: Suche ueber alle `.cob`-Dateien im Workspace. |
| V-41 | **Diagnostik-Debouncing pro Copybook** | 4-8 Std | Wenn mehrere Dokumente dasselbe Copybook referenzieren, Diagnostiken zusammenfuehren. |
| V-42 | **Automatische Spalten-Ruler** | 1-2 Std | VS Code `editor.rulers` per languageId-Default auf `[6, 7, 72, 80]` setzen. |

---

## 6. Test-Abdeckung

### 6.1 Uebersicht

| Testdatei | Tests | Abdeckung |
|---|---|---|
| `utils.test.ts` | 20 | Fixed-Format-Helfer, Separator-Period, Levenshtein, sliceLines |
| `preprocessor.test.ts` | 20 | Tokenizer, COPY-Parsing, REPLACING, Name-Normalisierung |
| `lint.test.ts` | 16 | Token-Scanning, EXEC-Ranges, Verb-Suggestions, IF/END-IF |
| `parser-bridge.test.ts` | 14 | UTF-8/16-Konvertierung, Tree-Edit, Error-Filtering |
| `definition.test.ts` | 12 | Word-Extraction, Index-Aufbau, Copybook-Mapping |
| `references.test.ts` | 12 | Occurrence-Finding, Definition-Site-Check, Symbol-Refs |
| `normalizer.test.ts` | 11 | Line-Normalisierung, ID→IDENTIFICATION, Listing-Control |
| `symbols.test.ts` | 10 | Symbol-Baum, Hierarchie, Range-Fixing, ID-Alias |
| `dead-code.test.ts` | 10 | Unreferenzierte Paragraphen/Sections, PERFORM THRU |
| `rename.test.ts` | 10 | Identifier-Validierung, Prepare/Perform Rename |
| `semantic-tokens.test.ts` | 10 | Token-Klassifizierung, Delta-Encoding |
| `completion.test.ts` | 9 | Context-aware Completions, Copybook-Index |
| `hover.test.ts` | 4 | Paragraph/Section/Data-Item Hover |
| **Gesamt** | **158** | |

### 6.2 Identifizierte Test-Luecken

| Bereich | Fehlende Tests |
|---|---|
| **Copybook-Integration** | Nur `definition.test.ts` testet Copybook-Mapping; Hover, Semantic Tokens fehlen |
| **EXEC SQL/CICS** | Nur DLI in Lint getestet; SQL/CICS nicht in Hover/Completion |
| **88-Level Conditions** | Keine Tests ueber alle Module hinweg |
| **Multi-Programm** | Keine Tests fuer verschachtelte PROGRAM-IDs |
| **Fehlerhafte Eingaben** | Alle Test-Inputs sind wohlgeformt; Stress-Tests mit kaputtem COBOL fehlen |
| **`collectTreeSitterErrors`** | Importiert aber nie getestet |

---

## 7. Code-Qualitaets-Beobachtungen

### 7.1 Positiv

- **Modulare Architektur:** Server.ts von ~3300 auf ~995 Zeilen reduziert; klare Modul-Grenzen
- **Segment-Mapping:** Preprocessor-Mapping ist sehr durchdacht — Fehler zeigen korrekt auf Copybooks
- **Scheduling:** Debounce-/Epoch-Logik ist robust gegen Race Conditions und Stale Results
- **EXEC DLI:** Ueberraschend detaillierte Validierung (WHERE, FIELDLENGTH, Clause-Ordering)
- **Levenshtein-Suggestions:** Nutzerfreundlich bei Verb-Tippfehlern
- **Inkrementelles Parsing:** `computeTreeEdit` ermoeglicht Tree-sitter Incremental Parse
- **Gute Test-Grundlage:** 158 Tests mit Vitest, vernuenftige Abdeckung aller Module
- **Konfigurierbarkeit:** Copybook-Pfade und Warnings sind einstellbar

### 7.2 Verbesserungsbedarf

- **Sprache:** Mischung aus Deutsch und Englisch in Kommentaren und Fehlermeldungen
- **Magic Numbers:** Werte wie `72`, `80`, `7`, `6` hardcodiert statt benannte Konstanten
- **Lineare Segment-Suche:** `mapGenPoint`/`mapOffset` — bei grossen Dateien besser Binary Search

---

## 8. Umsetzungsplan

### Phase 5: Bugfixes & Konsistenz — ABGESCHLOSSEN

- [x] **BUG-9 / V-26** Hover nutzt jetzt COPY-expandierten Index. `buildHover()` akzeptiert optionalen `preIndex`; `onHover`-Handler preprocessiert.
- [x] **BUG-10 / V-27** Semantic Tokens nutzt jetzt COPY-expandierten Index. `onSemanticTokens`-Handler preprocessiert.
- [x] **BUG-11 / V-28** PERFORM THRU in Dead-Code-Analyse. `findPerformThruRanges()` erkennt THRU/THROUGH-Muster; `collectThruReferencedParagraphs()` markiert Zwischen-Paragraphen als referenziert. 2 neue Tests.
- [x] **BUG-12 / V-29** Document-Symbol-Ranges korrigiert. `fixSymbolRanges()` setzt `range` auf den gesamten Symbol-Bereich bis zum naechsten Geschwister. `selectionRange` bleibt der Label-Bereich.
- [x] **BUG-13 / V-23** `lineComment: "*"` aus `language-configuration.json` entfernt. Custom-Command `toggleFixedComment` bleibt fuer `Ctrl+/`.
- [x] **V-30** `symbols.test.ts` erstellt: 10 Tests fuer Outline-Symbol-Baum, Hierarchie, Range-Berechnung, ID-Alias.
- [x] **BUG-14 / V-31** Lint-Performance: `text.split("\n")` wird einmalig in `lintPreprocessed()` aufgerufen und als `lines: string[]` an Sub-Funktionen weitergegeben.

### Phase 6: Feature-Ausbau (1-2 Wochen)

| # | Aufgabe | Aufwand | V-Ref |
|---|---|---|---|
| 8 | CodeLens fuer Referenzzaehlung | 4-8 Std | V-21 |
| 9 | CodeAction: Tab→Space | 2-4 Std | V-22 |
| 10 | EXEC SQL Validierung | 4-8 Std | V-25 |
| 11 | Qualified Name Resolution (`OF`/`IN`) | 4-8 Std | V-32 |
| 12 | 88-Level Condition-Names Sonderbehandlung | 2-4 Std | V-33 |
| 13 | Automatische Spalten-Ruler | 1 Std | V-42 |

### Phase 7: Fortgeschritten (fortlaufend)

| # | Aufgabe | Aufwand | V-Ref |
|---|---|---|---|
| 14 | i18n: Deutsch/Englisch umschaltbar | 4-8 Std | V-14 |
| 15 | Free-Format-Vorbereitung | 8-16 Std | V-15 |
| 16 | Binary Search fuer Segment-Mapping | 2-4 Std | V-35 |
| 17 | Workspace-weites Symbol-Scanning | 8-16 Std | V-40 |
| 18 | E2E-Tests mit VS Code Extension Host | 8-16 Std | V-39 |

---

## 9. Abgeschlossene Phasen (Changelog)

### Phase 1: Quick Wins & Bugfixes — ABGESCHLOSSEN

- [x] **BUG-1** `context.subscriptions.push(disposable)` ist umgesetzt.
- [x] **BUG-2** `language-configuration.json` ist in `package.json` angebunden.
- [x] **BUG-3** Tree-sitter Init-Retry ist umgesetzt (`tsInitPromise = null` im Fehlerfall).
- [x] **BUG-5** Mojibake vollstaendig behoben. Alle fehlerhaften Zeichen durch saubere ASCII-Strings ersetzt.
- [x] **BUG-7** Das stray `S` in `tsconfig.json` ist entfernt.
- [x] **BUG-8** Continuation-Check behandelt jetzt auch Single-Quotes.

### Phase 2: Stabilisierung — ABGESCHLOSSEN

- [x] **BUG-6** `collectCopyStatement` unterstuetzt mehrzeilige COPY-Statements ohne Continuation-Indicator.
- [x] **BUG-4** `publishedUrisByDoc: Map<string, Set<string>>` statt globalem Set.
- [x] **V-12** `server.ts` in Module aufgeteilt (types, utils, normalizer, preprocessor, lint, symbols, parser-bridge).
- [x] **V-20** Unit-Test-Grundgeruest (Vitest) vorhanden.
- [x] **V-13** `cobol85.copybookPaths` als konfigurierbares Setting.

### Phase 3: Kernfunktionen — ABGESCHLOSSEN

- [x] **V-10** Go-to-Definition fuer COPY-Books (`definition.ts`)
- [x] **V-7** Go-to-Definition fuer Paragraphen/Sections
- [x] **V-9** Hover-Informationen (`hover.ts`)
- [x] **V-8** Go-to-Definition fuer Data-Names
- [x] **V-11** Context-aware Completion (`completion.ts`)

### Phase 4: Ausbau — ABGESCHLOSSEN

- [x] **V-16** References / Find All Usages (`references.ts`, 12 Tests)
- [x] **V-19** Semantic Tokens (`semantic-tokens.ts`, 10 Tests)
- [x] **V-17** Rename (`rename.ts`, 10 Tests)
- [x] **V-18** Dead-Code-Erkennung (`dead-code.ts`, 8 Tests)

### Technischer Stand

- `npm run compile` laeuft fehlerfrei.
- `npm run test` laeuft fehlerfrei (13 Test-Dateien, 158 Tests bestanden).
