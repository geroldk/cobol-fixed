# EXEC CICS — Vollständiger Implementierungsplan

Erstellt: 2026-02-28  
Quelle: IBM CICS TS 6.x Application Programming Reference  

---

## 1. Übersicht

Ziel ist die vollständige Spezifikation und Implementierung aller **VSE-First Priority Group** EXEC CICS Kommandos im Linter (Option-Validierung). Es werden 29 neue `CicsCommandSpec`-Einträge hinzugefügt — plus Erweiterungen bestehender Einträge.

### 1.1 Status Quo (Phase 1 — abgeschlossen)

Bereits implementiert mit vollständigen Option-Specs:

| # | Kommando       | Kategorie         |
|---|----------------|--------------------|
| 1 | READ           | File Control       |
| 2 | READNEXT       | File Control       |
| 3 | READPREV       | File Control       |
| 4 | STARTBR        | File Control       |
| 5 | RESETBR        | File Control       |
| 6 | ENDBR          | File Control       |
| 7 | WRITE          | File Control       |
| 8 | REWRITE        | File Control       |
| 9 | DELETE         | File Control       |
|10 | UNLOCK         | File Control       |
|11 | LINK           | Program Control    |
|12 | XCTL           | Program Control    |
|13 | RETURN         | Program Control    |
|14 | START          | Interval Control   |
|15 | SEND MAP       | BMS                |
|16 | SEND TEXT      | BMS                |
|17 | SEND CONTROL   | BMS                |
|18 | RECEIVE MAP    | BMS                |

Plus: HANDLE CONDITION-Validierung, Multi-Word-Erkennung, Typo-Vorschläge.

### 1.2 Phase 2 — Neue Kommandos (dieses Ticket)

| # | Kommando            | Kategorie           | Priorität |
|---|---------------------|----------------------|-----------|
| 1 | ABEND               | Task Control         | HOCH      |
| 2 | ADDRESS             | Storage/System       | HOCH      |
| 3 | ASKTIME             | Interval Control     | HOCH      |
| 4 | ASSIGN              | System Info          | HOCH      |
| 5 | CANCEL              | Interval Control     | HOCH      |
| 6 | DELAY               | Interval Control     | HOCH      |
| 7 | FORMATTIME          | Interval Control     | HOCH      |
| 8 | GETMAIN             | Storage Control      | HOCH      |
| 9 | FREEMAIN            | Storage Control      | HOCH      |
|10 | LOAD                | Program Control      | HOCH      |
|11 | RELEASE             | Program Control      | HOCH      |
|12 | RETRIEVE            | Interval Control     | HOCH      |
|13 | SUSPEND             | Task Control         | HOCH      |
|14 | POST                | Task/Event Control   | MITTEL    |
|15 | ENQ                 | Task Control         | HOCH      |
|16 | DEQ                 | Task Control         | HOCH      |
|17 | READQ TD            | Queue (TD)           | HOCH      |
|18 | READQ TS            | Queue (TS)           | HOCH      |
|19 | WRITEQ TD           | Queue (TD)           | HOCH      |
|20 | WRITEQ TS           | Queue (TS)           | HOCH      |
|21 | DELETEQ TD          | Queue (TD)           | HOCH      |
|22 | DELETEQ TS          | Queue (TS)           | HOCH      |
|23 | HANDLE ABEND        | Exception Handling   | HOCH      |
|24 | HANDLE AID          | Exception Handling   | HOCH      |
|25 | IGNORE CONDITION    | Exception Handling   | HOCH      |
|26 | ISSUE ERASE         | Terminal Control     | MITTEL    |
|27 | ISSUE ERASEAUP      | Terminal Control     | MITTEL    |
|28 | SYNCPOINT           | Sync Control         | HOCH      |
|29 | SYNCPOINT ROLLBACK  | Sync Control         | HOCH      |
|30 | SEND PAGE           | BMS                  | MITTEL    |
|31 | PURGE MESSAGE       | BMS                  | MITTEL    |
|32 | PUSH HANDLE         | Exception Handling   | MITTEL    |
|33 | POP HANDLE          | Exception Handling   | MITTEL    |
|34 | WAIT EVENT          | Task/Event Control   | MITTEL    |
|35 | DUMP TRANSACTION    | Diagnostic           | MITTEL    |
|36 | ENTER TRACENUM      | Diagnostic           | MITTEL    |
|37 | WRITE OPERATOR      | Diagnostic           | MITTEL    |

---

## 2. Architektur der Implementierung

### 2.1 Dateien, die geändert werden

| Datei | Änderung |
|-------|----------|
| `server/src/lint.ts` | Neue `CicsCommandSpec`-Einträge in `CICS_COMMAND_SPECS`, neue Einträge in `CICS_ADDITIONAL_MULTIWORD_COMMANDS`, ggf. neue Conditions in `CICS_KNOWN_CONDITIONS` |
| `server/src/lint.test.ts` | Tests für alle neuen Kommando-Specs |
| `docs/EXEC_CICS_SYNTAX_SPEC.md` | Vollständige Syntax-Spezifikation (neue Datei) |

### 2.2 Kein Eingriff nötig in

- `server/src/completion.ts` — Completion-Support für CICS separat geplant
- `server/src/hover.ts` — Hover-Support für CICS separat geplant
- `cobol_fixed.tmLanguage.json` — TextMate-Grammar braucht keine Änderung (EXEC/END-EXEC bereits abgedeckt)

### 2.3 CicsCommandSpec-Schema

```typescript
type CicsCommandSpec = {
  allowedOptions: Set<string>;   // Alle erlaubten Optionen
  requiredAll?: string[];        // Alle diese müssen vorhanden sein
  requiredOneOf?: string[][];    // Von jeder Gruppe muss mindestens eine da sein
};
```

---

## 3. Detaillierte Option-Spezifikationen je Kommando

### 3.1 ABEND
```
Optionen: ABCODE, CANCEL, NODUMP
Pflicht: keine
```

### 3.2 ADDRESS
```
Optionen: ACEE, COMMAREA, CWA, EIB, TCTUA, TWA
Pflicht: keine
```

### 3.3 ASKTIME
```
Optionen: ABSTIME
Pflicht: keine
```

### 3.4 ASSIGN
```
Optionen: (ca. 100+ System-Abfrage-Felder, alle optional)
Pflicht: keine
Hinweis: Da ASSIGN ~100+ Optionen hat, wird eine umfangreiche allowedOptions-Liste angelegt.
```

### 3.5 CANCEL
```
Optionen: REQID, SYSID, TRANSID
Pflicht: keine
```

### 3.6 DELAY
```
Optionen: INTERVAL, TIME, FOR, HOURS, MINUTES, SECONDS, MILLISECS, UNTIL, REQID
Pflicht: keine
```

### 3.7 FORMATTIME
```
Optionen: ABSTIME, DATE, FULLDATE, DATEFORM, DATESEP, DATESTRING, STRINGZONE,
          DAYCOUNT, DAYOFMONTH, DAYOFWEEK, DDMMYY, DDMMYYYY, MILLISECONDS,
          MMDDYY, MMDDYYYY, MONTHOFYEAR, STRINGFORMAT, TIME, TIMESEP, YEAR,
          YYDDD, YYDDMM, YYMMDD, YYYYDDD, YYYYDDMM, YYYYMMDD
Pflicht: ABSTIME
```

### 3.8 GETMAIN
```
Optionen: SET, FLENGTH, LENGTH, BELOW, INITIMG, EXECUTABLE, SHARED, NOSUSPEND,
          USERDATAKEY, CICSDATAKEY
Pflicht: SET
PflichtOneOf: FLENGTH / LENGTH
```

### 3.9 FREEMAIN
```
Optionen: DATA, DATAPOINTER
Pflicht: keine
PflichtOneOf: DATA / DATAPOINTER
```

### 3.10 LOAD
```
Optionen: PROGRAM, SET, LENGTH, FLENGTH, ENTRY, HOLD
Pflicht: PROGRAM
```

### 3.11 RELEASE
```
Optionen: PROGRAM
Pflicht: PROGRAM
```

### 3.12 RETRIEVE
```
Optionen: INTO, SET, LENGTH, RTRANSID, RTERMID, QUEUE, WAIT
Pflicht: keine
PflichtOneOf: INTO / SET
```

### 3.13 SUSPEND
```
Optionen: (keine anwendungsspezifischen)
Pflicht: keine
```

### 3.14 POST
```
Optionen: INTERVAL, TIME, SET, REQID, AFTER, AT, HOURS, MINUTES, SECONDS
Pflicht: SET
```

### 3.15 ENQ
```
Optionen: RESOURCE, LENGTH, UOW, TASK, MAXLIFETIME, NOSUSPEND
Pflicht: RESOURCE
```

### 3.16 DEQ
```
Optionen: RESOURCE, LENGTH, UOW, TASK, MAXLIFETIME
Pflicht: RESOURCE
```

### 3.17 READQ TD
```
Optionen: QUEUE, INTO, SET, LENGTH
Pflicht: QUEUE
PflichtOneOf: INTO / SET
```

### 3.18 READQ TS
```
Optionen: QUEUE, INTO, SET, LENGTH, ITEM, NEXT, NUMITEMS, SYSID
Pflicht: QUEUE
PflichtOneOf: INTO / SET
```

### 3.19 WRITEQ TD
```
Optionen: QUEUE, FROM, LENGTH
Pflicht: QUEUE, FROM
```

### 3.20 WRITEQ TS
```
Optionen: QUEUE, FROM, LENGTH, ITEM, REWRITE, NOSUSPEND, MAIN, AUXILIARY, SYSID
Pflicht: QUEUE, FROM
```

### 3.21 DELETEQ TD
```
Optionen: QUEUE
Pflicht: QUEUE
```

### 3.22 DELETEQ TS
```
Optionen: QUEUE, SYSID
Pflicht: QUEUE
```

### 3.23 HANDLE ABEND
```
Optionen: PROGRAM, LABEL, CANCEL, RESET
Pflicht: keine
PflichtOneOf: PROGRAM / LABEL / CANCEL / RESET
```

### 3.24 HANDLE AID
```
Optionen: ANYKEY, CLEAR, CLRPARTN, ENTER, LIGHTPEN, OPERID, TRIGGER,
          PA1, PA2, PA3, PF1–PF24
Pflicht: keine
Hinweis: Jeder AID-Name wird von einem Label gefolgt. Spezialbehandlung nötig.
```

### 3.25 IGNORE CONDITION
```
Optionen: (Condition-Namen aus CICS_KNOWN_CONDITIONS)
Pflicht: mindestens 1 Condition
Hinweis: Spezialbehandlung wie bei HANDLE CONDITION, aber ohne Labels.
```

### 3.26 ISSUE ERASE
```
Optionen: (keine — nur WAIT, ERASE/ERASEAUP ist der Befehl selbst)
Pflicht: keine
```

### 3.27 ISSUE ERASEAUP
```
Optionen: (keine)
Pflicht: keine
```

### 3.28 SYNCPOINT
```
Optionen: (keine — ROLLBACK ist separates Kommando)
Pflicht: keine
```

### 3.29 SYNCPOINT ROLLBACK
```
Optionen: (keine)
Pflicht: keine
```

### 3.30 SEND PAGE
```
Optionen: RELEASE, TRANSID, TRAILER, RETAIN, AUTOPAGE, OPERPURGE, LAST,
          NOAUTOPAGE, FMHPARM
Pflicht: keine
```

### 3.31 PURGE MESSAGE
```
Optionen: (keine)
Pflicht: keine
```

### 3.32 PUSH HANDLE
```
Optionen: (keine)
Pflicht: keine
```

### 3.33 POP HANDLE
```
Optionen: (keine)
Pflicht: keine
```

### 3.34 WAIT EVENT
```
Optionen: ECADDR
Pflicht: ECADDR
```

### 3.35 DUMP TRANSACTION
```
Optionen: DUMPCODE, FROM, LENGTH, FLENGTH, COMPLETE, TASK, STORAGE, PROGRAM,
          TERMINAL, TABLES, FCT, PCT, PPT, SIT, TCT
Pflicht: DUMPCODE
```

### 3.36 ENTER TRACENUM
```
Optionen: FROM, FROMLENGTH, RESOURCE, EXCEPTION
Pflicht: keine
```

### 3.37 WRITE OPERATOR
```
Optionen: TEXT, TEXTLENGTH, ROUTECODES, NUMROUTES, ACTION, CRITICAL, EVENTUAL,
          IMMEDIATE, REPLY, MAXLENGTH, TIMEOUT
Pflicht: TEXT
```

---

## 4. Neue CICS Conditions

Folgende Conditions sind noch nicht in `CICS_KNOWN_CONDITIONS`:

```
NOSTG, PGMIDERR, SUPPRESSED, TERMIDERR
```

→ Werden ergänzt.

---

## 5. Reihenfolge der Implementierung

1. ✅ Aktuellen Code und IBM-Quellen lesen
2. 📌 CICS Syntax-Spezifikationsdokument erstellen (`docs/EXEC_CICS_SYNTAX_SPEC.md`)
3. 📌 Neue `CicsCommandSpec`-Einträge in `lint.ts` hinzufügen (alle 37 Kommandos)
4. 📌 `CICS_ADDITIONAL_MULTIWORD_COMMANDS` durch Specs ersetzen (TD/TS, HANDLE*, IGNORE, ISSUE*, SYNCPOINT ROLLBACK → bekommen jetzt eigene Specs)
5. 📌 Neue Conditions in `CICS_KNOWN_CONDITIONS` ergänzen
6. 📌 Spezialbehandlung für HANDLE AID und IGNORE CONDITION
7. 📌 Tests hinzufügen (~30+ neue Tests)
8. 📌 Build + alle Tests ausführen

---

## 6. Test-Strategie

Jedes neue Kommando bekommt mindestens einen Test:
- **Positiv-Test**: Korrekter Aufruf ohne Diagnostik
- **Negativ-Test**: Fehlende Pflicht-Option → Diagnostik erwartet
- **Negativ-Test**: Nicht erlaubte Option → Diagnostik erwartet

Für Spezialbehandlungen (HANDLE AID, IGNORE CONDITION):
- Korrekte Condition-Liste
- Unbekannte Condition
- Leere Condition-Liste
