# DL/I CALL und EXEC DLI Syntax-Spezifikation (Parser/Linter)

Stand: 2026-02-27

Ziel: Eine umsetzbare, eindeutige Spezifikation fuer Parser- und Linter-Erweiterungen rund um DL/I in COBOL (mit Referenz auf PL/I/Assembler dort, wo die IBM-Quellen dies explizit vorgeben).

## 1. Quellen und Prioritaet

Normative Quellen:
- `specs/DLI/fhxria00.pdf` (DL/I DOS/VS Application Programming: CALL and RQDLI Interfaces)
- `specs/DLI/fhxhlp00.pdf` (DL/I DOS/VS Application Programming: High Level Programming Interface)
- `specs/DLI/x2451034.pdf` (Reference Summary: CALL Programming Interface)
- `specs/DLI/x2451202.pdf` (Reference Summary: High Level Programming Interface)

Prioritaet bei Konflikten:
1. Langform-Handbuecher (`fhxria00`, `fhxhlp00`)
2. Reference Summaries (`x2451034`, `x2451202`) als kompakte Bestaetigung/Tabellenquelle

## 2. Begriffe

- `CALL DLI` = klassische DL/I Call-Schnittstelle (`CBLTDLI` / `PLITDLI` / `CALLDLI`)
- `EXEC DLI` = High Level Programming Interface (HLPI), translator-basiert
- `SSA` = Segment Search Argument
- `PCB` = Program Communication Block

## 3. Allgemeine Lexik-Regeln

- Keywords sind case-insensitive.
- Sprachen unterscheiden sich in Delimitern:
  - COBOL HLPI: `END-EXEC` als Command-Delimiter.
  - PL/I HLPI: `;` als Delimiter.
- In dieser Spezifikation bedeuten:
  - `name`: Segment-/PSB-/Feldname nach IBM-Regeln
  - `ref`: Name einer Hostvariablen
  - `exp`: Host-Ausdruck (meist positive Integer), ausser `ID(exp)` bei CHKP (8-Byte char)

## 4. Teil A: CALL DLI (Low Level Interface)

### 4.1 COBOL CALL-Formen

Normativ aus `fhxria00` und `x2451034`.

```ebnf
CobolDliCallGeneral =
  "CALL" "'CBLTDLI'" "USING"
  [ parm_count "," ]
  call_function "," db_pcb_name "," io_area
  [ "," ssa { "," ssa } ] ;

CobolDliCallOnlineScheduling =
  "CALL" "'CBLTDLI'" "USING"
  [ parm_count "," ]
  call_function
  [ "," psbname [ "," uibparm ] ] ;
```

Hinweise:
- In der Summary ist die SSA-Liste explizit bis `ssa15` gezeigt.
- `parm-count` ist fuer COBOL optional; wenn vorhanden zaehlt er die nachfolgenden Parameter (ohne sich selbst).
- Summary nennt bis zu 18 moegliche Werte fuer `parm-count`.

### 4.2 PL/I CALL-Formen

```ebnf
PliDliCallGeneral =
  "CALL" "PLITDLI" "("
  parm_count "," call_function "," db_pcb_name "," io_area
  [ "," ssa { "," ssa } ]
  ")" ";" ;

PliDliCallOnlineScheduling =
  "CALL" "PLITDLI" "("
  parm_count "," call_function
  [ "," psbname [ "," uibparm ] ]
  ")" ";" ;
```

### 4.3 Assembler CALLDLI (fuer Vollstaendigkeit)

IBM gibt `CALLDLI ASMTDLI` und alternativ `MF=(E,...)` an; fuer den COBOL-Linter meist nicht direkt relevant, aber wichtig als Quellenkontext.

### 4.4 Zulaessige `call-function` Codes (CALL Interface)

Normativ (x2451034 + fhxria00):
- `GU` (Get Unique)
- `GHU` (Get Hold Unique)
- `GN` (Get Next)
- `GHN` (Get Hold Next)
- `GNP` (Get Next Within Parent)
- `GHNP` (Get Hold Next Within Parent)
- `DLET` (Delete)
- `REPL` (Replace)
- `ISRT` (Insert)
- `CHKP` (Checkpoint)
- `PCB` (Scheduling Call, online)
- `TERM` (Termination, online)

### 4.5 CALL-Parametersemantik

- `db-pcb-name`: PCB-Maske / PCB-Adresse.
- `io-area`: Segment-I/O Bereich.
- `ssa...`: ein oder mehrere Segment Search Arguments.
- `psbname`: PSB-Name (online scheduling).
- `uibparm`: Fullword-Adresse auf User Interface Block (UIB).

### 4.6 SSA-Syntax (CALL Interface)

#### 4.6.1 Unqualified SSA

```ebnf
UnqualifiedSSA =
  SegmentName8 Delimiter ;

Delimiter = " " | "(" ;   (* "(": Beginn qualified form *)
```

IBM-Format: 8-Byte Segmentname, bei unqualified SSA in Pos. 9 Blank.

#### 4.6.2 Qualified SSA

```ebnf
QualifiedSSA =
  SegmentName8 [ "*" CommandCodes ] "("
    Qualification { BooleanOp Qualification }
  ")" ;

Qualification =
  FieldName8 RelOp ComparativeValue ;

RelOp = "=" | "=>" | ">=" | "=<" | "<=" | ">" | "<" | "NOT=" ;

BooleanOp = "&" | "*" | "+" | "|" ;
```

Hinweise:
- AND entspricht `&` oder `*`.
- OR entspricht `+` oder `|`.
- Ohne Command-Codes muss nach Segmentname in Pos. 9 `(` oder Blank kommen.

### 4.7 SSA-Nutzung je Funktion (CALL Interface)

Aus `x2451034` (SSA Usage):

| Function | Must specify SSA | May specify SSA | Must not specify SSA |
|---|---|---|---|
| `GU`, `GHU` |  | X |  |
| `GN`, `GHN` |  | X |  |
| `GNP`, `GHNP` |  | X |  |
| `DLET` |  |  | X (Fn.1) |
| `REPL` |  |  | X (Fn.1) |
| `ISRT` | X (Fn.2) |  |  |

Fn.1: Unqualified SSA ist erlaubt, um Segment aus zuletzt per Path-Call geholtem Pfad zu selektieren.  
Fn.2: Mindestens eine unqualified SSA; bei Mischung aus qualified/unqualified muss die letzte SSA unqualified sein.

### 4.8 Command-Codes in SSA (CALL Interface)

Aus `fhxria00` Chapter 4 und `x2451034`:
- `D`, `F`, `L`, `N`, `Q`, `U`, `V` (plus null/dummy via `-` in bestimmten Formaten).

Anwendbarkeit (A=Applicable, D=Disregarded) laut Summary-Tabelle:

| Code | GU/GHU | GN/GHN | GNP/GHNP | DLET | REPL | ISRT | CHKP |
|---|---|---|---|---|---|---|---|
| `D` | A | A | A | D | D | A | D |
| `F` | D | A | A | D | D | A | D |
| `L` | A | A | A | D | D | A | D |
| `N` | D | D | D | D | A | D | D |
| `Q` | A | A | A | D | D | D | D |
| `U` | A | A | A | D | D | A | D |
| `V` | A | A | A | D | D | A | D |

Linter-Hinweis:
- Bei nicht anwendbarem Code laut IBM: ignoriert (kein Fehlerstatus allein dadurch).
- Optional als Warning modellierbar (`code ignored for function`).

## 5. Teil B: EXEC DLI (HLPI)

### 5.1 Trigger

```ebnf
DliTrigger = ("EXECUTE" | "EXEC") "DLI" ;
```

Nur diese Trigger-Kombination ist fuer HLPI reserviert.

### 5.2 Funktionen und Kurzformen

- `GET NEXT` / `GN`
- `GET NEXT IN PARENT` / `GNP`
- `GET UNIQUE` / `GU`
- `INSERT` / `ISRT`
- `REPLACE` / `REPL`
- `DELETE` / `DLET`
- `LOAD`
- `CHECKPOINT` / `CHKP`
- `SCHEDULE` / `SCHD`
- `TERMINATE` / `TERM`

### 5.3 Gemeinsame Bausteine (HLPI)

```ebnf
PcbOption = "USING" "PCB" "(" exp ")" ;

KeyFeedbackOption =
  "KEYFEEDBACK" "(" ref ")"
  [ "FEEDBACKLEN" "(" exp ")" ] ;

SegmentPrefix = [ "FIRST" | "LAST" ] [ "VARIABLE" ] ;

GetTransfer = "INTO" "(" ref ")" [ "LOCKED" ] [ "OFFSET" "(" exp ")" ] [ "SEGLENGTH" "(" exp ")" ] ;
PutTransfer = "FROM" "(" ref ")" [ "OFFSET" "(" exp ")" ] [ "SEGLENGTH" "(" exp ")" ] ;

WhereClause =
  "WHERE" "(" RelCond { ( "AND" | "OR" ) RelCond } ")"
  [ "FIELDLENGTH" "(" exp { "," exp } ")" ] ;

RelCond = name RelOpHLPI ref ;

RelOpHLPI = ">" | "<" | "=" | "NOT=" | "<=" | ">=" | "=<" | "NOT>" | "=>" | "NOT<" ;
```

IBM zeigt fuer die NOT-Formen in Teilen der Dokumentation EBCDIC-Notation (not-sign plus operator); fuer die Parser-Spezifikation werden diese als `NOT=`, `NOT>`, `NOT<` normalisiert.

### 5.4 Segmentstruktur (HLPI)

IBM-Modell:
- 0..14 Parent-Segmente
- 1 Object-Segment (Ausnahme: `GET NEXT`/`GNP` koennen ohne Segment auskommen)

```ebnf
ParentSegmentGet =
  SegmentPrefix "SEGMENT" "(" name ")"
  [ GetTransfer ]
  [ WhereClause ] ;

ObjectSegmentGet =
  SegmentPrefix "SEGMENT" "(" name ")"
  GetTransfer
  [ WhereClause ] ;

ParentSegmentPut =
  SegmentPrefix "SEGMENT" "(" name ")"
  [ PutTransfer ]
  [ WhereClause ] ;         (* WHERE nur dort erlaubt, wo command-spezifisch erlaubt *)

ObjectSegmentPut =
  SegmentPrefix "SEGMENT" "(" name ")"
  PutTransfer ;
```

### 5.5 Vollsyntax je HLPI-Kommando

#### 5.5.1 GET NEXT / GN

```ebnf
ExecDliGn =
  DliTrigger ( "GET NEXT" | "GN" )
  [ PcbOption ]
  [ KeyFeedbackOption ]
  [ ParentSegmentGet { ParentSegmentGet } ]
  [ ObjectSegmentGet ]
  EndExec ;
```

#### 5.5.2 GET NEXT IN PARENT / GNP

```ebnf
ExecDliGnp =
  DliTrigger ( "GET NEXT IN PARENT" | "GNP" )
  [ PcbOption ]
  [ KeyFeedbackOption ]
  [ ParentSegmentGet { ParentSegmentGet } ]
  [ ObjectSegmentGet ]
  EndExec ;
```

#### 5.5.3 GET UNIQUE / GU

```ebnf
ExecDliGu =
  DliTrigger ( "GET UNIQUE" | "GU" )
  [ PcbOption ]
  [ KeyFeedbackOption ]
  [ ParentSegmentGet { ParentSegmentGet } ]
  ObjectSegmentGet
  EndExec ;
```

#### 5.5.4 INSERT / ISRT

```ebnf
ExecDliIsrt =
  DliTrigger ( "INSERT" | "ISRT" )
  [ PcbOption ]
  [ ParentSegmentPut { ParentSegmentPut } ]
  ObjectSegmentPut
  EndExec ;
```

#### 5.5.5 REPLACE / REPL

```ebnf
ExecDliRepl =
  DliTrigger ( "REPLACE" | "REPL" )
  [ PcbOption ]
  [ ( [ "VARIABLE" ] "SEGMENT" "(" name ")" PutTransfer ) { ( [ "VARIABLE" ] "SEGMENT" "(" name ")" PutTransfer ) } ]
  EndExec ;
```

#### 5.5.6 DELETE / DLET

```ebnf
ExecDliDlet =
  DliTrigger ( "DELETE" | "DLET" )
  [ PcbOption ]
  [ "VARIABLE" ] "SEGMENT" "(" name ")"
  "FROM" "(" ref ")" [ "SEGLENGTH" "(" exp ")" ]
  EndExec ;
```

#### 5.5.7 LOAD

```ebnf
ExecDliLoad =
  DliTrigger "LOAD"
  [ PcbOption ]
  [ "VARIABLE" ] "SEGMENT" "(" name_or_load_var ")"
  "FROM" "(" ref ")" [ "SEGLENGTH" "(" exp ")" ]
  EndExec ;
```

`name_or_load_var`:
- normal: `name`
- nur fuer LOAD: `SEGMENT((VARNAME))`

#### 5.5.8 CHECKPOINT / CHKP

```ebnf
ExecDliChkp =
  DliTrigger ( "CHECKPOINT" | "CHKP" )
  "ID" "(" exp ")"
  EndExec ;
```

#### 5.5.9 SCHEDULE / SCHD

```ebnf
ExecDliSchd =
  DliTrigger ( "SCHEDULE" | "SCHD" )
  "PSB" "(" name ")"
  EndExec ;
```

#### 5.5.10 TERMINATE / TERM

```ebnf
ExecDliTerm =
  DliTrigger ( "TERMINATE" | "TERM" )
  EndExec ;
```

#### 5.5.11 Delimiter

```ebnf
EndExec = "END-EXEC" | ";" ;
```

Parser fuer COBOL: `END-EXEC` als Ende erwarten; optionaler Satzpunkt nach `END-EXEC` ist COBOL-Statementsyntax, nicht HLPI-Kernsyntax.

## 6. Klausel-Regelmatrix (HLPI, parser/linter-relevant)

| Function | USING PCB | KEYFEEDBACK/FEEDBACKLEN | SEGMENT | INTO | FROM | WHERE | FIELDLENGTH | LOCKED | OFFSET | SEGLENGTH | PSB | ID |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| `GN` | opt | opt (nur GET) | opt | opt (bei Segment) | nein | opt | opt* | opt** | opt*** | opt**** | nein | nein |
| `GNP` | opt | opt (nur GET) | opt | opt (bei Segment) | nein | opt | opt* | opt** | opt*** | opt**** | nein | nein |
| `GU` | opt | opt (nur GET) | obj pflicht | obj pflicht | nein | opt | opt* | opt** | opt*** | opt**** | nein | nein |
| `ISRT` | opt | nein | obj pflicht | nein | obj pflicht | opt***** | opt* | nein | opt*** | opt**** | nein | nein |
| `REPL` | opt | nein | optionaler Segmentblock | nein | pflicht in jedem Segmentblock | nein | nein | nein | opt*** | opt**** | nein | nein |
| `DLET` | opt | nein | pflicht | nein | pflicht | nein | nein | nein | nein | opt**** | nein | nein |
| `LOAD` | opt | nein | pflicht | nein | pflicht | nein | nein | nein | nein | opt**** | nein | nein |
| `CHKP` | nein | nein | nein | nein | nein | nein | nein | nein | nein | nein | nein | pflicht |
| `SCHD` | nein | nein | nein | nein | nein | nein | nein | nein | nein | nein | pflicht | nein |
| `TERM` | nein | nein | nein | nein | nein | nein | nein | nein | nein | nein | nein | nein |

\* In COBOL bei `WHERE` erforderlich; in PL/I optional.  
\** `LOCKED` nur bei GET-Funktionen und nur mit assoziiertem `INTO`.  
\*** `OFFSET` nur bei Concatenated Segment + `VARIABLE` + (`INTO` oder `FROM`).  
\**** In COBOL bei `INTO`/`FROM` erforderlich; in PL/I optional.  
\***** `WHERE` bei INSERT fuer Segment-Selection laut HLPI-Syntax erlaubt; fuers selbe Parent-Segment darf nicht gleichzeitig `FROM` und `WHERE` gesetzt sein.

## 7. Zusaetzliche semantische Regeln fuer Linter

- `VARIABLE` nie ohne zugehoeriges `INTO`/`FROM`.
- Parent-Segmente muessen in hierarchischer Reihenfolge codiert sein.
- Bei angegebenem Parent muss auch Object-Segment vorhanden sein.
- `FIRST` und `LAST` sind gegenseitig exklusiv.
- `FIRST` fuer `GN`, `GNP`, `ISRT`; `LAST` fuer `GU`, `GN`, `GNP`, `ISRT`.
- `SCHEDULE` und `TERMINATE` nur online.
- `LOAD` nur batch.
- `WHERE`:
  - max. 12 Relationen / 11 Boolean-Operatoren.
  - keine inneren Klammern innerhalb WHERE-Ausdruck.
  - `AND`/`OR` als separate Tokens (mit Trennzeichen).

## 8. Parser-Empfehlung (praktisch)

- Zwei getrennte Entry-Points:
  - `parseCallDli(...)` fuer `CALL 'CBLTDLI' ...`
  - `parseExecDli(...)` fuer `EXEC[UTE] DLI ... END-EXEC`
- Parser strikt fuer Struktur, tolerant fuer `exp`/`ref`/`name`-Inhalte (Hostausdruecke nicht voll ausparsen).
- Linter uebernimmt funktionsabhaengige Klauselvalidierung und Umgebungsregeln.

## 9. Quellenanker (schneller Einstieg)

- CALL-General/Online-Format und Parameterliste:
  - `x2451034.pdf` Panel 3 (CALL formats, parameters, function codes)
  - `fhxria00.pdf` Chapter 1 "DL/I Batch Program Call" und Chapter 3 online scheduling/termination calls
- SSA-Formate und Command-Codes:
  - `fhxria00.pdf` Chapter 1 (SSA basics), Chapter 4 (Command Codes)
  - `x2451034.pdf` Panels 4-7 (Qualified SSA, SSA usage, command code matrix)
- EXEC DLI Syntax:
  - `fhxhlp00.pdf` Chapter 2 Syntax chart + Chapter 4 command-specific syntax
  - `x2451202.pdf` Panels 2-8 (Syntax summary, options/functions)
