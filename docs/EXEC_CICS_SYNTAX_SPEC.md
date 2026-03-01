# EXEC CICS Syntax-Spezifikation

Erstellt: 2026-02-28  
Quelle: IBM CICS TS 6.x Application Programming Reference  
Verwendung: Linter-Option-Validierung für `cobol-fixed` VS Code Extension  

---

## Globale Hinweise

- **NOHANDLE**, **RESP(data-area)** und **RESP2(data-area)** können bei JEDEM EXEC CICS Kommando angegeben werden und werden in den Specs NICHT aufgeführt.
- Optionen ohne Klammer-Argument sind Flags (Boolean-Optionen).
- Optionen mit `(data-area)` oder `(name)` erwarten einen COBOL-Identifier oder ein Literal als Argument.

---

## 1. File Control

### 1.1 READ
```
EXEC CICS READ
  FILE(name)                     — Pflicht
  INTO(data-area) | SET(ptr-ref) — Eins von beiden Pflicht
  RIDFLD(data-area)
  KEYLENGTH(data-value)
  GENERIC
  GTEQ | EQUAL
  RBA | RRN | XRBA
  TOKEN(data-area)
  LENGTH(data-area)
  UPDATE
  NOSUSPEND
  CONSISTENT
  SYSID(systemname)
END-EXEC
Conditions: DUPKEY, FILENOTFOUND, ILLOGIC, INVREQ, IOERR, ISCINVREQ,
            LENGERR, NOTAUTH, NOTFND, NOTOPEN, RECORDBUSY, SYSIDERR
```

### 1.2 READNEXT
```
EXEC CICS READNEXT
  FILE(name)                     — Pflicht
  INTO(data-area) | SET(ptr-ref) — Eins von beiden Pflicht
  RIDFLD(data-area)
  KEYLENGTH(data-value)
  REQID(data-value)
  RBA | RRN | XRBA
  TOKEN(data-area)
  LENGTH(data-area)
  UPDATE
  NOSUSPEND
  SYSID(systemname)
END-EXEC
Conditions: DUPKEY, ENDFILE, FILENOTFOUND, ILLOGIC, INVREQ, IOERR,
            ISCINVREQ, LENGERR, NOTAUTH, NOTFND, NOTOPEN, RECORDBUSY, SYSIDERR
```

### 1.3 READPREV
```
EXEC CICS READPREV
  FILE(name)                     — Pflicht
  INTO(data-area) | SET(ptr-ref) — Eins von beiden Pflicht
  RIDFLD(data-area)
  KEYLENGTH(data-value)
  REQID(data-value)
  RBA | RRN | XRBA
  TOKEN(data-area)
  LENGTH(data-area)
  UPDATE
  NOSUSPEND
  SYSID(systemname)
END-EXEC
Conditions: DUPKEY, ENDFILE, FILENOTFOUND, ILLOGIC, INVREQ, IOERR,
            ISCINVREQ, LENGERR, NOTAUTH, NOTFND, NOTOPEN, RECORDBUSY, SYSIDERR
```

### 1.4 WRITE
```
EXEC CICS WRITE
  FILE(name)                     — Pflicht
  FROM(data-area)                — Pflicht
  RIDFLD(data-area)
  KEYLENGTH(data-value)
  RBA | RRN
  LENGTH(data-value)
  MASSINSERT
  NOSUSPEND
  SYSID(systemname)
END-EXEC
Conditions: DUPREC, FILENOTFOUND, ILLOGIC, INVREQ, IOERR, ISCINVREQ,
            LENGERR, NOSPACE, NOTAUTH, NOTOPEN, SYSIDERR
```

### 1.5 REWRITE
```
EXEC CICS REWRITE
  FILE(name)                     — Pflicht
  FROM(data-area)                — Pflicht
  TOKEN(data-area)
  LENGTH(data-value)
  NOSUSPEND
  SYSID(systemname)
END-EXEC
Conditions: DUPREC, FILENOTFOUND, ILLOGIC, INVREQ, IOERR, LENGERR,
            NOSPACE, NOTAUTH, NOTOPEN, SYSIDERR
```

### 1.6 DELETE
```
EXEC CICS DELETE
  FILE(name)                     — Pflicht
  RIDFLD(data-area) | RBA | RRN | XRBA | TOKEN — Eins erforderlich
  KEYLENGTH(data-value)
  GENERIC
  NOSUSPEND
  SYSID(systemname)
END-EXEC
Conditions: FILENOTFOUND, ILLOGIC, INVREQ, IOERR, ISCINVREQ, NOTAUTH,
            NOTFND, NOTOPEN, SYSIDERR
```

### 1.7 STARTBR
```
EXEC CICS STARTBR
  FILE(name)                     — Pflicht
  RIDFLD(data-area)
  KEYLENGTH(data-value)
  GENERIC
  GTEQ | EQUAL
  RBA | RRN | XRBA
  REQID(data-value)
  DEBKEY | DEBREC
  NOSUSPEND
  SYSID(systemname)
END-EXEC
Conditions: FILENOTFOUND, ILLOGIC, INVREQ, IOERR, ISCINVREQ, NOTAUTH,
            NOTFND, NOTOPEN, SYSIDERR
```

### 1.8 RESETBR
```
EXEC CICS RESETBR
  FILE(name)                     — Pflicht
  RIDFLD(data-area)
  KEYLENGTH(data-value)
  GENERIC
  GTEQ | EQUAL
  RBA | RRN | XRBA
  REQID(data-value)
  NOSUSPEND
  SYSID(systemname)
END-EXEC
Conditions: FILENOTFOUND, ILLOGIC, INVREQ, IOERR, ISCINVREQ, NOTAUTH,
            NOTFND, NOTOPEN, SYSIDERR
```

### 1.9 ENDBR
```
EXEC CICS ENDBR
  FILE(name)                     — Pflicht
  REQID(data-value)
  SYSID(systemname)
END-EXEC
Conditions: FILENOTFOUND, ILLOGIC, INVREQ, ISCINVREQ, NOTAUTH, SYSIDERR
```

### 1.10 UNLOCK
```
EXEC CICS UNLOCK
  FILE(name)                     — Pflicht
  TOKEN(data-area)
  SYSID(systemname)
END-EXEC
Conditions: FILENOTFOUND, INVREQ, IOERR, NOTAUTH
```

---

## 2. Program Control

### 2.1 LINK
```
EXEC CICS LINK
  PROGRAM(name)                  — Pflicht
  COMMAREA(data-area)
  LENGTH(data-value)
  DATALENGTH(data-value)
  CHANNEL(name)
  INPUTMSG(data-area)
  INPUTMSGLEN(data-value)
  SYNCONRETURN
  SYSID(systemname)
END-EXEC
Conditions: INVREQ, LENGERR, NOTAUTH, PGMIDERR, ROLLLEDBACK, SYSIDERR, TERMERR
```

### 2.2 XCTL
```
EXEC CICS XCTL
  PROGRAM(name)                  — Pflicht
  COMMAREA(data-area)
  LENGTH(data-value)
  CHANNEL(name)
  SYSID(systemname)
END-EXEC
Conditions: INVREQ, LENGERR, NOTAUTH, PGMIDERR
```

### 2.3 RETURN
```
EXEC CICS RETURN
  TRANSID(name)
  COMMAREA(data-area)
  LENGTH(data-value)
  CHANNEL(name)
  IMMEDIATE
  INPUTMSG(data-area)
  INPUTMSGLEN(data-value)
END-EXEC
Conditions: INVREQ, LENGERR
```

### 2.4 LOAD
```
EXEC CICS LOAD
  PROGRAM(name)                  — Pflicht
  SET(ptr-ref)
  LENGTH(data-area)
  FLENGTH(data-area)
  ENTRY(ptr-ref)
  HOLD
END-EXEC
Conditions: INVREQ, LENGERR, NOTAUTH, PGMIDERR
```

### 2.5 RELEASE
```
EXEC CICS RELEASE
  PROGRAM(name)                  — Pflicht
END-EXEC
Conditions: INVREQ, NOTAUTH, PGMIDERR
```

---

## 3. Interval Control

### 3.1 START
```
EXEC CICS START
  TRANSID(name)                  — Pflicht
  TERMID(name)
  INTERVAL(hhmmss) | TIME(hhmmss) | AFTER | AT
  REQID(name)
  FROM(data-area)
  LENGTH(data-value)
  RTRANSID(name)
  RTERMID(name)
  QUEUE(name)
  NOCHECK
  SYSID(systemname)
END-EXEC
Conditions: INVREQ, IOERR, ISCINVREQ, LENGERR, NOTAUTH, SYSIDERR, TRANSIDERR, TERMIDERR
```

### 3.2 CANCEL
```
EXEC CICS CANCEL
  REQID(name)
  SYSID(systemname)
  TRANSID(name)
END-EXEC
Conditions: ISCINVREQ, NOTAUTH, NOTFND, SYSIDERR
```

### 3.3 DELAY
```
EXEC CICS DELAY
  INTERVAL(hhmmss)
  TIME(hhmmss)
  FOR
  HOURS(data-value)
  MINUTES(data-value)
  SECONDS(data-value)
  MILLISECS(data-value)
  UNTIL
  REQID(name)
END-EXEC
Conditions: EXPIRED, INVREQ, NORMAL
```

### 3.4 ASKTIME
```
EXEC CICS ASKTIME
  ABSTIME(data-area)
END-EXEC
Conditions: keine
```

### 3.5 FORMATTIME
```
EXEC CICS FORMATTIME
  ABSTIME(data-area)             — Pflicht
  DATE(data-area)
  FULLDATE(data-area)
  DATEFORM(data-area)
  DATESEP(data-value)
  DATESTRING(data-area)
  STRINGZONE(cvda)
  DAYCOUNT(data-area)
  DAYOFMONTH(data-area)
  DAYOFWEEK(data-area)
  DDMMYY(data-area)
  DDMMYYYY(data-area)
  MILLISECONDS(data-area)
  MMDDYY(data-area)
  MMDDYYYY(data-area)
  MONTHOFYEAR(data-area)
  STRINGFORMAT(cvda)
  TIME(data-area)
  TIMESEP(data-value)
  YEAR(data-area)
  YYDDD(data-area)
  YYDDMM(data-area)
  YYMMDD(data-area)
  YYYYDDD(data-area)
  YYYYDDMM(data-area)
  YYYYMMDD(data-area)
END-EXEC
Conditions: INVREQ
```

### 3.6 RETRIEVE
```
EXEC CICS RETRIEVE
  INTO(data-area) | SET(ptr-ref) — Eins von beiden Pflicht
  LENGTH(data-area)
  RTRANSID(data-area)
  RTERMID(data-area)
  QUEUE(data-area)
  WAIT
END-EXEC
Conditions: ENDDATA, ENVDEFERR, INVREQ, IOERR, LENGERR, NOTFND
```

---

## 4. Storage Control

### 4.1 GETMAIN
```
EXEC CICS GETMAIN
  SET(ptr-ref)                   — Pflicht
  FLENGTH(data-value) | LENGTH(data-value) — Eins Pflicht
  BELOW
  INITIMG(data-value)
  EXECUTABLE
  SHARED
  NOSUSPEND
  USERDATAKEY
  CICSDATAKEY
END-EXEC
Conditions: LENGERR, NOSTG
```

### 4.2 FREEMAIN
```
EXEC CICS FREEMAIN
  DATA(data-area) | DATAPOINTER(ptr-value) — Eins Pflicht
END-EXEC
Conditions: INVREQ
```

---

## 5. Task Control

### 5.1 ABEND
```
EXEC CICS ABEND
  ABCODE(name)
  CANCEL
  NODUMP
END-EXEC
Conditions: keine
```

### 5.2 SUSPEND
```
EXEC CICS SUSPEND
END-EXEC
Conditions: keine
```

### 5.3 ENQ
```
EXEC CICS ENQ
  RESOURCE(data-area)            — Pflicht
  LENGTH(data-value)
  UOW | TASK | MAXLIFETIME(data-value)
  NOSUSPEND
END-EXEC
Conditions: ENQBUSY, INVREQ, LENGERR
```

### 5.4 DEQ
```
EXEC CICS DEQ
  RESOURCE(data-area)            — Pflicht
  LENGTH(data-value)
  UOW | TASK | MAXLIFETIME(data-value)
END-EXEC
Conditions: INVREQ, LENGERR
```

---

## 6. Event/Timer Control

### 6.1 POST
```
EXEC CICS POST
  INTERVAL(hhmmss)
  TIME(hhmmss)
  SET(ptr-ref)                   — Pflicht
  REQID(name)
  AFTER
  AT
  HOURS(data-value)
  MINUTES(data-value)
  SECONDS(data-value)
END-EXEC
Conditions: EXPIRED, INVREQ
```

### 6.2 WAIT EVENT
```
EXEC CICS WAIT EVENT
  ECADDR(ptr-value)              — Pflicht
END-EXEC
Conditions: INVREQ
```

---

## 7. Queue (Transient Data — TD)

### 7.1 READQ TD
```
EXEC CICS READQ TD
  QUEUE(name)                    — Pflicht
  INTO(data-area) | SET(ptr-ref) — Eins von beiden Pflicht
  LENGTH(data-area)
END-EXEC
Conditions: INVREQ, IOERR, ISCINVREQ, LENGERR, NOTAUTH, NOTOPEN, QBUSY, QIDERR, QZERO, SYSIDERR
```

### 7.2 WRITEQ TD
```
EXEC CICS WRITEQ TD
  QUEUE(name)                    — Pflicht
  FROM(data-area)                — Pflicht
  LENGTH(data-value)
END-EXEC
Conditions: INVREQ, IOERR, ISCINVREQ, LENGERR, NOSPACE, NOTAUTH, NOTOPEN, QIDERR, SYSIDERR
```

### 7.3 DELETEQ TD
```
EXEC CICS DELETEQ TD
  QUEUE(name)                    — Pflicht
END-EXEC
Conditions: INVREQ, ISCINVREQ, NOTAUTH, QIDERR, SYSIDERR
```

---

## 8. Queue (Temporary Storage — TS)

### 8.1 READQ TS
```
EXEC CICS READQ TS
  QUEUE(name)                    — Pflicht
  INTO(data-area) | SET(ptr-ref) — Eins von beiden Pflicht
  LENGTH(data-area)
  ITEM(data-value)
  NEXT
  NUMITEMS(data-area)
  SYSID(systemname)
END-EXEC
Conditions: INVREQ, IOERR, ISCINVREQ, ITEMERR, LENGERR, NOTAUTH, QIDERR, SYSIDERR
```

### 8.2 WRITEQ TS
```
EXEC CICS WRITEQ TS
  QUEUE(name)                    — Pflicht
  FROM(data-area)                — Pflicht
  LENGTH(data-value)
  ITEM(data-area)
  REWRITE
  NOSUSPEND
  MAIN | AUXILIARY
  SYSID(systemname)
END-EXEC
Conditions: INVREQ, IOERR, ISCINVREQ, ITEMERR, LENGERR, NOSPACE, NOTAUTH, QIDERR, SYSIDERR
```

### 8.3 DELETEQ TS
```
EXEC CICS DELETEQ TS
  QUEUE(name)                    — Pflicht
  SYSID(systemname)
END-EXEC
Conditions: INVREQ, ISCINVREQ, NOTAUTH, QIDERR, SYSIDERR
```

---

## 9. BMS (Basic Mapping Support)

### 9.1 SEND MAP
```
EXEC CICS SEND MAP
  MAP(name)                      — Pflicht
  MAPSET(name)
  FROM(data-area)
  LENGTH(data-value)
  DATAONLY | MAPONLY
  ERASE
  ALARM
  FREEKB
  FRSET
  CURSOR(data-value)
  WAIT
  LAST
  PAGING
END-EXEC
Conditions: INVMPSZ, INVREQ, OVERFLOW
```

### 9.2 SEND TEXT
```
EXEC CICS SEND TEXT
  FROM(data-area)                — Pflicht
  LENGTH(data-value)
  ERASE
  ALARM
  FREEKB
  WAIT
  LAST
  PAGING
END-EXEC
Conditions: INVREQ, LENGERR
```

### 9.3 SEND CONTROL
```
EXEC CICS SEND CONTROL
  ERASE
  ALARM
  FREEKB
  FRSET
END-EXEC
Conditions: keine
```

### 9.4 SEND PAGE
```
EXEC CICS SEND PAGE
  RELEASE
  TRANSID(name)
  TRAILER(data-area)
  RETAIN
  AUTOPAGE | NOAUTOPAGE
  OPERPURGE
  LAST
  FMHPARM(data-area)
END-EXEC
Conditions: INVREQ, TSIOERR
```

### 9.5 RECEIVE MAP
```
EXEC CICS RECEIVE MAP
  MAP(name)                      — Pflicht
  MAPSET(name)
  INTO(data-area)                — Pflicht
  LENGTH(data-area)
  ASIS
  NOTRUNCATE
END-EXEC
Conditions: INVMPSZ, INVREQ, MAPFAIL
```

### 9.6 PURGE MESSAGE
```
EXEC CICS PURGE MESSAGE
END-EXEC
Conditions: INVREQ, TSIOERR
```

---

## 10. Exception Handling

### 10.1 HANDLE CONDITION
```
EXEC CICS HANDLE CONDITION
  condition(label) ...
END-EXEC
Spezialbehandlung: Jeder Condition-Name (aus CICS_KNOWN_CONDITIONS) wird von einem Label gefolgt.
```

### 10.2 HANDLE ABEND
```
EXEC CICS HANDLE ABEND
  PROGRAM(name) | LABEL(label) | CANCEL | RESET
END-EXEC
Conditions: PGMIDERR
PflichtOneOf: PROGRAM / LABEL / CANCEL / RESET
```

### 10.3 HANDLE AID
```
EXEC CICS HANDLE AID
  option(label) ...
Gültige Optionen: ANYKEY, CLEAR, CLRPARTN, ENTER, LIGHTPEN, OPERID, TRIGGER,
                   PA1, PA2, PA3, PF1–PF24
END-EXEC
Spezialbehandlung: Jeder AID-Name kann von einem Label gefolgt werden.
```

### 10.4 IGNORE CONDITION
```
EXEC CICS IGNORE CONDITION
  condition ...
END-EXEC
Spezialbehandlung: Condition-Namen ohne Labels. Mindestens 1 Condition erforderlich.
```

### 10.5 PUSH HANDLE
```
EXEC CICS PUSH HANDLE
END-EXEC
Conditions: keine
```

### 10.6 POP HANDLE
```
EXEC CICS POP HANDLE
END-EXEC
Conditions: keine
```

---

## 11. Sync Point Control

### 11.1 SYNCPOINT
```
EXEC CICS SYNCPOINT
END-EXEC
Conditions: ROLLEDBACK
```

### 11.2 SYNCPOINT ROLLBACK
```
EXEC CICS SYNCPOINT ROLLBACK
END-EXEC
Conditions: INVREQ
```

---

## 12. System Info

### 12.1 ADDRESS
```
EXEC CICS ADDRESS
  ACEE(ptr-ref)
  COMMAREA(ptr-ref)
  CWA(ptr-ref)
  EIB(ptr-ref)
  TCTUA(ptr-ref)
  TWA(ptr-ref)
END-EXEC
Conditions: keine
```

### 12.2 ASSIGN
```
EXEC CICS ASSIGN
  ABCODE(data-area)
  ABDUMP(data-area)
  ABOFFSET(data-area)
  ABPROGRAM(data-area)
  ACTIVITY(data-area)
  ACTIVITYID(data-area)
  ALTSCRNHT(data-area)
  ALTSCRNWD(data-area)
  APLKYBD(data-area)
  APLTEXT(data-area)
  APPLICATION(data-area)
  APPLID(data-area)
  ASRAINTRPT(data-area)
  ASRAKEY(cvda)
  ASRAPSW(data-area)
  ASRAREGS(data-area)
  ASRASPC(cvda)
  ASRASTG(cvda)
  BRIDGE(data-area)
  BTRANS(data-area)
  CHANNEL(data-area)
  CMDSEC(data-area)
  COLOR(data-area)
  CWALENG(data-area)
  DEFSCRNHT(data-area)
  DEFSCRNWD(data-area)
  DELIMITER(data-area)
  DESTCOUNT(data-area)
  DESTID(data-area)
  DESTIDLENG(data-area)
  DSSCS(data-area)
  DS3270(data-area)
  ERRORMSG(data-area)
  ERRORMSGLEN(data-area)
  EWASUPP(data-area)
  EXTDS(data-area)
  FACILITY(data-area)
  FCI(data-area)
  GCHARS(data-area)
  GCODES(data-area)
  GMMI(data-area)
  GMEXITOPT(data-area)
  HILIGHT(data-area)
  INITPARM(data-area)
  INITPARMLEN(data-area)
  INPARTN(data-area)
  INPUTMSGLEN(data-area)
  INVOKINGPROG(data-area)
  KATAKANA(data-area)
  LANGINUSE(data-area)
  LDCMNEM(data-area)
  LDCNUM(data-area)
  LINKLEVEL(data-area)
  LOCALCCSID(data-area)
  MAPCOLUMN(data-area)
  MAPHEIGHT(data-area)
  MAPLINE(data-area)
  MAPWIDTH(data-area)
  MSRCONTROL(data-area)
  NATLANGINUSE(data-area)
  NETNAME(data-area)
  NEXTTRANSID(data-area)
  NUMTAB(data-area)
  OPCLASS(data-area)
  OPERKEYS(data-area)
  OPID(data-area)
  OPSECURITY(data-area)
  ORGABCODE(data-area)
  OUTLINE(data-area)
  PAGENUM(data-area)
  PARTNPAGE(data-area)
  PARTNS(data-area)
  PARTNSET(data-area)
  PLATFORM(data-area)
  PRINSYSID(data-area)
  PROCESSTYPE(data-area)
  PROGRAM(data-area)
  PS(data-area)
  QNAME(data-area)
  RESSEC(data-area)
  RESTART(data-area)
  RETURNPROG(data-area)
  SCRNHT(data-area)
  SCRNWD(data-area)
  SIGDATA(data-area)
  SOSI(data-area)
  STARTCODE(data-area)
  STATIONID(data-area)
  SYSID(data-area)
  TASKPRIORITY(data-area)
  TCTUALENG(data-area)
  TELLERID(data-area)
  TERMCODE(data-area)
  TERMPRIORITY(data-area)
  TEXTKYBD(data-area)
  TEXTPRINT(data-area)
  TNADDR(data-area)
  TNIPFAMILY(data-area)
  TNPORT(data-area)
  TRANPRIORITY(data-area)
  TWALENG(data-area)
  UNATTEND(data-area)
  USERID(data-area)
  USERNAME(data-area)
  USERPRIORITY(data-area)
  VALIDATION(data-area)
END-EXEC
Conditions: INVREQ
Pflicht: keine
```

---

## 13. Diagnostic Control

### 13.1 DUMP TRANSACTION
```
EXEC CICS DUMP TRANSACTION
  DUMPCODE(name)                 — Pflicht
  FROM(data-area)
  LENGTH(data-value)
  FLENGTH(data-value)
  COMPLETE
  TASK
  STORAGE
  PROGRAM
  TERMINAL
  TABLES
  FCT
  PCT
  PPT
  SIT
  TCT
END-EXEC
Conditions: INVREQ, NOSPACE, NOTAUTH, SUPPRESSED
```

### 13.2 ENTER TRACENUM
```
EXEC CICS ENTER TRACENUM
  FROM(data-area)
  FROMLENGTH(data-value)
  RESOURCE(name)
  EXCEPTION
END-EXEC
Conditions: INVREQ
```

### 13.3 WRITE OPERATOR
```
EXEC CICS WRITE OPERATOR
  TEXT(data-area)                — Pflicht
  TEXTLENGTH(data-value)
  ROUTECODES(data-area)
  NUMROUTES(data-value)
  ACTION(cvda)
  CRITICAL | EVENTUAL | IMMEDIATE
  REPLY(data-area)
  MAXLENGTH(data-value)
  TIMEOUT(data-value)
END-EXEC
Conditions: INVREQ, LENGERR
```

---

## 14. Terminal Control

### 14.1 ISSUE ERASE
```
EXEC CICS ISSUE ERASE
END-EXEC
Conditions: keine
```

### 14.2 ISSUE ERASEAUP
```
EXEC CICS ISSUE ERASEAUP
END-EXEC
Conditions: keine
```

---

## Condition Cross-Reference

| Condition     | Beschreibung                                      |
|--------------|---------------------------------------------------|
| CBIDERR      | BMS mapset not found                               |
| DUPKEY       | Duplicate key detected                             |
| DUPREC       | Duplicate record                                   |
| ENDDATA      | End of data on RETRIEVE                            |
| ENDFILE      | End of file during browse                          |
| ENQBUSY      | Resource already enqueued                          |
| ENVDEFERR    | Environment definition error                       |
| EOC          | End of chain                                       |
| EODS         | End of data set                                    |
| ERROR        | General error                                      |
| EXPIRED      | Timer expired                                      |
| FILENOTFOUND | File not found in FCT                              |
| ILLOGIC      | VSAM logical error                                 |
| INVMPSZ      | Invalid map size                                   |
| INVREQ       | Invalid request                                    |
| IOERR        | I/O error                                          |
| ISCINVREQ    | ISC invalid request                                |
| ITEMERR      | TS queue item error                                |
| LENGERR      | Length error                                        |
| LINKABEND    | Linked program abended                             |
| MAPFAIL      | Map input failure                                   |
| NORMAL       | Normal completion                                   |
| NOSPACE      | Out of space                                        |
| NOSTG        | Out of storage (GETMAIN)                            |
| NOTALLOC     | Session not allocated                               |
| NOTAUTH      | Not authorized                                      |
| NOTFND       | Not found                                           |
| NOTOPEN      | File not open                                       |
| PGMIDERR     | Program ID error                                    |
| QBUSY        | Queue busy                                          |
| QIDERR       | Queue ID error                                      |
| QZERO        | Queue empty                                         |
| READONLY     | Read-only file                                      |
| RECORDBUSY   | Record in use by another task                       |
| REQIDERR     | Request ID error                                    |
| ROLLEDBACK   | Transaction rolled back                             |
| SUPPRESSED   | Dump suppressed                                     |
| SYSIDERR     | System ID error                                     |
| TASKIDERR    | Task ID error                                       |
| TERMERR      | Terminal error                                       |
| TERMIDERR    | Terminal ID error                                    |
| TRANSIDERR   | Transaction ID error                                 |
| TSIOERR      | TS I/O error                                         |
