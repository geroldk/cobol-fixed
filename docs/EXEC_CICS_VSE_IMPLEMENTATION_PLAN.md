# EXEC CICS (IBM z/VSE) Implementation Plan

Last updated: 2026-02-27

## Goals
- Expand EXEC CICS linting from command-presence only to command + option validation.
- Prioritize IBM z/VSE style command-level usage in file control and terminal control.
- Add explicit support checks for `SYSID`, `LENGERR`, `UPDATE`, and `ERASE` first.

## Phase 1 (implemented now)
- Add CICS command resolver with multi-word command support (for example `HANDLE CONDITION`, `READQ TS`, `SEND MAP`).
- Add unknown-command diagnostic with suggestions.
- Add option validation for VSE-first commands:
  - File control: `READ`, `READNEXT`, `READPREV`, `STARTBR`, `RESETBR`, `ENDBR`, `WRITE`, `REWRITE`, `DELETE`, `UNLOCK`.
  - Program control: `LINK`, `XCTL`, `RETURN`, `START`.
  - BMS/terminal basics: `SEND MAP`, `SEND TEXT`, `SEND CONTROL`, `RECEIVE MAP`.
- Add condition-name validation for `HANDLE CONDITION` including `LENGERR`.
- New diagnostics for option misuse (for example `UPDATE` on unsupported commands, `ERASE` outside SEND contexts).

## Phase 2 (next)
- Add queue/temporary-storage/transient-data detailed clause validation (`READQ/WRITEQ/DELETEQ`).
- Add `RESP`/`RESP2` awareness and condition mapping hints.
- Add SYSID cross-command consistency checks and remote-access hints.

## Phase 3 (next)
- Extend to conversation/terminal families (`ISSUE`, `WAIT`, `CONVERSE`, `RECEIVE`, `SEND` variants).
- Add release profile switch (strict z/VSE profile vs broad CICS profile).
- Add quick-fix suggestions for frequent typos and invalid options.

## Test Strategy
- Unit tests in `server/src/lint.test.ts` for positive and negative cases.
- Keep tests focused on deterministic token-level behavior.
- Run `npm run test` after each phase.
