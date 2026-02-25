# Changelog

Kompletter Verlauf von "Stunde 0" bis jetzt.

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

## Vollstaendige Timeline (Commit fuer Commit)

| # | Zeitpunkt | Commit | Autor | Beschreibung |
|---|---|---|---|---|
| 1 | 2026-02-25T16:22:52+01:00 | `90c6c3e` | gerold | inital |
| 2 | 2026-02-25T16:35:12+01:00 | `c7b32c4` | gerold | Add COBOL extension sources and .gitignore |
| 3 | 2026-02-25T19:20:12+01:00 | `aa90795` | gerold | Update vendor/tree-sitter-cobol submodule |
| 4 | 2026-02-25T19:35:17+01:00 | `791a307` | gerold | new  url |
| 5 | 2026-02-25T18:40:03+00:00 | `ed52f03` | copilot-swe-agent[bot] | Initial plan |
| 6 | 2026-02-25T18:41:58+00:00 | `8d2c621` | copilot-swe-agent[bot] | Add THIRD-PARTY-LICENSES.md initial plan |
| 7 | 2026-02-25T18:42:38+00:00 | `fe555e2` | copilot-swe-agent[bot] | Add THIRD-PARTY-LICENSES.md and set license field in package.json |
| 8 | 2026-02-25T19:43:52+01:00 | `46ac949` | Gerold | Merge pull request #1 from geroldk/copilot/check-license-requirements |
| 9 | 2026-02-25T22:55:28+01:00 | `013edb5` | gerold | Add configurable BLOCK_CLOSED_BY_PERIOD warning, bump version to 0.0.2, and refresh parser/docs |

## Submodul-Verlauf (`vendor/tree-sitter-cobol`)

- Initialer Pointer im Hauptrepo: `e99dbdc`
- Update in Hauptrepo-Commit `aa90795`: `e99dbdc` -> `7a813b`
- Update in Hauptrepo-Commit `013edb5`: `7a813b` -> `f766bdd`
