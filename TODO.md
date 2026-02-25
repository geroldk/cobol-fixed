High: Der Language Server ist nicht robust gegen Tree-sitter-Init-Fehler: eine einmal abgelehnte Init-Promise wird gecacht und danach immer wieder geworfen; validateDocument fängt das nicht ab. Ergebnis: Diagnostik kann dauerhaft ausfallen.

Medium: Stale Diagnostics möglich: im ID DIVISION-Early-Return wird publiziert, aber das Clearing (lastPublishedUris) übersprungen. Alte Fehler in anderen URIs können stehen bleiben.

Medium: language-configuration.json wird nie geladen, weil in contributes.languages die configuration-Property fehlt. Damit fehlen z.B. Folding/Indent-Regeln effektiv in VS Code.

Go-to-definition für:
Data names (WORKING-STORAGE, LINKAGE)
Paragraphen/Sections
COPY books

Hover: “01 CUSTOMER-REC …” mit Level/PICTURE/USAGE

References, Rename (mit Scope-Regeln)

Completion (Context-aware: Data Division vs Procedure Division)

Call graph / dead code hints