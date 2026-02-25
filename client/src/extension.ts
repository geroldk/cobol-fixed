import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  State,
  TransportKind,
} from "vscode-languageclient/node";


let client: LanguageClient | undefined;
const FORCE_VALIDATE_NOTIFICATION = "cobol85/forceValidate";


export function activate(context: vscode.ExtensionContext) {
  const isCobol85Document = (doc: vscode.TextDocument | undefined): doc is vscode.TextDocument => {
    if (!doc) return false;
    if (doc.languageId !== "cobol85") return false;
    return doc.uri.scheme === "file" || doc.uri.scheme === "untitled";
  };

  const notifyForceValidate = (doc: vscode.TextDocument | undefined) => {
    if (!client || !isCobol85Document(doc)) return;
    client.sendNotification(FORCE_VALIDATE_NOTIFICATION, { uri: doc.uri.toString() });
  };

  const disposable = vscode.commands.registerCommand(
    "cobol85.toggleFixedComment",
    async () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) return;

      const doc = editor.document;

      // Optional: Tabs sind für Fixed-Columns Gift. Hier warnen wir nur.
      const hasTabs = editor.selections.some((sel) => {
        const { startLine, endLine } = selectionToLineRange(doc, sel);
        for (let line = startLine; line <= endLine; line++) {
          if (doc.lineAt(line).text.includes("\t")) return true;
        }
        return false;
      });

      if (hasTabs) {
        vscode.window.showWarningMessage(
          "COBOL Fixed-Format: Tabs können die Spaltenzählung kaputt machen. Besser: Spaces verwenden."
        );
      }

      // Alle betroffenen Zeilen (Multi-Cursor/Selections) einsammeln
      const lines = new Set<number>();
      for (const sel of editor.selections) {
        const { startLine, endLine } = selectionToLineRange(doc, sel);
        for (let line = startLine; line <= endLine; line++) lines.add(line);
      }

      const tabSize = typeof editor.options.tabSize === "number" ? editor.options.tabSize : 4;

      await editor.edit(
        (editBuilder) => {
          // Stabil: von oben nach unten ok, wir ersetzen ganze Zeilen.
          for (const line of Array.from(lines).sort((a, b) => a - b)) {
            const lineObj = doc.lineAt(line);
            const original = lineObj.text;

            // Optional: Tabs expanden (macht Fixed-Columns wenigstens konsistent auf Zeichenebene)
            const text = expandTabs(original, tabSize);

            const toggled = toggleIndicatorAtColumn7(text);

            // Ganze Zeile ersetzen (ohne Zeilenumbruch)
            editBuilder.replace(lineObj.range, toggled);
          }
        },
        { undoStopBefore: true, undoStopAfter: true }
      );
    }
  );
  context.subscriptions.push
  // ---- LSP Client boot
  const serverModule = context.asAbsolutePath(
    path.join("server", "out", "server.js")
  );

  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: { execArgv: ["--nolazy", "--inspect=6009"] },
    },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "cobol85" },
      { scheme: "untitled", language: "cobol85" },
    ],
    synchronize: {
      // optional: copybooks trigger re-validate
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.{cpy,CPY}"),
    },
  };

  client = new LanguageClient(
    "cobol85LanguageServer",
    "COBOL 85 Language Server",
    serverOptions,
    clientOptions
  );

  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor((editor) => {
      notifyForceValidate(editor?.document);
    })
  );

  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument((doc) => {
      notifyForceValidate(doc);
    })
  );

  context.subscriptions.push(
    client.onDidChangeState((e) => {
      if (e.newState !== State.Running) return;
      notifyForceValidate(vscode.window.activeTextEditor?.document);
      for (const editor of vscode.window.visibleTextEditors) {
        notifyForceValidate(editor.document);
      }
    })
  );

  client.start();

  // Falls der Client bereits laeuft (z.B. schneller Restart), trotzdem initial triggern.
  if (client.state === State.Running) {
    notifyForceValidate(vscode.window.activeTextEditor?.document);
    for (const editor of vscode.window.visibleTextEditors) {
      notifyForceValidate(editor.document);
    }
  }

  context.subscriptions.push(client);
}

export async function deactivate(): Promise<void> {
  if (client) await client.stop();
}

/**
 * COBOL fixed format: Column 7 (1-based) == index 6 (0-based).
 * Toggle-Regel:
 *   - wenn '*' oder '/' in col 7: -> Space (uncomment)
 *   - sonst -> '*' (comment)
 */
function toggleIndicatorAtColumn7(line: string): string {
  const chars = Array.from(line);

  // Auf mindestens 7 Zeichen auffüllen, damit index 6 existiert
  while (chars.length < 7) chars.push(" ");

  const ind = chars[6];
  if (ind === "*" || ind === "/") {
    chars[6] = " ";
  } else {
    chars[6] = "*";
  }
  return chars.join("");
}

/**
 * VS Code selections sind "end-exklusive". Wenn selection.end.character==0 und multi-line,
 * ist die letzte Zeile in der Praxis oft nicht gemeint.
 */
function selectionToLineRange(doc: vscode.TextDocument, sel: vscode.Selection): { startLine: number; endLine: number } {
  const startLine = Math.max(0, Math.min(sel.start.line, doc.lineCount - 1));
  let endLine = Math.max(0, Math.min(sel.end.line, doc.lineCount - 1));

  if (!sel.isEmpty && sel.end.character === 0 && endLine > startLine) {
    endLine -= 1;
  }
  return { startLine, endLine };
}

/**
 * Expand Tabs -> Spaces, so dass die "visuelle" Spaltenlogik eher dem Fixed-Format entspricht.
 * (Bleibt natürlich nur eine Annäherung; ideal sind Spaces im File.)
 */
function expandTabs(s: string, tabSize: number): string {
  let col = 0;
  let out = "";
  for (const ch of s) {
    if (ch === "\t") {
      const spaces = tabSize - (col % tabSize);
      out += " ".repeat(spaces);
      col += spaces;
    } else {
      out += ch;
      col += 1;
    }
  }
  return out;
}
