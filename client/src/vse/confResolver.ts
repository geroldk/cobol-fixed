import { readdir } from "node:fs/promises";
import * as path from "path";
import * as vscode from "vscode";
import { ConfResolveResult } from "./types";

type ConfCandidate = {
  label: string;
  path: string;
};

async function listConfCandidates(dirPath: string): Promise<ConfCandidate[]> {
  const entries = await readdir(dirPath, { withFileTypes: true });
  return entries
    .filter((entry) => entry.isFile() && entry.name.toLowerCase().endsWith(".conf"))
    .map((entry) => ({ label: entry.name, path: path.join(dirPath, entry.name) }))
    .sort((a, b) => a.label.localeCompare(b.label));
}

export async function resolveConfForSource(docPath: string): Promise<ConfResolveResult> {
  const dirPath = path.dirname(docPath);
  const ext = path.extname(docPath);
  const memberBaseName = path.basename(docPath, ext);
  const candidates = await listConfCandidates(dirPath);

  const exactName = `${memberBaseName}.conf`.toLowerCase();
  const exact = candidates.find((c) => c.label.toLowerCase() === exactName);
  if (exact) {
    return { kind: "resolved", confPath: exact.path };
  }

  if (candidates.length === 0) {
    return {
      kind: "missing",
      suggestedPath: path.join(dirPath, `${memberBaseName}.conf`),
      dirPath,
      memberBaseName,
    };
  }

  const selected = await vscode.window.showQuickPick(
    candidates.map((c) => ({
      label: c.label,
      description: c.path,
      path: c.path,
    })),
    {
      title: `Select .conf for ${path.basename(docPath)}`,
      placeHolder: "No exact .conf match found. Select one manually.",
      ignoreFocusOut: true,
    }
  );

  if (!selected) {
    throw new Error("No .conf selected.");
  }

  return { kind: "resolved", confPath: selected.path };
}

