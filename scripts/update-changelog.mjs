#!/usr/bin/env node
import { execSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";

const START = "<!-- AUTO-CHANGELOG-START -->";
const END = "<!-- AUTO-CHANGELOG-END -->";

function run(cmd) {
  try {
    return execSync(cmd, { encoding: "utf8" }).trimEnd();
  } catch (err) {
    const msg = err?.stderr?.toString?.() || err?.message || String(err);
    throw new Error(`Command failed: ${cmd}\n${msg}`);
  }
}

function parseShortStat(text) {
  const files = Number((text.match(/(\d+)\s+files?\s+changed/) || [])[1] || 0);
  const insertions = Number((text.match(/(\d+)\s+insertions?\(\+\)/) || [])[1] || 0);
  const deletions = Number((text.match(/(\d+)\s+deletions?\(-\)/) || [])[1] || 0);
  return { files, insertions, deletions };
}

function asList(text) {
  if (!text) return [];
  return text.split(/\r?\n/).map((s) => s.trim()).filter(Boolean);
}

function normalizePathFromNameStatus(line) {
  const parts = line.split("\t");
  return parts[parts.length - 1];
}

const repoRoot = process.cwd();
const changelogPath = path.join(repoRoot, "CHANGELOG.md");

if (!fs.existsSync(changelogPath)) {
  throw new Error(`Missing CHANGELOG.md in ${repoRoot}`);
}

const lastTag = run("git describe --tags --abbrev=0").trim();
const commitCount = Number(run(`git rev-list --count ${lastTag}..HEAD`).trim() || "0");
const shortStat = parseShortStat(run(`git diff --shortstat ${lastTag}`));

const tracked = asList(run(`git diff --name-status ${lastTag}`))
  .map(normalizePathFromNameStatus)
  .sort((a, b) => a.localeCompare(b));

const untracked = asList(run("git ls-files --others --exclude-standard"))
  .sort((a, b) => a.localeCompare(b));

let commitLines = [];
if (commitCount === 0) {
  commitLines = [`- Keine neuen Commits seit \`${lastTag}\`.`];
} else {
  const raw = asList(run(`git log ${lastTag}..HEAD --date=short --pretty=format:%h%x09%ad%x09%s`));
  commitLines = raw.map((line) => {
    const [hash, date, ...subjectParts] = line.split("\t");
    const subject = subjectParts.join("\t");
    return `- \`${hash}\` (${date}) ${subject}`;
  });
}

const trackedLines = tracked.length > 0
  ? tracked.map((p) => `- \`${p}\``)
  : ["- Keine."];

const untrackedLines = untracked.length > 0
  ? untracked.map((p) => `- \`${p}\``)
  : ["- Keine."];

const autoBlock = [
  START,
  "### Snapshot (auto)",
  `- Letzter Release-Tag: \`${lastTag}\``,
  `- Neue Commits seit letztem Release: \`${commitCount}\``,
  `- Tracking-Diff seit \`${lastTag}\`: \`${shortStat.files}\` Dateien, \`${shortStat.insertions}\` Einfuegungen, \`${shortStat.deletions}\` Loeschungen`,
  `- Untracked Dateien: \`${untracked.length}\``,
  "",
  "### Commits seit letztem Release",
  ...commitLines,
  "",
  `### Dateien (Tracked, geaendert seit \`${lastTag}\`)`,
  ...trackedLines,
  "",
  "### Dateien (Untracked)",
  ...untrackedLines,
  END,
].join("\n");

const original = fs.readFileSync(changelogPath, "utf8");
const startIdx = original.indexOf(START);
const endIdx = original.indexOf(END);

if (startIdx < 0 || endIdx < 0 || endIdx < startIdx) {
  throw new Error("Could not locate AUTO-CHANGELOG markers in CHANGELOG.md");
}

const before = original.slice(0, startIdx);
const after = original.slice(endIdx + END.length);
const updated = `${before}${autoBlock}${after}`;

fs.writeFileSync(changelogPath, updated, "utf8");
console.log(`Updated ${changelogPath}`);