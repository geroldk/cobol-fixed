import { CobolMemberType, MemberConf } from "./types";

const TYPE_VALUES: ReadonlySet<number> = new Set([0, 1, 2, 3, 4]);

function assertString(map: Map<string, string>, key: string): string {
  const value = map.get(key);
  if (value === undefined) throw new Error(`Missing required key '${key}' in .conf`);
  return value;
}

function parseType(raw: string): CobolMemberType {
  const value = Number.parseInt(raw.trim(), 10);
  if (!TYPE_VALUES.has(value)) {
    throw new Error(`Unsupported member type '${raw}'. Supported: 0,1,2,3,4`);
  }
  return value as CobolMemberType;
}

function parseSource(raw: string | undefined): number {
  if (!raw) return 0;
  const value = Number.parseInt(raw.trim(), 10);
  if (Number.isNaN(value)) return 0;
  return value;
}

function decodeQuotedValue(raw: string): string {
  const trimmed = raw.trim();
  if (!(trimmed.startsWith("\"") && trimmed.endsWith("\"") && trimmed.length >= 2)) {
    return trimmed;
  }

  const body = trimmed.slice(1, -1);
  let out = "";
  for (let i = 0; i < body.length; i++) {
    const ch = body[i];
    if (ch !== "\\") {
      out += ch;
      continue;
    }

    const next = body[i + 1];
    if (next === undefined) {
      out += "\\";
      break;
    }

    if (next === "n") out += "\n";
    else if (next === "r") out += "\r";
    else if (next === "t") out += "\t";
    else if (next === "\"") out += "\"";
    else if (next === "\\") out += "\\";
    else out += next;
    i += 1;
  }

  return out;
}

function encodeQuotedValue(raw: string): string {
  return raw
    .replace(/\\/g, "\\\\")
    .replace(/\r/g, "\\r")
    .replace(/\n/g, "\\n")
    .replace(/\t/g, "\\t")
    .replace(/"/g, "\\\"");
}

export function parseMemberConf(content: string): MemberConf {
  const values = new Map<string, string>();
  let inGeneralSection = false;

  for (const lineRaw of content.split(/\r?\n/)) {
    const line = lineRaw.trim();
    if (!line || line.startsWith(";")) continue;
    if (line.startsWith("[") && line.endsWith("]")) {
      inGeneralSection = line.slice(1, -1).trim().toLowerCase() === "general";
      continue;
    }
    if (!inGeneralSection && values.size > 0) continue;

    const eq = lineRaw.indexOf("=");
    if (eq < 0) continue;
    const key = lineRaw.slice(0, eq).trim().toLowerCase();
    if (!key) continue;
    const rawValue = lineRaw.slice(eq + 1);
    values.set(key, decodeQuotedValue(rawValue));
  }

  return {
    source: parseSource(values.get("source")),
    phase: assertString(values, "phase").trim(),
    type: parseType(assertString(values, "type")),
    tjcl: assertString(values, "tjcl"),
    pjcl: assertString(values, "pjcl"),
    toptions: assertString(values, "toptions"),
    poptions: assertString(values, "poptions"),
    txopts: values.get("txopts") ?? "",
    pxopts: values.get("pxopts") ?? "",
  };
}

export function serializeMemberConf(conf: MemberConf): string {
  const txopts = conf.txopts.trim();
  const pxopts = conf.pxopts.trim();

  const lines = [
    "[General]",
    `source=${Number.isFinite(conf.source) ? Math.trunc(conf.source) : 0}`,
    `phase=${conf.phase}`,
    `type=${conf.type}`,
    `tjcl="${encodeQuotedValue(conf.tjcl)}"`,
    `pjcl="${encodeQuotedValue(conf.pjcl)}"`,
    `toptions="${encodeQuotedValue(conf.toptions)}"`,
    `poptions="${encodeQuotedValue(conf.poptions)}"`,
    txopts ? `txopts=${txopts}` : "txopts=",
    pxopts ? `pxopts=${pxopts}` : "pxopts=",
    "",
  ];

  return lines.join("\n");
}

