import * as vscode from "vscode";
import { VseSettings } from "./types";

function readString(config: vscode.WorkspaceConfiguration, key: string, fallback = ""): string {
  const value = config.get<unknown>(key);
  return typeof value === "string" ? value : fallback;
}

function readNumber(config: vscode.WorkspaceConfiguration, key: string, fallback: number): number {
  const value = config.get<unknown>(key);
  return typeof value === "number" && Number.isFinite(value) ? value : fallback;
}

function readBoolean(config: vscode.WorkspaceConfiguration, key: string, fallback: boolean): boolean {
  const value = config.get<unknown>(key);
  return typeof value === "boolean" ? value : fallback;
}

export function getVseSettings(): VseSettings {
  const config = vscode.workspace.getConfiguration("cobol85");
  const executionModeRaw = readString(config, "vse.submit.executionMode", "blocking");
  const executionMode = executionModeRaw === "background" ? "background" : "blocking";

  return {
    host: readString(config, "vse.host", "").trim(),
    port: readNumber(config, "vse.port", 2893),
    user: readString(config, "vse.user", "").trim(),
    charset: readString(config, "vse.charset", "utf8").trim() || "utf8",
    submit: {
      executionMode,
      timeoutSec: Math.max(0, Math.trunc(readNumber(config, "vse.submit.timeoutSec", 120))),
      previewBeforeSubmit: readBoolean(config, "vse.submit.previewBeforeSubmit", true),
    },
    conf: {
      autoCreateOnMissing: readBoolean(config, "vse.conf.autoCreateOnMissing", true),
      previewBeforeCreate: readBoolean(config, "vse.conf.previewBeforeCreate", true),
    },
    placeholders: {
      catalogBatchTest: readString(config, "vse.placeholders.catalogBatchTest", "USRWMT").trim(),
      catalogBatchProd: readString(config, "vse.placeholders.catalogBatchProd", "USRWMP").trim(),
      catalogCicsTest: readString(config, "vse.placeholders.catalogCicsTest", "USRWMT").trim(),
      catalogCicsProd: readString(config, "vse.placeholders.catalogCicsProd", "USRWMP").trim(),
      id: readString(config, "vse.placeholders.id", "").trim(),
      lnkstep: readString(config, "vse.placeholders.lnkstep", "").trim(),
    },
    output: {
      baseDir: readString(config, "vse.output.baseDir", ".vse/out").trim() || ".vse/out",
    },
  };
}

export function validateConnectionSettings(settings: VseSettings): string[] {
  const errors: string[] = [];
  if (!settings.host) errors.push("`cobol85.vse.host` is missing.");
  if (!settings.user) errors.push("`cobol85.vse.user` is missing.");
  if (settings.port <= 0 || settings.port > 65535) errors.push("`cobol85.vse.port` must be between 1 and 65535.");
  return errors;
}
