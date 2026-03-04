export type BuildMode = "T" | "P";

export type CobolMemberType = 0 | 1 | 2 | 3 | 4;

export type MemberConf = {
  source: number;
  phase: string;
  type: CobolMemberType;
  tjcl: string;
  pjcl: string;
  toptions: string;
  poptions: string;
  txopts: string;
  pxopts: string;
};

export type ConfResolveResult =
  | { kind: "resolved"; confPath: string }
  | { kind: "missing"; suggestedPath: string; dirPath: string; memberBaseName: string };

export type VseSettings = {
  host: string;
  port: number;
  user: string;
  charset: string;
  submit: {
    executionMode: "blocking" | "background";
    timeoutSec: number;
    previewBeforeSubmit: boolean;
  };
  conf: {
    autoCreateOnMissing: boolean;
    previewBeforeCreate: boolean;
  };
  placeholders: {
    catalogTest: string;
    catalogProd: string;
    id: string;
    lnkstep: string;
  };
  output: {
    baseDir: string;
  };
};

export const VSE_LAST_BUILD_MODE_KEY = "cobol85.vse.lastBuildMode";
