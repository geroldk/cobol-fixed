/**
 * Tree-sitter bridge: pure utility functions for tree-sitter integration.
 * Handles UTF-8/UTF-16 offset conversion, tree edit computation, and
 * error collection from parse trees.
 */
import { TSTreeEdit, TSTreePoint, TsErr } from "./types";

export function utf8ByteOffsetToUtf16Index(text: string, byteOff: number): number {
  if (byteOff <= 0) return 0;
  const buf = Buffer.from(text, "utf8");
  if (byteOff >= buf.length) return text.length;
  return Buffer.from(buf.subarray(0, byteOff)).toString("utf8").length;
}

export function computeTreeEdit(oldText: string, newText: string): TSTreeEdit | undefined {
  if (oldText === newText) return undefined;

  const oldLen = oldText.length;
  const newLen = newText.length;
  const minLen = Math.min(oldLen, newLen);

  let start = 0;
  while (start < minLen && oldText.charCodeAt(start) === newText.charCodeAt(start)) start++;

  let oldEnd = oldLen;
  let newEnd = newLen;
  while (
    oldEnd > start &&
    newEnd > start &&
    oldText.charCodeAt(oldEnd - 1) === newText.charCodeAt(newEnd - 1)
  ) {
    oldEnd--;
    newEnd--;
  }

  return {
    startIndex: utf16IndexToUtf8ByteOffset(oldText, start),
    oldEndIndex: utf16IndexToUtf8ByteOffset(oldText, oldEnd),
    newEndIndex: utf16IndexToUtf8ByteOffset(newText, newEnd),
    startPosition: utf16IndexToTreeSitterPoint(oldText, start),
    oldEndPosition: utf16IndexToTreeSitterPoint(oldText, oldEnd),
    newEndPosition: utf16IndexToTreeSitterPoint(newText, newEnd),
  };
}

export function utf16IndexToUtf8ByteOffset(text: string, utf16Index: number): number {
  const lim = Math.max(0, Math.min(utf16Index, text.length));
  let bytes = 0;

  for (let i = 0; i < lim; i++) {
    const c = text.charCodeAt(i);
    if (c <= 0x7f) {
      bytes += 1;
      continue;
    }
    if (c <= 0x7ff) {
      bytes += 2;
      continue;
    }

    if (c >= 0xd800 && c <= 0xdbff && i + 1 < lim) {
      const next = text.charCodeAt(i + 1);
      if (next >= 0xdc00 && next <= 0xdfff) {
        bytes += 4;
        i++;
        continue;
      }
    }

    bytes += 3;
  }

  return bytes;
}

export function utf16IndexToTreeSitterPoint(text: string, utf16Index: number): TSTreePoint {
  const lim = Math.max(0, Math.min(utf16Index, text.length));
  let row = 0;
  let column = 0;

  for (let i = 0; i < lim; i++) {
    const c = text.charCodeAt(i);

    if (c === 0x0a) {
      row++;
      column = 0;
      continue;
    }

    if (c <= 0x7f) {
      column += 1;
      continue;
    }
    if (c <= 0x7ff) {
      column += 2;
      continue;
    }

    if (c >= 0xd800 && c <= 0xdbff && i + 1 < lim) {
      const next = text.charCodeAt(i + 1);
      if (next >= 0xdc00 && next <= 0xdfff) {
        column += 4;
        i++;
        continue;
      }
    }

    column += 3;
  }

  return { row, column };
}

export function isLikelyGlobalParserFailure(err: TsErr, textLength: number): boolean {
  const span = Math.max(0, err.endByte - err.startByte);
  return err.startByte <= 16 && span >= Math.max(512, Math.floor(textLength * 0.7));
}

export function filterTreeSitterErrors(errs: TsErr[], textLength: number): TsErr[] {
  if (errs.length === 0) return errs;
  return errs.filter((e, idx) => !(idx === 0 && isLikelyGlobalParserFailure(e, textLength)));
}

export function collectTreeSitterErrors(root: any, max = 25): TsErr[] {
  const out: TsErr[] = [];
  const seenRows = new Set<number>();

  const visit = (node: any) => {
    if (!node || out.length >= max) return;

    if (node.isError || node.type === "ERROR" || node.isMissing) {
      const row = node.startPosition?.row ?? 0;
      if (!seenRows.has(row)) {
        seenRows.add(row);
        out.push({
          startByte: node.startIndex,
          endByte: Math.max(node.endIndex, node.startIndex + 1),
          row,
          col: node.startPosition?.column ?? 0
        });
      }
      return; // nicht tiefer absteigen -> weniger Spam
    }

    for (const c of node.children ?? []) visit(c);
  };

  visit(root);
  return out;
}
