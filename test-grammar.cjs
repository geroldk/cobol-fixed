const { readFileSync } = require("fs");
const { resolve } = require("path");
const vsctm = require("vscode-textmate");
const oniguruma = require("vscode-oniguruma");

// Load oniguruma WASM
const wasmPath = resolve(__dirname, "node_modules/vscode-oniguruma/release/onig.wasm");
const wasmBin = readFileSync(wasmPath).buffer;

async function main() {
await oniguruma.loadWASM(wasmBin);

const registry = new vsctm.Registry({
  onigLib: Promise.resolve({
    createOnigScanner: (patterns) => new oniguruma.OnigScanner(patterns),
    createOnigString: (s) => new oniguruma.OnigString(s),
  }),
  loadGrammar: async (scopeName) => {
    if (scopeName === "source.cobol.fixed") {
      const grammarPath = resolve(__dirname, "cobol_fixed.tmLanguage.json");
      const content = readFileSync(grammarPath, "utf8");
      return vsctm.parseRawGrammar(content, grammarPath);
    }
    return null;
  },
});

const grammar = await registry.loadGrammar("source.cobol.fixed");

function tokenizeLine(text, prevState) {
  // Pad to fixed-format: 6 spaces + blank indicator + code
  const line = "      " + " " + text.padEnd(65);
  const result = grammar.tokenizeLine(line, prevState);
  return result;
}

function tokenizeBlock(lines) {
  let state = vsctm.INITIAL;
  const allTokens = [];
  for (const line of lines) {
    const padded = line.padEnd(80);
    const result = grammar.tokenizeLine(padded, state);
    state = result.ruleStack;
    const tokens = result.tokens
      .filter(t => {
        const scopes = t.scopes.join(" ");
        // Skip sequence area, indicator, identification area, and whitespace-only tokens
        if (scopes.includes("comment.line-number") || scopes.includes("comment.block.identification-area")) return false;
        if (scopes.includes("constant.character.indicator")) return false;
        const text = padded.substring(t.startIndex, t.endIndex).trim();
        if (!text) return false;
        return true;
      })
      .map(t => ({
        text: padded.substring(t.startIndex, t.endIndex).trim(),
        scopes: t.scopes.filter(s => s !== "source.cobol.fixed"),
      }));
    allTokens.push(...tokens);
  }
  return { tokens: allTokens, state };
}

// ── Test Cases ──────────────────────────────────────────────────────────

let passed = 0;
let failed = 0;

function expectScope(testName, tokens, word, expectedScope) {
  const tok = tokens.find(t => t.text === word);
  if (!tok) {
    console.log(`  FAIL: ${testName} — token "${word}" not found`);
    failed++;
    return;
  }
  const hasScope = tok.scopes.some(s => s.includes(expectedScope));
  if (hasScope) {
    passed++;
  } else {
    console.log(`  FAIL: ${testName} — "${word}" expected scope containing "${expectedScope}", got: ${tok.scopes.join(", ")}`);
    failed++;
  }
}

function expectNoScope(testName, tokens, word, unexpectedScope) {
  const tok = tokens.find(t => t.text === word);
  if (!tok) {
    // Token not found as a separate token — that's OK, it means it wasn't highlighted
    passed++;
    return;
  }
  const hasScope = tok.scopes.some(s => s.includes(unexpectedScope));
  if (!hasScope) {
    passed++;
  } else {
    console.log(`  FAIL: ${testName} — "${word}" should NOT have scope "${unexpectedScope}", got: ${tok.scopes.join(", ")}`);
    failed++;
  }
}

// Test 1: EXEC CICS ASSIGN — verbs & options highlighted, parens are plain
console.log("Test 1: EXEC CICS ASSIGN TCTUALENG(TCTUALENG) OPID(OPER-ID) SYSID(SYS-ID)");
{
  const { tokens } = tokenizeBlock([
    "           EXEC CICS ASSIGN TCTUALENG (TCTUALENG)                               ",
    "                            OPID      (OPER-ID)                                 ",
    "                            SYSID     (SYS-ID)                                  ",
    "                            END-EXEC.                                           ",
  ]);
  
  expectScope("EXEC CICS", tokens, "EXEC CICS", "keyword.control");
  expectScope("ASSIGN verb", tokens, "ASSIGN", "support.function.cics");
  expectScope("TCTUALENG option", tokens, "TCTUALENG", "keyword.other.clause.cics");
  expectScope("OPID option", tokens, "OPID", "keyword.other.clause.cics");
  expectScope("SYSID option", tokens, "SYSID", "keyword.other.clause.cics");
  expectScope("END-EXEC", tokens, "END-EXEC", "keyword.control.flow");
  
  // Inside parens — should NOT be CICS keywords
  const tctInParen = tokens.filter(t => t.text === "TCTUALENG");
  if (tctInParen.length >= 2) {
    const secondTct = tctInParen[1];
    const hasKeyword = secondTct.scopes.some(s => s.includes("keyword") || s.includes("support.function"));
    if (!hasKeyword) {
      passed++;
    } else {
      console.log(`  FAIL: TCTUALENG in parens should be plain, got: ${secondTct.scopes.join(", ")}`);
      failed++;
    }
  }
}

// Test 2: EXEC CICS ADDRESS TCTUA (ADDRESS OF TCTUA) — ADDRESS in parens must NOT be verb
console.log("\nTest 2: EXEC CICS ADDRESS TCTUA (ADDRESS OF TCTUA)");
{
  const { tokens } = tokenizeBlock([
    "           EXEC CICS ADDRESS TCTUA (ADDRESS OF TCTUA)                           ",
    "                             END-EXEC.                                          ",
  ]);
  
  expectScope("ADDRESS verb", tokens, "ADDRESS", "support.function.cics");
  expectScope("TCTUA option", tokens, "TCTUA", "keyword.other.clause.cics");
  
  // ADDRESS inside parens should NOT be a verb
  const addrTokens = tokens.filter(t => t.text === "ADDRESS");
  if (addrTokens.length >= 2) {
    const addrInParen = addrTokens[1];
    const hasVerb = addrInParen.scopes.some(s => s.includes("support.function"));
    if (!hasVerb) {
      passed++;
    } else {
      console.log(`  FAIL: ADDRESS in parens should NOT be verb, got: ${addrInParen.scopes.join(", ")}`);
      failed++;
    }
  }
  
  // OF inside parens — should not be keyword
  const ofTokens = tokens.filter(t => t.text === "OF");
  if (ofTokens.length > 0) {
    const ofInParen = ofTokens[0];
    const hasKeyword = ofInParen.scopes.some(s => s.includes("keyword"));
    if (!hasKeyword) {
      passed++;
    } else {
      console.log(`  FAIL: OF in parens should be plain, got: ${ofInParen.scopes.join(", ")}`);
      failed++;
    }
  } else {
    passed++; // OF not tokenized separately = fine
  }
}

// Test 3: EXEC CICS SEND MAP — verbs and options
console.log("\nTest 3: EXEC CICS SEND MAP('EMKOMP9') FROM(RETUR2) LENGTH(28) ERASE");
{
  const { tokens } = tokenizeBlock([
    "           EXEC CICS SEND FROM (RETUR2)                                         ",
    "                          LENGTH (28)                                           ",
    "                          ERASE                                                 ",
    "                          END-EXEC                                              ",
  ]);
  
  expectScope("SEND verb", tokens, "SEND", "support.function.cics");
  expectScope("FROM option", tokens, "FROM", "keyword.other.clause.cics");
  expectScope("LENGTH option", tokens, "LENGTH", "keyword.other.clause.cics");
  expectScope("ERASE option", tokens, "ERASE", "keyword.other.clause.cics");
  expectNoScope("RETUR2 in parens", tokens, "RETUR2", "keyword");
}

// Test 4: EXEC CICS HANDLE CONDITION — conditions highlighted
console.log("\nTest 4: EXEC CICS HANDLE CONDITION NOTOPEN(NOT-OPEN) NOSPACE(VOLL)");
{
  const { tokens } = tokenizeBlock([
    "           EXEC CICS HANDLE CONDITION NOTOPEN (NOT-OPEN)                        ",
    "                                      NOSPACE (VOLL)                             ",
    "                                      END-EXEC.                                 ",
  ]);
  
  expectScope("HANDLE verb", tokens, "HANDLE", "support.function.cics");
  expectScope("CONDITION option", tokens, "CONDITION", "keyword.other.clause.cics");
  expectScope("NOTOPEN condition", tokens, "NOTOPEN", "support.type.cics");
  expectScope("NOSPACE condition", tokens, "NOSPACE", "support.type.cics");
  expectNoScope("NOT-OPEN in parens", tokens, "NOT-OPEN", "keyword");
}

// Test 5: EXEC CICS RETURN (simple, no options)
console.log("\nTest 5: EXEC CICS RETURN END-EXEC");
{
  const { tokens } = tokenizeBlock([
    "           EXEC CICS RETURN END-EXEC.                                           ",
  ]);
  
  expectScope("RETURN verb", tokens, "RETURN", "support.function.cics");
}

// Test 6: EXEC CICS DELETEQ TS QUEUE(Q-NAME)  
console.log("\nTest 6: EXEC CICS DELETEQ TS QUEUE(Q-NAME)");
{
  const { tokens } = tokenizeBlock([
    "           EXEC CICS DELETEQ TS QUEUE (Q-NAME)                                  ",
    "                                END-EXEC.                                       ",
  ]);
  
  expectScope("DELETEQ verb", tokens, "DELETEQ", "support.function.cics");
  expectScope("TS option", tokens, "TS", "keyword.other.clause.cics");
  expectScope("QUEUE option", tokens, "QUEUE", "keyword.other.clause.cics");
  expectNoScope("Q-NAME in parens", tokens, "Q-NAME", "keyword");
}

// Test 7: Regular COBOL outside EXEC CICS — SEND, ERASE etc. should NOT be CICS-colored
console.log("\nTest 7: Regular COBOL — MOVE ERASE TO WS-FIELD (no CICS scope)");
{
  const { tokens } = tokenizeBlock([
    "           MOVE ERASE TO WS-FIELD.                                              ",
  ]);
  
  expectNoScope("ERASE outside CICS", tokens, "ERASE", "cics");
  expectNoScope("ERASE outside CICS", tokens, "ERASE", "support.function");
}

// Test 8: EXEC CICS START TRANSID(EIBTRNID) TERMID(EIBTRMID)
console.log("\nTest 8: EXEC CICS START TRANSID(EIBTRNID) TERMID(EIBTRMID)");
{
  const { tokens } = tokenizeBlock([
    "           EXEC CICS START TRANSID (EIBTRNID)                                   ",
    "                           TERMID  (EIBTRMID)                                   ",
    "                           END-EXEC.                                            ",
  ]);
  
  expectScope("START verb", tokens, "START", "support.function.cics");
  expectScope("TRANSID option", tokens, "TRANSID", "keyword.other.clause.cics");
  expectScope("TERMID option", tokens, "TERMID", "keyword.other.clause.cics");
  expectNoScope("EIBTRNID in parens", tokens, "EIBTRNID", "keyword");
  expectNoScope("EIBTRMID in parens", tokens, "EIBTRMID", "keyword");
}

// ── Summary ─────────────────────────────────────────────────────────────
console.log(`\n${"=".repeat(50)}`);
console.log(`Results: ${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
}
main();
