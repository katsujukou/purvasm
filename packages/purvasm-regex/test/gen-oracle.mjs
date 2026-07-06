// Gate 2 (ADR-0081 §3): regenerate the "CST Lexer patterns vs the JS RegExp oracle" describe
// block of Test.Unit.Regex.Core from a live node `RegExp` — the authoritative ES semantics.
//
//   node test/gen-oracle.mjs        # prints the PureScript describe block to stdout
//
// then paste it over the generated block in test/Unit/Regex/Core.purs (below the `match` block).
// Every one of PureScript.CST.Lexer's demanding patterns, wrapped `^(?:…)` exactly as the Lexer
// does, over a representative input set. Pattern strings use \\ for every regex backslash and
// \\r/\\n for CR/LF, so the literal is written verbatim into PureScript — no escape-equivalence
// ambiguity between the two languages.

const P = {
  "block comment":     "\\{-(-(?!\\})|[^-]+)*(-\\}|$)",
  "line comment":      "--[^\\r\\n]*",
  "shebang":           "#![^\\r\\n]*",
  "spaces":            " +",
  "newline LF":        "\\n+",
  "newline CRLF":      "(?:\\r\\n)+",
  "module name":       "(?:(?:\\p{Lu}[\\p{L}0-9_']*)\\.)*",
  "proper name":       "\\p{Lu}[\\p{L}0-9_']*",
  "ident":             "[\\p{Ll}_][\\p{L}0-9_']*",
  "symbol":            "(?:[:!#$%&*+./<=>?@\\^|~-]|(?!\\p{P})\\p{S})+",
  "hex":               "[a-fA-F0-9]{1,6}",
  "whitespace escape": "\\\\[ \\r\\n]+\\\\",
  "string characters": "[^\"\\\\]+",
  "raw string chars":  "\"\"\"\"{0,2}([^\"]+\"{1,2})*[^\"]*\"\"\"",
  "int part":          "(0|[1-9][0-9_]*)",
  "fraction part":     "[0-9_]+",
  "hex int":           "[a-fA-F0-9]+",
};
const IN = {
  "block comment":     ["{- a - b -} tail", "{- x", "no comment"],
  "line comment":      ["-- hi\nrest", "--\nx", "code"],
  "shebang":           ["#!/usr/bin/env x\nrest", "#!", "x"],
  "spaces":            ["   x", " ", "x"],
  "newline LF":        ["\n\nx", "\n", "x"],
  "newline CRLF":      ["\r\n\r\nx", "\r\n", "\rx"],
  "module name":       ["Data.List.rest", "Foo.", "x", ""],
  "proper name":       ["Über.rest", "Foo9_'x", "lower"],
  "ident":             ["año_3'", "_x", "Upper"],
  "symbol":            ["<=>rest", "+", "±§rest", "a"],
  "hex":               ["deadBEEF12", "0", "xyz"],
  "whitespace escape": ["\\  \n \\rest", "\\x"],
  "string characters": ["abc\"x", "a\\b", "\""],
  "raw string chars":  ["\"\"\"hi\"\"\"x", "\"\"\"a\"b\"\"\"", "\"\"\"\"\"\""],
  "int part":          ["1_000x", "0", "0123", "x"],
  "fraction part":     ["0_5x", "9", "x"],
  "hex int":           ["a1B2z", "0", "g"],
};
const ps = (s) => JSON.stringify(s);
const psMaybe = (x) => (x === null || x === undefined) ? "Nothing" : "Just " + ps(x);
const psRes = (m) => m === null ? "Nothing"
  : "Just [ " + Array.from(m, g => psMaybe(g === undefined ? null : g)).join(", ") + " ]";

const L = [];
L.push('  -- GENERATED from a live node `RegExp` oracle (gate 2, ADR-0081 §3) — the authoritative');
L.push('  -- ES semantics. Regenerate: node test/gen-oracle.mjs (script committed). Every one of');
L.push('  -- PureScript.CST.Lexer\'s demanding patterns, wrapped `^(?:…)` as the Lexer does.');
L.push('  describe "CST Lexer patterns vs the JS RegExp oracle" do');
for (const [name, p] of Object.entries(P)) {
  const full = "^(?:" + p + ")";
  const re = new RegExp(full, "u");
  L.push(`    describe ${ps(name)} do`);
  IN[name].forEach((inp, i) => {
    L.push(`      it ${ps("case " + (i + 1))} do`);
    L.push(`        match (compile' ${ps(full)}) ${ps(inp)}`);
    L.push(`          \`shouldEqual\` ${psRes(inp.match(re))}`);
  });
}
console.log(L.join("\n"));
