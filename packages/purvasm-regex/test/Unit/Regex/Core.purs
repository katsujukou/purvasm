-- | Floor-coverage units for `Regex.Core` (ADR-0081 §3), including several of the fourteen
-- | demanding Lexer patterns with expectations captured from a JS `RegExp` oracle.
module Test.Unit.Regex.Core (main) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Regex.Core (Regex, compile, match, test)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- Compile-or-crash for the known-good fixtures (mirrors `unsafeRegex` usage).
compile' :: String -> Regex
compile' src = case compile src of
  Right r -> r
  Left e -> unsafeCrashWith e

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "compile" do
    it "rejects out-of-floor syntax loudly" do
      isLeft (compile "a*?") `shouldEqual` true -- lazy
      isLeft (compile "(?<=x)y") `shouldEqual` true -- lookbehind
      isLeft (compile "(?=x)y") `shouldEqual` true -- positive lookahead
      isLeft (compile "\\p{Sc}") `shouldEqual` true -- category outside the floor
      isLeft (compile "\\d") `shouldEqual` true -- class escapes not in the floor
    it "accepts the floor" do
      (compile "^(?:a|bc)*[x-z]{1,3}$" # map (const unit)) `shouldEqual` Right unit

  describe "match" do
    it "literals, sequence, alternation (leftmost-first)" do
      match (compile' "ab|a") "abc" `shouldEqual` Just [ Just "ab" ]
    it "greedy quantifiers backtrack" do
      match (compile' "a*ab") "aaab" `shouldEqual` Just [ Just "aaab" ]
    it "bounded repetition" do
      match (compile' "^[a-f0-9]{1,3}") "abcd12" `shouldEqual` Just [ Just "abc" ]
    it "capturing groups and non-participation" do
      match (compile' "(a)|(b)") "b" `shouldEqual` Just [ Just "b", Nothing, Just "b" ]
    it "anchors" do
      match (compile' "^b") "ab" `shouldEqual` Nothing
      match (compile' "b$") "ab" `shouldEqual` Just [ Just "b" ]
    it "negated classes and dot" do
      match (compile' "^[^x]+") "abx" `shouldEqual` Just [ Just "ab" ]
      match (compile' "^.$") "\n" `shouldEqual` Nothing
    it "negative lookahead" do
      match (compile' "^a(?!b)") "ab" `shouldEqual` Nothing
      match (compile' "^a(?!b)") "ac" `shouldEqual` Just [ Just "a" ]
    it "unicode categories" do
      test (compile' "^\\p{Lu}$") "Å" `shouldEqual` true
      test (compile' "^\\p{Lu}$") "å" `shouldEqual` false
      test (compile' "^[\\p{L}0-9_']+$") "año_3'" `shouldEqual` true
  -- GENERATED from a live node `RegExp` oracle (gate 2, ADR-0081 §3) — the authoritative
  -- ES semantics. Regenerate: node test/gen-oracle.mjs (script committed). Every one of
  -- PureScript.CST.Lexer's demanding patterns, wrapped `^(?:…)` as the Lexer does.
  describe "CST Lexer patterns vs the JS RegExp oracle" do
    describe "block comment" do
      it "case 1" do
        match (compile' "^(?:\\{-(-(?!\\})|[^-]+)*(-\\}|$))") "{- a - b -} tail"
          `shouldEqual` Just [ Just "{- a - b -}", Just " b ", Just "-}" ]
      it "case 2" do
        match (compile' "^(?:\\{-(-(?!\\})|[^-]+)*(-\\}|$))") "{- x"
          `shouldEqual` Just [ Just "{- x", Just " x", Just "" ]
      it "case 3" do
        match (compile' "^(?:\\{-(-(?!\\})|[^-]+)*(-\\}|$))") "no comment"
          `shouldEqual` Nothing
    describe "line comment" do
      it "case 1" do
        match (compile' "^(?:--[^\\r\\n]*)") "-- hi\nrest"
          `shouldEqual` Just [ Just "-- hi" ]
      it "case 2" do
        match (compile' "^(?:--[^\\r\\n]*)") "--\nx"
          `shouldEqual` Just [ Just "--" ]
      it "case 3" do
        match (compile' "^(?:--[^\\r\\n]*)") "code"
          `shouldEqual` Nothing
    describe "shebang" do
      it "case 1" do
        match (compile' "^(?:#![^\\r\\n]*)") "#!/usr/bin/env x\nrest"
          `shouldEqual` Just [ Just "#!/usr/bin/env x" ]
      it "case 2" do
        match (compile' "^(?:#![^\\r\\n]*)") "#!"
          `shouldEqual` Just [ Just "#!" ]
      it "case 3" do
        match (compile' "^(?:#![^\\r\\n]*)") "x"
          `shouldEqual` Nothing
    describe "spaces" do
      it "case 1" do
        match (compile' "^(?: +)") "   x"
          `shouldEqual` Just [ Just "   " ]
      it "case 2" do
        match (compile' "^(?: +)") " "
          `shouldEqual` Just [ Just " " ]
      it "case 3" do
        match (compile' "^(?: +)") "x"
          `shouldEqual` Nothing
    describe "newline LF" do
      it "case 1" do
        match (compile' "^(?:\\n+)") "\n\nx"
          `shouldEqual` Just [ Just "\n\n" ]
      it "case 2" do
        match (compile' "^(?:\\n+)") "\n"
          `shouldEqual` Just [ Just "\n" ]
      it "case 3" do
        match (compile' "^(?:\\n+)") "x"
          `shouldEqual` Nothing
    describe "newline CRLF" do
      it "case 1" do
        match (compile' "^(?:(?:\\r\\n)+)") "\r\n\r\nx"
          `shouldEqual` Just [ Just "\r\n\r\n" ]
      it "case 2" do
        match (compile' "^(?:(?:\\r\\n)+)") "\r\n"
          `shouldEqual` Just [ Just "\r\n" ]
      it "case 3" do
        match (compile' "^(?:(?:\\r\\n)+)") "\rx"
          `shouldEqual` Nothing
    describe "module name" do
      it "case 1" do
        match (compile' "^(?:(?:(?:\\p{Lu}[\\p{L}0-9_']*)\\.)*)") "Data.List.rest"
          `shouldEqual` Just [ Just "Data.List." ]
      it "case 2" do
        match (compile' "^(?:(?:(?:\\p{Lu}[\\p{L}0-9_']*)\\.)*)") "Foo."
          `shouldEqual` Just [ Just "Foo." ]
      it "case 3" do
        match (compile' "^(?:(?:(?:\\p{Lu}[\\p{L}0-9_']*)\\.)*)") "x"
          `shouldEqual` Just [ Just "" ]
      it "case 4" do
        match (compile' "^(?:(?:(?:\\p{Lu}[\\p{L}0-9_']*)\\.)*)") ""
          `shouldEqual` Just [ Just "" ]
    describe "proper name" do
      it "case 1" do
        match (compile' "^(?:\\p{Lu}[\\p{L}0-9_']*)") "Über.rest"
          `shouldEqual` Just [ Just "Über" ]
      it "case 2" do
        match (compile' "^(?:\\p{Lu}[\\p{L}0-9_']*)") "Foo9_'x"
          `shouldEqual` Just [ Just "Foo9_'x" ]
      it "case 3" do
        match (compile' "^(?:\\p{Lu}[\\p{L}0-9_']*)") "lower"
          `shouldEqual` Nothing
    describe "ident" do
      it "case 1" do
        match (compile' "^(?:[\\p{Ll}_][\\p{L}0-9_']*)") "año_3'"
          `shouldEqual` Just [ Just "año_3'" ]
      it "case 2" do
        match (compile' "^(?:[\\p{Ll}_][\\p{L}0-9_']*)") "_x"
          `shouldEqual` Just [ Just "_x" ]
      it "case 3" do
        match (compile' "^(?:[\\p{Ll}_][\\p{L}0-9_']*)") "Upper"
          `shouldEqual` Nothing
    describe "symbol" do
      it "case 1" do
        match (compile' "^(?:(?:[:!#$%&*+./<=>?@\\^|~-]|(?!\\p{P})\\p{S})+)") "<=>rest"
          `shouldEqual` Just [ Just "<=>" ]
      it "case 2" do
        match (compile' "^(?:(?:[:!#$%&*+./<=>?@\\^|~-]|(?!\\p{P})\\p{S})+)") "+"
          `shouldEqual` Just [ Just "+" ]
      it "case 3" do
        match (compile' "^(?:(?:[:!#$%&*+./<=>?@\\^|~-]|(?!\\p{P})\\p{S})+)") "±§rest"
          `shouldEqual` Just [ Just "±" ]
      it "case 4" do
        match (compile' "^(?:(?:[:!#$%&*+./<=>?@\\^|~-]|(?!\\p{P})\\p{S})+)") "a"
          `shouldEqual` Nothing
    describe "hex" do
      it "case 1" do
        match (compile' "^(?:[a-fA-F0-9]{1,6})") "deadBEEF12"
          `shouldEqual` Just [ Just "deadBE" ]
      it "case 2" do
        match (compile' "^(?:[a-fA-F0-9]{1,6})") "0"
          `shouldEqual` Just [ Just "0" ]
      it "case 3" do
        match (compile' "^(?:[a-fA-F0-9]{1,6})") "xyz"
          `shouldEqual` Nothing
    describe "whitespace escape" do
      it "case 1" do
        match (compile' "^(?:\\\\[ \\r\\n]+\\\\)") "\\  \n \\rest"
          `shouldEqual` Just [ Just "\\  \n \\" ]
      it "case 2" do
        match (compile' "^(?:\\\\[ \\r\\n]+\\\\)") "\\x"
          `shouldEqual` Nothing
    describe "string characters" do
      it "case 1" do
        match (compile' "^(?:[^\"\\\\]+)") "abc\"x"
          `shouldEqual` Just [ Just "abc" ]
      it "case 2" do
        match (compile' "^(?:[^\"\\\\]+)") "a\\b"
          `shouldEqual` Just [ Just "a" ]
      it "case 3" do
        match (compile' "^(?:[^\"\\\\]+)") "\""
          `shouldEqual` Nothing
    describe "raw string chars" do
      it "case 1" do
        match (compile' "^(?:\"\"\"\"{0,2}([^\"]+\"{1,2})*[^\"]*\"\"\")") "\"\"\"hi\"\"\"x"
          `shouldEqual` Just [ Just "\"\"\"hi\"\"\"", Nothing ]
      it "case 2" do
        match (compile' "^(?:\"\"\"\"{0,2}([^\"]+\"{1,2})*[^\"]*\"\"\")") "\"\"\"a\"b\"\"\""
          `shouldEqual` Just [ Just "\"\"\"a\"b\"\"\"", Just "a\"" ]
      it "case 3" do
        match (compile' "^(?:\"\"\"\"{0,2}([^\"]+\"{1,2})*[^\"]*\"\"\")") "\"\"\"\"\"\""
          `shouldEqual` Just [ Just "\"\"\"\"\"\"", Nothing ]
    describe "int part" do
      it "case 1" do
        match (compile' "^(?:(0|[1-9][0-9_]*))") "1_000x"
          `shouldEqual` Just [ Just "1_000", Just "1_000" ]
      it "case 2" do
        match (compile' "^(?:(0|[1-9][0-9_]*))") "0"
          `shouldEqual` Just [ Just "0", Just "0" ]
      it "case 3" do
        match (compile' "^(?:(0|[1-9][0-9_]*))") "0123"
          `shouldEqual` Just [ Just "0", Just "0" ]
      it "case 4" do
        match (compile' "^(?:(0|[1-9][0-9_]*))") "x"
          `shouldEqual` Nothing
    describe "fraction part" do
      it "case 1" do
        match (compile' "^(?:[0-9_]+)") "0_5x"
          `shouldEqual` Just [ Just "0_5" ]
      it "case 2" do
        match (compile' "^(?:[0-9_]+)") "9"
          `shouldEqual` Just [ Just "9" ]
      it "case 3" do
        match (compile' "^(?:[0-9_]+)") "x"
          `shouldEqual` Nothing
    describe "hex int" do
      it "case 1" do
        match (compile' "^(?:[a-fA-F0-9]+)") "a1B2z"
          `shouldEqual` Just [ Just "a1B2" ]
      it "case 2" do
        match (compile' "^(?:[a-fA-F0-9]+)") "0"
          `shouldEqual` Just [ Just "0" ]
      it "case 3" do
        match (compile' "^(?:[a-fA-F0-9]+)") "g"
          `shouldEqual` Nothing
