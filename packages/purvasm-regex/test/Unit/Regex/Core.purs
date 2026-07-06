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

  describe "the demanding Lexer patterns (JS oracle expectations)" do
    it "line comment" do
      match (compile' "^(?:--[^\\r\\n]*)") "-- hi\nrest"
        `shouldEqual` Just [ Just "-- hi" ]
    it "block comment (negative lookahead)" do
      match (compile' "^(?:\\{-(-(?!\\})|[^-]+)*(-\\}|$))") "{- a - b -} tail"
        `shouldEqual` Just [ Just "{- a - b -}", Just " b ", Just "-}" ]
    it "int part" do
      match (compile' "^(?:(0|[1-9][0-9_]*))") "1_000x"
        `shouldEqual` Just [ Just "1_000", Just "1_000" ]
    it "proper name" do
      match (compile' "^(?:\\p{Lu}[\\p{L}0-9_']*)") "Über.rest"
        `shouldEqual` Just [ Just "Über" ]
    it "crlf runs" do
      match (compile' "^(?:(?:\\r\\n)+)") "\r\n\r\nx"
        `shouldEqual` Just [ Just "\r\n\r\n" ]
