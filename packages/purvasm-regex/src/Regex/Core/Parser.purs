-- | The pattern parser (ADR-0081 §1): pattern source → `Node`, floor-only. Anything outside
-- | the demanded floor is a structured `Left` at construction time — never a silently-wrong
-- | match later. The grammar follows ES `u`-mode Patterns restricted to the floor.
module Regex.Core.Parser
  ( parse
  , ParseResult
  ) where

import Prelude

import Data.Array (length, snoc, unsafeIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Regex.Core.Ast (ClsItem(..), Node(..))
import Regex.Core.Unicode (Category(..))
import Regex.Core.Utf8 (fromCodePoints, toCodePoints)

-- | A parsed pattern and its capturing-group count (for the `match` result array width).
type ParseResult = { node :: Node, ngroups :: Int }

type St = { cps :: Array Int, pos :: Int, ngroups :: Int }

parse :: String -> Either String ParseResult
parse src = do
  let cps = toCodePoints src
  r <- alternation { cps, pos: 0, ngroups: 0 }
  if r.st.pos < length r.st.cps then
    Left ("regex: unbalanced `)` at " <> show r.st.pos)
  else
    Right { node: r.node, ngroups: r.st.ngroups }

-- Character constants, by code point.
cLparen = 40 :: Int
cRparen = 41 :: Int
cStar = 42 :: Int
cPlus = 43 :: Int
cDot = 46 :: Int
cQuestion = 63 :: Int
cLbrack = 91 :: Int
cBackslash = 92 :: Int
cRbrack = 93 :: Int
cCaret = 94 :: Int
cLbrace = 123 :: Int
cPipe = 124 :: Int
cRbrace = 125 :: Int
cDollar = 36 :: Int
cComma = 44 :: Int
cMinus = 45 :: Int
cBang = 33 :: Int
cColon = 58 :: Int

peek :: St -> Maybe Int
peek st =
  if st.pos < length st.cps then Just (unsafePartial (unsafeIndex st.cps st.pos))
  else Nothing

advance :: St -> St
advance st = st { pos = st.pos + 1 }

showCp :: Int -> String
showCp cp = "`" <> fromCodePoints [ cp ] <> "`"

-- Alternation: sequence ('|' sequence)*
alternation :: St -> Either String { node :: Node, st :: St }
alternation st0 = do
  first <- sequenceP st0
  go [ first.node ] first.st
  where
  go acc st = case peek st of
    Just c | c == cPipe -> do
      nxt <- sequenceP (advance st)
      go (snoc acc nxt.node) nxt.st
    _ ->
      Right { node: if length acc == 1 then unsafePartial (unsafeIndex acc 0) else Alt acc, st }

-- Sequence: (atom quantifier?)* until '|' or ')' or end.
sequenceP :: St -> Either String { node :: Node, st :: St }
sequenceP = go []
  where
  go acc st = case peek st of
    Nothing -> done acc st
    Just c
      | c == cPipe || c == cRparen -> done acc st
      | otherwise -> do
          a <- atom st
          q <- quantified a.node a.st
          go (snoc acc q.node) q.st
  done acc st =
    Right { node: if length acc == 1 then unsafePartial (unsafeIndex acc 0) else Seq acc, st }

-- One atom, then an optional quantifier over it.
quantified :: Node -> St -> Either String { node :: Node, st :: St }
quantified node st = case peek st of
  Just c
    | c == cStar -> lazyCheck (Rep node 0 Nothing) (advance st)
    | c == cPlus -> lazyCheck (Rep node 1 Nothing) (advance st)
    | c == cQuestion -> lazyCheck (Rep node 0 (Just 1)) (advance st)
    | c == cLbrace -> do
        b <- braces (advance st)
        lazyCheck (Rep node b.min b.max) b.st
  _ -> Right { node, st }
  where
  -- The floor has no lazy quantifiers: `*?` etc. is a loud construction error (ADR-0081 §1).
  lazyCheck n st' = case peek st' of
    Just c | c == cQuestion -> Left "regex: lazy quantifiers are outside the ADR-0081 floor"
    _ -> Right { node: n, st: st' }

-- `{m}` | `{m,}` | `{m,n}` (already past '{').
braces :: St -> Either String { min :: Int, max :: Maybe Int, st :: St }
braces st0 = do
  m <- int st0
  case peek m.st of
    Just c
      | c == cRbrace -> Right { min: m.value, max: Just m.value, st: advance m.st }
      | c == cComma -> case peek (advance m.st) of
          Just c2 | c2 == cRbrace -> Right { min: m.value, max: Nothing, st: advance (advance m.st) }
          _ -> do
            n <- int (advance m.st)
            case peek n.st of
              Just c3 | c3 == cRbrace ->
                Right { min: m.value, max: Just n.value, st: advance n.st }
              _ -> Left "regex: malformed `{m,n}` quantifier"
    _ -> Left "regex: malformed `{m}` quantifier"
  where
  int st = go 0 false st
    where
    go acc seen s = case peek s of
      Just c | c >= 48 && c <= 57 -> go (acc * 10 + (c - 48)) true (advance s)
      _ -> if seen then Right { value: acc, st: s } else Left "regex: expected a number in `{}`"

-- One atom: group / class / escape / anchor / dot / literal.
atom :: St -> Either String { node :: Node, st :: St }
atom st = case peek st of
  Nothing -> Left "regex: expected an atom at end of pattern"
  Just c
    | c == cLparen -> group (advance st)
    | c == cLbrack -> charClass (advance st)
    | c == cBackslash -> escape (advance st) false
    | c == cCaret -> Right { node: AnchorStart, st: advance st }
    | c == cDollar -> Right { node: AnchorEnd, st: advance st }
    | c == cDot -> Right { node: Dot, st: advance st }
    | c == cStar || c == cPlus || c == cQuestion || c == cRbrack
        || c == cLbrace
        || c == cRbrace ->
        Left ("regex: unexpected " <> showCp c <> " (quantifier without an atom?)")
    | otherwise -> Right { node: Lit c, st: advance st }

-- `(?:…)`, `(?!…)`, `(?=…)` (rejected), or a capturing `(…)`.
group :: St -> Either String { node :: Node, st :: St }
group st = case peek st of
  Just c | c == cQuestion -> case peek (advance st) of
    Just c2
      | c2 == cColon -> do
          r <- alternation (advance (advance st))
          close r.st (\st' -> { node: r.node, st: st' })
      | c2 == cBang -> do
          r <- alternation (advance (advance st))
          close r.st (\st' -> { node: NegAhead r.node, st: st' })
      | c2 == 61 {- '=' -} ->
          Left "regex: positive lookahead is outside the ADR-0081 floor"
      | c2 == 60 {- '<' -} ->
          Left "regex: lookbehind / named groups are outside the ADR-0081 floor"
    _ -> Left "regex: malformed group"
  _ -> do
    -- ES numbers capturing groups by the order of their `(` — reserve the index BEFORE
    -- parsing the body so nested groups number correctly.
    let idx = st.ngroups + 1
    r <- alternation (st { ngroups = idx })
    close r.st (\st' -> { node: Group idx r.node, st: st' })
  where
  close st' k = case peek st' of
    Just c | c == cRparen -> Right (k (advance st'))
    _ -> Left "regex: missing `)`"

-- A character class `[…]` (already past '[').
charClass :: St -> Either String { node :: Node, st :: St }
charClass st0 = do
  let negated = peek st0 == Just cCaret
  let st1 = if negated then advance st0 else st0
  go [] st1 negated
  where
  go items st negated = case peek st of
    Nothing -> Left "regex: missing `]`"
    Just c
      | c == cRbrack -> Right { node: Cls negated items, st: advance st }
      | otherwise -> do
          lo <- classMember st
          case lo.item of
            CiCat _ -> go (snoc items lo.item) lo.st negated
            CiRange l _ -> case peek lo.st of
              -- `x-y` range, unless `-` is the last char before `]` (then literal).
              Just m
                | m == cMinus && peek (advance lo.st) /= Just cRbrack
                    && peek (advance lo.st) /= Nothing -> do
                    hi <- classMember (advance lo.st)
                    case hi.item of
                      CiRange h _ ->
                        if l <= h then go (snoc items (CiRange l h)) hi.st negated
                        else Left "regex: reversed range in character class"
                      CiCat _ -> Left "regex: `\\p{…}` cannot bound a class range"
              _ -> go (snoc items lo.item) lo.st negated

  -- One class member: an escape or a literal (returned as a degenerate range).
  classMember st = case peek st of
    Nothing -> Left "regex: missing `]`"
    Just c
      | c == cBackslash -> do
          e <- escape (advance st) true
          case e.node of
            Lit cp -> Right { item: CiRange cp cp, st: e.st }
            Cls _ [ ci ] -> Right { item: ci, st: e.st }
            _ -> Left "regex: unsupported escape in character class"
      | otherwise -> Right { item: CiRange c c, st: advance st }

-- An escape (already past '\'). `inClass` relaxes nothing in the floor; both contexts accept
-- the same set. ES `u`-mode identity escapes cover exactly the syntax characters.
escape :: St -> Boolean -> Either String { node :: Node, st :: St }
escape st _inClass = case peek st of
  Nothing -> Left "regex: dangling `\\`"
  Just c
    | c == 110 {- n -} -> Right { node: Lit 10, st: advance st }
    | c == 114 {- r -} -> Right { node: Lit 13, st: advance st }
    | c == 116 {- t -} -> Right { node: Lit 9, st: advance st }
    | c == 112 {- p -} -> property (advance st)
    | isSyntaxChar c -> Right { node: Lit c, st: advance st }
    | otherwise ->
        Left ("regex: escape `\\" <> showCp c <> "` is outside the ADR-0081 floor")
  where
  isSyntaxChar c =
    c == cCaret || c == cDollar || c == cBackslash || c == cDot || c == cStar
      || c == cPlus
      || c == cQuestion
      || c == cLparen
      || c == cRparen
      || c == cLbrack
      || c == cRbrack
      || c == cLbrace
      || c == cRbrace
      || c == cPipe
      || c == 47 {- / -}
      || c == cMinus

  -- `\p{Name}` — the five demanded general categories only (ADR-0081 §1).
  property st' = case peek st' of
    Just c | c == cLbrace -> do
      let name = takeName (advance st') []
      cat <- case name.chars of
        [ 76, 117 ] -> Right CatLu -- Lu
        [ 76, 108 ] -> Right CatLl -- Ll
        [ 76 ] -> Right CatLetter -- L
        [ 80 ] -> Right CatPunct -- P
        [ 83 ] -> Right CatSymbol -- S
        _ -> Left "regex: `\\p{…}` category outside the ADR-0081 floor (Lu, Ll, L, P, S)"
      case peek name.st of
        Just c2 | c2 == cRbrace ->
          Right { node: Cls false [ CiCat cat ], st: advance name.st }
        _ -> Left "regex: missing `}` in `\\p{…}`"
    _ -> Left "regex: `\\p` needs `{…}`"

  takeName s acc = case peek s of
    Just c | c /= cRbrace && c /= cBackslash && length acc < 8 ->
      takeName (advance s) (snoc acc c)
    _ -> { chars: acc, st: s }
