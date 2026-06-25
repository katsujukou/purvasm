-- | ulib SHADOW of `prelude`'s `Data.Show` (ADR-0038 / ADR-0006), targeting prelude 6.0.2.
-- |
-- | `showIntImpl` / `showCharImpl` / `showStringImpl` / `showArrayImpl` are reimplemented in
-- | PureScript over the byte-level `Purvasm.String` / `Purvasm.Char` / `Purvasm.Array` primitives, so they
-- | run standalone on purvasm and the element-`show` of `showArrayImpl` *specializes* (ADR-0027). The
-- | escaping matches `prelude`'s reference JS (`\a\b\f\n\r\t\v`, `\NNN` decimal with a `\&`
-- | separator before a following digit). `showStringImpl` escapes only ASCII bytes in
-- | `[\0-\x1F\x7F"\\]` and passes every other byte through, so multi-byte UTF-8 is preserved.
-- |
-- | `showNumberImpl` is KEPT foreign: `Number` → shortest round-trip string needs a Ryu/Grisu-class
-- | algorithm, so it is resolved by the purvasm native host rather than reimplemented here. The
-- | public interface is unchanged. As a sub-`Prelude` module it cannot `import Prelude` (cycle), so
-- | it imports the specific operator modules directly.
module Data.Show
  ( class Show
  , show
  , class ShowRecordFields
  , showRecordFields
  ) where

import Data.Semigroup ((<>))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit)
import Data.Void (Void, absurd)
import Prim.Row (class Nub)
import Prim.RowList as RL
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))
import Purvasm.Array as PA
import Purvasm.Char as PC
import Purvasm.Int as PI
import Purvasm.String as PS

-- | The `Show` type class represents those types which can be converted into
-- | a human-readable `String` representation.
class Show a where
  show :: a -> String

instance showUnit :: Show Unit where
  show _ = "unit"

instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"

instance showInt :: Show Int where
  show = showIntImpl

instance showNumber :: Show Number where
  show = showNumberImpl

instance showChar :: Show Char where
  show = showCharImpl

instance showString :: Show String where
  show = showStringImpl

instance showArray :: Show a => Show (Array a) where
  show = showArrayImpl show

instance showProxy :: Show (Proxy a) where
  show _ = "Proxy"

instance showVoid :: Show Void where
  show = absurd

instance showRecord ::
  ( Nub rs rs
  , RL.RowToList rs ls
  , ShowRecordFields ls rs
  ) =>
  Show (Record rs) where
  show record = "{" <> showRecordFields (Proxy :: Proxy ls) record <> "}"

-- | A class for records where all fields have `Show` instances, used to
-- | implement the `Show` instance for records.
class ShowRecordFields :: RL.RowList Type -> Row Type -> Constraint
class ShowRecordFields rowlist row where
  showRecordFields :: Proxy rowlist -> Record row -> String

instance showRecordFieldsNil :: ShowRecordFields RL.Nil row where
  showRecordFields _ _ = ""
else instance showRecordFieldsConsNil ::
  ( IsSymbol key
  , Show focus
  ) =>
  ShowRecordFields (RL.Cons key focus RL.Nil) row where
  showRecordFields _ record = " " <> key <> ": " <> show focus <> " "
    where
    key = reflectSymbol (Proxy :: Proxy key)
    focus = unsafeGet key record :: focus
else instance showRecordFieldsCons ::
  ( IsSymbol key
  , ShowRecordFields rowlistTail row
  , Show focus
  ) =>
  ShowRecordFields (RL.Cons key focus rowlistTail) row where
  showRecordFields _ record = " " <> key <> ": " <> show focus <> "," <> tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    focus = unsafeGet key record :: focus
    tail = showRecordFields (Proxy :: Proxy rowlistTail) record

-------------------------------------------------------------------------------
-- ulib shadow implementations (UTF-8, code-point semantics; ADR-0006). String building uses `<>`
-- (`StrConcat`); show is not a hot path, so its O(n²) accumulation is acceptable.
-------------------------------------------------------------------------------

-- | A one-byte string of the raw byte `b` (an ASCII char, or one byte of a UTF-8 sequence).
put1 :: Int -> String
put1 b = PS.unsafeSetByte (PS.unsafeNew 1) 0 b

-- | Encode a single code point as its UTF-8 string.
cpStr :: Int -> String
cpStr cp =
  if PI.lt cp 0x80 then put1 cp
  else if PI.lt cp 0x800 then put2 (PI.add 0xC0 (PI.div cp 64)) (cont cp)
  else if PI.lt cp 0x10000 then put3 (PI.add 0xE0 (PI.div cp 4096)) (PI.add 0x80 (PI.mod (PI.div cp 64) 64)) (cont cp)
  else put4 (PI.add 0xF0 (PI.div cp 262144)) (PI.add 0x80 (PI.mod (PI.div cp 4096) 64)) (PI.add 0x80 (PI.mod (PI.div cp 64) 64)) (cont cp)
  where
  cont x = PI.add 0x80 (PI.mod x 64)
  put2 a b = PS.unsafeSetByte (PS.unsafeSetByte (PS.unsafeNew 2) 0 a) 1 b
  put3 a b c = PS.unsafeSetByte (PS.unsafeSetByte (PS.unsafeSetByte (PS.unsafeNew 3) 0 a) 1 b) 2 c
  put4 a b c d = PS.unsafeSetByte (PS.unsafeSetByte (PS.unsafeSetByte (PS.unsafeSetByte (PS.unsafeNew 4) 0 a) 1 b) 2 c) 3 d

-- | The named C-style escape (`\a`/`\b`/…) for a control byte, or `fallback` for any other.
ctrlEscape :: Int -> String -> String
ctrlEscape b fallback =
  if PI.eq b 0x07 then "\\a"
  else if PI.eq b 0x08 then "\\b"
  else if PI.eq b 0x0C then "\\f"
  else if PI.eq b 0x0A then "\\n"
  else if PI.eq b 0x0D then "\\r"
  else if PI.eq b 0x09 then "\\t"
  else if PI.eq b 0x0B then "\\v"
  else fallback

-- | A byte `< 0x20` or `== 0x7F` — a control character needing an escape.
isCtrl :: Int -> Boolean
isCtrl b = if PI.lt b 0x20 then true else PI.eq b 0x7F

showIntImpl :: Int -> String
showIntImpl n =
  if PI.eq n 0 then "0"
  else if PI.eq n minInt then "-2147483648"
  else if PI.lt n 0 then "-" <> digits (PI.sub 0 n)
  else digits n
  where
  -- `-2147483648` cannot be written literally (the magnitude overflows `Int`, and unary `-` is
  -- `Data.Ring.negate`, not imported here); build it as `-(maxInt) - 1`.
  minInt = PI.sub (PI.sub 0 2147483647) 1
  digits m = go m ""
  go m acc = if PI.eq m 0 then acc else go (PI.div m 10) (put1 (PI.add 48 (PI.mod m 10)) <> acc)

showCharImpl :: Char -> String
showCharImpl c =
  if isCtrl code then "'" <> ctrlEscape code ("\\" <> showIntImpl code) <> "'"
  else if PI.eq code 0x27 then quoted
  else if PI.eq code 0x5C then quoted
  else "'" <> cpStr code <> "'"
  where
  code = PC.toCodePoint c
  quoted = "'\\" <> cpStr code <> "'"

showStringImpl :: String -> String
showStringImpl s = "\"" <> go 0 "" <> "\""
  where
  n = PS.byteLength s
  go i acc = if PI.lt i n then go (PI.add i 1) (acc <> render i) else acc
  render i =
    let
      b = PS.byteAt s i
    in
      if PI.eq b 0x22 then "\\\""
      else if PI.eq b 0x5C then "\\\\"
      else if isCtrl b then ctrlEscape b ("\\" <> showIntImpl b <> amp (PI.add i 1))
      else put1 b
  -- a digit immediately after a `\NNN` escape needs a `\&` separator (Haskell convention)
  amp k = if PI.lt k n then (if isDigit (PS.byteAt s k) then "\\&" else "") else ""
  isDigit bb = if PI.lt bb 0x30 then false else if PI.lt 0x39 bb then false else true

showArrayImpl :: forall a. (a -> String) -> Array a -> String
showArrayImpl f xs = "[" <> go 0 "" <> "]"
  where
  m = PA.length xs
  go i acc =
    if PI.lt i m then go (PI.add i 1) (if PI.eq i 0 then f (PA.unsafeIndex xs i) else acc <> "," <> f (PA.unsafeIndex xs i))
    else acc

-- | `Number` → string (kept foreign — shortest round-trip needs a Ryu/Grisu-class algorithm).
foreign import showNumberImpl :: Number -> String
