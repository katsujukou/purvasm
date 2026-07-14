-- | Foreign signatures reconstructed from `.purs` source (ADR-0080): the pure pass.
-- |
-- | CoreFn is untyped and `purs` externs omit private declarations, so a foreign's calling
-- | shape — its arity, and whether it is an effectful leaf — is reconstructed from the one
-- | artifact that has it: the `foreign import x :: T` declaration in the module's source,
-- | parsed with `language-cst-parser`. This module is pure (`String` in, shapes out); source
-- | *location* (spago's `cache-db.json`, ulib patch provenance) is the driver's concern.
-- |
-- | The type-to-shape interpretation is the ADR-0080 §2 contract, as the dual per-value
-- | summary the effect analysis actually consumes (ADR-0034: `vsat`/`ret_vsat`) rather than a
-- | single lossy `effectful` bit:
-- |
-- |   * `arity` = the number of top-level `->` arrows (after peeling `forall`s, parens,
-- |     constraints, kinds), **plus** the uncurried arity of a `Fn{N}`/`EffectFn{N}`/`STFn{N}`
-- |     return head — purvasm has no packed-uncurried representation, so an `FnN`/`EffectFnN`
-- |     value *is* an ordinary N-ary curried closure (`mkFnN = identity`, ADR-0039);
-- |   * `vsat` = does **saturating** this value perform? True for a bare `EffectFn{N}`/`STFn{N}`
-- |     value, whose saturated application runs the effect (boot's `mkSTFnN f = \x… -> run_eff
-- |     (f x…)`, ADR-0039) — the [0034](0034) I1 force point is the saturation itself;
-- |   * `ret_vsat` = is the **saturated result** an effect thunk (fires at a *later* force)?
-- |     True for an `Effect r`/`ST r t` return head — the ordinary perform-leaf shape.
-- |
-- | Everything else is opaque (both bits false). The CST is not desugared, so a type synonym
-- | aliasing `Effect`/`EffectFnN` is not seen through (accepted: arity from arrows stays
-- | correct, and this project controls its ulib sources); the head-constructor check ignores
-- | the qualifier so `E.Effect` / `EU.EffectFn2` under a qualified import still count.
module Purvasm.Compiler.ForeignSig
  ( ForeignShape
  , ModuleSigs
  , SigIssue(..)
  , reconstructModule
  , interpretType
  , shapeToJson
  , shapeFromJson
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromBoolean, fromNumber, fromObject, toBoolean, toNumber, toObject)
import Data.Array (mapMaybe)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Foreign.Object as Object
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Types (Declaration(..), Foreign(..), Module(..), Type(..)) as CST

-- | A foreign's calling shape (ADR-0080 §2), the ADR-0034 dual summary. `arity` is the **semantic**
-- | arity — the source **arrow count** — consumed *verbatim* by the effect-placement analysis (ADR-0034;
-- | a nullary `Effect a` leaf is semantic arity 0). It is deliberately **not** the *physical* closure
-- | arity: a native `Effect`/`ST` leaf is a thunk, so a nullary one *is* the effect action and its
-- | closure takes the unit-run (physical arity 1). The `AForeign` lowering derives that physical arity
-- | from the shape (`Backend.LLVM.Driver.leafClosureArity`); the correction lives **there**, not here —
-- | folding it back into `arity` would hand EffectAnalysis the wrong semantic arity. `vsat` (saturating
-- | performs) / `retVsat` (the saturated result is a later-forced effect thunk) are the effect bits.
type ForeignShape = { arity :: Int, vsat :: Boolean, retVsat :: Boolean }

-- | One module's reconstructed foreign surface: the header's module name (dotted) and the
-- | value foreigns' shapes by bare identifier (`foreign import data` declares types, not
-- | values, and is skipped). Qualified keys are the caller's convention to apply.
type ModuleSigs = { moduleName :: String, sigs :: Array (String /\ ForeignShape) }

-- | Why a module yields no signatures. A hard diagnostic by design (ADR-0080 §1): the
-- | silent-default failure mode is the thing this pass retires. A recovering parse
-- | (`ParseSucceededWithErrors`) is treated as failure too — a source that `purs` compiled
-- | parses cleanly, so recovered errors mean the tree is not the compiled module's.
data SigIssue = SigParseFailed String

derive instance eqSigIssue :: Eq SigIssue

instance showSigIssue :: Show SigIssue where
  show (SigParseFailed msg) = "(SigParseFailed " <> show msg <> ")"

-- | Reconstruct a module's foreign shapes from its source text.
reconstructModule :: String -> Either SigIssue ModuleSigs
reconstructModule src = case parseModule src of
  ParseSucceeded (CST.Module { header, body }) ->
    let
      name = unwrap (unwrap (unwrap header).name).name
      decls = (unwrap body).decls
    in
      Right { moduleName: name, sigs: mapMaybe foreignSig decls }
  ParseSucceededWithErrors _ errs ->
    Left
      ( SigParseFailed
          ("recovered parse error: " <> printParseError (NEA.head errs).error)
      )
  ParseFailed err -> Left (SigParseFailed (printParseError err.error))
  where
  foreignSig = case _ of
    CST.DeclForeign _ _ (CST.ForeignValue labeled) ->
      let
        l = unwrap labeled
      in
        Just (unwrap (unwrap l.label).name /\ interpretType l.value)
    _ -> Nothing

-- | The ADR-0080 §2 interpretation, total over a successfully parsed type (`Type Void` has
-- | no error nodes): peel the transparent wrappers, count top-level arrows, then classify the
-- | return head into the ADR-0034 dual summary (§2 head-family table).
interpretType :: CST.Type Void -> ForeignShape
interpretType = go 0
  where
  go :: Int -> CST.Type Void -> ForeignShape
  go arrows ty = case peel ty of
    CST.TypeArrow _ _ rest -> go (arrows + 1) rest
    ret -> case classifyHead (headName ret) of
      -- an uncurried `EffectFn{N}`/`STFn{N}` value: its N-arg saturation runs the effect
      VsatFamily n -> { arity: arrows + n, vsat: true, retVsat: false }
      -- a pure `Fn{N}` value: N extra curried args, no effect
      PureFamily n -> { arity: arrows + n, vsat: false, retVsat: false }
      -- an ordinary perform leaf: the saturated result is a later-forced effect thunk
      EffectReturn -> { arity: arrows, vsat: false, retVsat: true }
      Opaque -> { arity: arrows, vsat: false, retVsat: false }

  -- Transparent syntax around the structural shape: quantifiers, parens, constraints
  -- (disallowed on foreigns by `purs`, peeled defensively), kind annotations.
  peel :: CST.Type Void -> CST.Type Void
  peel = case _ of
    CST.TypeForall _ _ _ t -> peel t
    CST.TypeParens w -> peel (unwrap w).value
    CST.TypeConstrained _ _ t -> peel t
    CST.TypeKinded t _ _ -> peel t
    t -> t

  -- The head constructor name of an application spine (`EffectFn2 a b c` → "EffectFn2"),
  -- qualifier ignored; a non-constructor head (a bare type variable, a record) yields "".
  headName :: CST.Type Void -> String
  headName t = case peel t of
    CST.TypeApp f _ -> headName f
    CST.TypeConstructor qn -> unwrap (unwrap qn).name
    _ -> ""

data HeadClass
  = EffectReturn -- `Effect`/`ST` head: ret_vsat
  | VsatFamily Int -- `EffectFn{N}`/`STFn{N}`: vsat, N uncurried args
  | PureFamily Int -- `Fn{N}`: pure, N uncurried args
  | Opaque

-- | Classify a return-head constructor (ADR-0080 §2 table). The `EffectFn{N}`/`STFn{N}`/`Fn{N}`
-- | families are matched by prefix + trailing arity digits (most specific prefix first, so
-- | `EffectFn2`/`STFn2` are not mistaken for `Fn2`); `Effect`/`ST` are the exact perform heads.
classifyHead :: String -> HeadClass
classifyHead name = case family "EffectFn", family "STFn", family "Fn" of
  Just n, _, _ -> VsatFamily n
  _, Just n, _ -> VsatFamily n
  _, _, Just n -> PureFamily n
  _, _, _
    | name == "Effect" || name == "ST" -> EffectReturn
    | otherwise -> Opaque
  where
  family pfx = String.stripPrefix (String.Pattern pfx) name >>= Int.fromString

-- --- JSON codec (ADR-0080 §1): the shape as it rides in `ulib.json`'s `foreignSigs` -------

-- | A `ForeignShape` as a JSON object `{ "arity": n, "vsat": b, "retVsat": b }` — the form a
-- | `ulib.json` declares (author-written, validated by `ulib-tools build` against the source
-- | reconstruction) and the compiler driver reads for a ulib-overlaid module.
shapeToJson :: ForeignShape -> Json
shapeToJson s = fromObject $ Object.fromFoldable
  [ "arity" /\ fromNumber (Int.toNumber s.arity)
  , "vsat" /\ fromBoolean s.vsat
  , "retVsat" /\ fromBoolean s.retVsat
  ]

shapeFromJson :: Json -> Either String ForeignShape
shapeFromJson json = do
  obj <- note "foreignSigs: shape is not an object" (toObject json)
  arityN <- note "foreignSigs: 'arity' missing/not a number"
    (Object.lookup "arity" obj >>= toNumber)
  vsat <- note "foreignSigs: 'vsat' missing/not a boolean"
    (Object.lookup "vsat" obj >>= toBoolean)
  retVsat <- note "foreignSigs: 'retVsat' missing/not a boolean"
    (Object.lookup "retVsat" obj >>= toBoolean)
  pure { arity: Int.round arityN, vsat, retVsat }
