-- | Foreign-name resolution as an ordered provider ladder (ADR-0017/0020): a qualified
-- | foreign ident is tried intrinsic (eta-expanded primops) then structural (guest terms
-- | over first-order primitives), first match wins. Ported from boot's `Ffi`.
-- |
-- | The *native* rung (opaque host leaves, ADR-0022) is omitted here: the linker drops a
-- | native `TmForeign` exactly as it drops an unresolved name (host-resolved at run), so
-- | for producing `app.pvm` the two are indistinguishable. Hand-written CESK terms; they
-- | must match boot's verbatim for byte-identical linked images.
module Purvasm.Compiler.Ffi (resolver, intrinsicPrim, intrinsicTerm) where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (foldl, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Purvasm.Compiler.CESK.AST (Term(..))
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.Primitive (PrimOp(..))

-- helpers ------------------------------------------------------------------------------

v :: String -> Term
v = TmVar

intLit :: Int -> Term
intLit = TmLit <<< LInt

lams :: Array String -> Term -> Term
lams params body = foldr TmLam body params

-- | Eta-expand an arity-`n` primop into `\$0 -> … -> \$(n-1) -> Prim(op, [$0; …])`.
eta :: PrimOp -> Int -> Term
eta op arity =
  let
    names = map (\i -> "$" <> show i) (0 .. (arity - 1))
  in
    foldr TmLam (TmPrim op (map v names)) names

-- intrinsics (eta-expanded primops) ----------------------------------------------------

intDegree :: Term
intDegree =
  TmLam "$0"
    (TmIf (TmPrim LtInt [ v "$0", intLit 0 ]) (TmPrim SubInt [ intLit 0, v "$0" ]) (v "$0"))

-- | The identity function `\$0 -> $0` — a representation-preserving cast at runtime. Used for `Char`
-- | code conversions (`Char` is `Int`, ADR-0006) and for `unsafeCoerce`: the canonical `Unsafe.Coerce`
-- | plus the exact whitelisted private copies (`Data.Symbol`, `Data.Reflectable`), all intrinsified as the
-- | identity below (see the `unsafeCoerce` note at the `intrinsics` entries for why exact-key, not
-- | `ulib`-shadow-delegate).
identityFn :: Term
identityFn = TmLam "$0" (v "$0")

charId :: Term
charId = identityFn

intrinsics :: Map String Term
intrinsics = Map.fromFoldable
  -- The `purvasm-base` primitive layer (ADR-0038): `Purvasm.*` foreigns the backend recognises as
  -- intrinsics — the seam the `ulib` reimplementations build on. Mirrors boot's `Ffi`; without
  -- these the overlaid (patched-`ulib`) build's `Purvasm.Int.eq` etc. link to an unbound name.
  [ "Purvasm.Int.add" /\ eta AddInt 2
  , "Purvasm.Int.sub" /\ eta SubInt 2
  , "Purvasm.Int.mul" /\ eta MulInt 2
  , "Purvasm.Int.eq" /\ eta EqInt 2
  , "Purvasm.Int.lt" /\ eta LtInt 2
  , "Purvasm.Int.div" /\ eta DivInt 2
  , "Purvasm.Int.mod" /\ eta ModInt 2
  , "Purvasm.Int.and" /\ eta AndInt 2
  , "Purvasm.Int.or" /\ eta OrInt 2
  , "Purvasm.Int.xor" /\ eta XorInt 2
  , "Purvasm.Int.shl" /\ eta ShlInt 2
  , "Purvasm.Int.shr" /\ eta ShrInt 2
  , "Purvasm.Int.zshr" /\ eta ZshrInt 2
  , "Purvasm.Int.complement" /\ eta ComplementInt 1
  -- Cross-representation conversions (ADR-0041): `Int`<->`Number` casts. `fromNumber` is `ToInt32`.
  , "Purvasm.Int.toNumber" /\ eta IntToNumber 1
  , "Purvasm.Int.fromNumber" /\ eta NumberToInt 1
  , "Purvasm.Number.add" /\ eta AddNumber 2
  , "Purvasm.Number.sub" /\ eta SubNumber 2
  , "Purvasm.Number.mul" /\ eta MulNumber 2
  , "Purvasm.Number.div" /\ eta DivNumber 2
  , "Purvasm.Number.eq" /\ eta EqNumber 2
  , "Purvasm.Number.lt" /\ eta LtNumber 2
  , "Purvasm.Array.length" /\ eta LengthArray 1
  , "Purvasm.Array.unsafeIndex" /\ eta IndexArray 2
  , "Purvasm.Array.unsafeNew" /\ eta NewArray 1
  , "Purvasm.Array.unsafeSet" /\ eta SetArray 3
  , "Purvasm.Boolean.not" /\ eta NotBool 1
  -- `Record.Unsafe` dynamic record access by runtime label (ADR-0010 record-as-field-map): each a
  -- single record primop, mirroring boot's `Ffi`. (`unsafeUnionFn` → `RecordUnion` is added below with
  -- the primop.)
  , "Record.Unsafe.unsafeGet" /\ eta RecordGet 2
  , "Record.Unsafe.unsafeSet" /\ eta RecordSet 3
  , "Record.Unsafe.unsafeHas" /\ eta RecordHas 2
  , "Record.Unsafe.unsafeDelete" /\ eta RecordDelete 2
  , "Record.Unsafe.Union.unsafeUnionFn" /\ eta RecordUnion 2
  , "Purvasm.Char.toCodePoint" /\ charId
  , "Purvasm.Char.fromCodePoint" /\ charId
  -- stock-registry scalar leaves (used when building against stock, no-overlay corefn) --------
  , "Data.Semiring.intAdd" /\ eta AddInt 2
  , "Data.Semiring.intMul" /\ eta MulInt 2
  , "Data.Semiring.numAdd" /\ eta AddNumber 2
  , "Data.Semiring.numMul" /\ eta MulNumber 2
  , "Data.Ring.intSub" /\ eta SubInt 2
  , "Data.Ring.numSub" /\ eta SubNumber 2
  , "Data.EuclideanRing.intDiv" /\ eta DivInt 2
  , "Data.EuclideanRing.intMod" /\ eta ModInt 2
  , "Data.EuclideanRing.numDiv" /\ eta DivNumber 2
  , "Data.EuclideanRing.intDegree" /\ intDegree
  , "Data.Eq.eqIntImpl" /\ eta EqInt 2
  , "Data.Eq.eqNumberImpl" /\ eta EqNumber 2
  , "Data.Eq.eqStringImpl" /\ eta EqString 2
  , "Data.Eq.eqCharImpl" /\ eta EqInt 2
  , "Data.Eq.eqBooleanImpl" /\ eta EqBool 2
  , "Data.HeytingAlgebra.boolConj" /\ eta AndBool 2
  , "Data.HeytingAlgebra.boolDisj" /\ eta OrBool 2
  , "Data.HeytingAlgebra.boolNot" /\ eta NotBool 1
  , "Data.Semigroup.concatString" /\ eta Append 2
  -- `Data.Int.Bits` 32-bit bitwise ops (the `integers` package foreigns): each a single `Int`
  -- primop, like the `Data.Semiring`/`Data.Ring` scalar leaves above. The registry module is
  -- kept verbatim; its foreigns resolve here, reusing the same primops the `Purvasm.Int.*`
  -- base uses (mirrors boot's `Ffi`).
  , "Data.Int.Bits.and" /\ eta AndInt 2
  , "Data.Int.Bits.or" /\ eta OrInt 2
  , "Data.Int.Bits.xor" /\ eta XorInt 2
  , "Data.Int.Bits.shl" /\ eta ShlInt 2
  , "Data.Int.Bits.shr" /\ eta ShrInt 2
  , "Data.Int.Bits.zshr" /\ eta ZshrInt 2
  , "Data.Int.Bits.complement" /\ eta ComplementInt 1
  , "Data.Unit.unit" /\ intLit 0
  , "Prim.undefined" /\ intLit 0
  -- `unsafeCoerce` is a representation-preserving cast — the identity on purvasm's type-erased tagged
  -- words (ADR-0064), regardless of which module defines it. Its being a `foreign import` in PureScript
  -- is only the type-system escape hatch; there is no code. So every library's copy is intrinsified as
  -- the identity here (rather than `ulib`-shadowing each to delegate, which would add the very
  -- `unsafe-coerce` dependency those low-level modules deliberately avoid). Canonical `Unsafe.Coerce`
  -- plus the prelude/reflectable private copies; extend if another library ships its own.
  , "Unsafe.Coerce.unsafeCoerce" /\ identityFn
  , "Data.Symbol.unsafeCoerce" /\ identityFn
  , "Data.Reflectable.unsafeCoerce" /\ identityFn
  ]

-- structural / higher-order foreigns as guest terms ------------------------------------

unitLit :: Term
unitLit = intLit 0

runEff :: Term -> Term
runEff e = TmApp e unitLit

arrayMap :: Term
arrayMap = lams [ "f", "xs" ]
  ( TmLet "n" (TmPrim LengthArray [ v "xs" ])
      ( TmLetrec
          [ "go" /\ lams [ "out", "i" ]
              ( TmIf (TmPrim LtInt [ v "i", v "n" ])
                  ( TmApp
                      ( TmApp (v "go")
                          ( TmPrim SetArray
                              [ v "out", v "i", TmApp (v "f") (TmPrim IndexArray [ v "xs", v "i" ]) ]
                          )
                      )
                      (TmPrim AddInt [ v "i", intLit 1 ])
                  )
                  (v "out")
              )
          ]
          (TmApp (TmApp (v "go") (TmPrim NewArray [ v "n" ])) (intLit 0))
      )
  )

eqArray :: Term
eqArray = lams [ "eq", "xs", "ys" ]
  ( TmLet "n" (TmPrim LengthArray [ v "xs" ])
      ( TmIf (TmPrim EqInt [ v "n", TmPrim LengthArray [ v "ys" ] ])
          ( TmLetrec
              [ "go" /\ TmLam "i"
                  ( TmIf (TmPrim LtInt [ v "i", v "n" ])
                      ( TmIf
                          ( TmApp
                              (TmApp (v "eq") (TmPrim IndexArray [ v "xs", v "i" ]))
                              (TmPrim IndexArray [ v "ys", v "i" ])
                          )
                          (TmApp (v "go") (TmPrim AddInt [ v "i", intLit 1 ]))
                          (TmLit (LBool false))
                      )
                      (TmLit (LBool true))
                  )
              ]
              (TmApp (v "go") (intLit 0))
          )
          (TmLit (LBool false))
      )
  )

ordCmp :: PrimOp -> PrimOp -> Term
ordCmp lt eq = lams [ "lt", "eq", "gt", "x", "y" ]
  ( TmIf (TmPrim lt [ v "x", v "y" ])
      (v "lt")
      (TmIf (TmPrim eq [ v "x", v "y" ]) (v "eq") (v "gt"))
  )

ordBoolean :: Term
ordBoolean = lams [ "lt", "eq", "gt", "x", "y" ]
  (TmIf (TmPrim EqBool [ v "x", v "y" ]) (v "eq") (TmIf (v "x") (v "gt") (v "lt")))

effPure :: Term
effPure = lams [ "a", "$u" ] (v "a")

effBind :: Term
effBind = lams [ "a", "f", "$u" ] (runEff (TmApp (v "f") (runEff (v "a"))))

effUntil :: Term
effUntil = lams [ "f", "$u" ]
  ( TmLetrec
      [ "go" /\ TmLam "$g" (TmIf (runEff (v "f")) unitLit (TmApp (v "go") unitLit)) ]
      (TmApp (v "go") unitLit)
  )

effWhile :: Term
effWhile = lams [ "f", "a", "$u" ]
  ( TmLetrec
      [ "go" /\ TmLam "$g"
          ( TmIf (runEff (v "f"))
              (TmLet "$_" (runEff (v "a")) (TmApp (v "go") unitLit))
              unitLit
          )
      ]
      (TmApp (v "go") unitLit)
  )

effFor :: Term
effFor = lams [ "lo", "hi", "f", "$u" ]
  ( TmLetrec
      [ "go" /\ TmLam "i"
          ( TmIf (TmPrim LtInt [ v "i", v "hi" ])
              ( TmLet "$_" (runEff (TmApp (v "f") (v "i")))
                  (TmApp (v "go") (TmPrim AddInt [ v "i", intLit 1 ]))
              )
              unitLit
          )
      ]
      (TmApp (v "go") (v "lo"))
  )

effForeach :: Term
effForeach = lams [ "as", "f", "$u" ]
  ( TmLet "n" (TmPrim LengthArray [ v "as" ])
      ( TmLetrec
          [ "go" /\ TmLam "i"
              ( TmIf (TmPrim LtInt [ v "i", v "n" ])
                  ( TmLet "$_" (runEff (TmApp (v "f") (TmPrim IndexArray [ v "as", v "i" ])))
                      (TmApp (v "go") (TmPrim AddInt [ v "i", intLit 1 ]))
                  )
                  unitLit
              )
          ]
          (TmApp (v "go") (intLit 0))
      )
  )

refNew :: Term
refNew = lams [ "val", "$u" ]
  (TmPrim SetArray [ TmPrim NewArray [ intLit 1 ], intLit 0, v "val" ])

refRead :: Term
refRead = lams [ "ref", "$u" ] (TmPrim IndexArray [ v "ref", intLit 0 ])

refWrite :: Term
refWrite = lams [ "val", "ref", "$u" ]
  (TmLet "$_" (TmPrim SetArray [ v "ref", intLit 0, v "val" ]) unitLit)

refModify :: Term
refModify = lams [ "f", "ref", "$u" ]
  ( TmLet "t" (TmApp (v "f") (TmPrim IndexArray [ v "ref", intLit 0 ]))
      ( TmLet "$_" (TmPrim SetArray [ v "ref", intLit 0, TmAccessor (v "t") "state" ])
          (TmAccessor (v "t") "value")
      )
  )

-- boot's `st_map`/`st_run`/`st_write` (ADR-0023/0039): the `ST` thunk analogues of the Effect
-- combinators. `map_ f a = \_ -> f (a unit)`; `run f = f unit`; `write` returns the written value.
stMap :: Term
stMap = lams [ "f", "a", "$u" ] (TmApp (v "f") (runEff (v "a")))

stRun :: Term
stRun = lams [ "f" ] (runEff (v "f"))

stWrite :: Term
stWrite = lams [ "val", "ref", "$u" ]
  (TmLet "$_" (TmPrim SetArray [ v "ref", intLit 0, v "val" ]) (v "val"))

-- `Record.Builder.unsafeModify l f r = RecordSet l (f (RecordGet l r)) r` (higher-order, ADR-0020).
recordModify :: Term
recordModify = lams [ "l", "f", "r" ]
  (TmPrim RecordSet [ v "l", TmApp (v "f") (TmPrim RecordGet [ v "l", v "r" ]), v "r" ])

recordRename :: Term
recordRename = lams [ "o", "n", "r" ]
  (TmPrim RecordSet [ v "n", TmPrim RecordGet [ v "o", v "r" ], TmPrim RecordDelete [ v "o", v "r" ] ])

-- `Data.Number.fromStringImpl` (ADR-0046): parse via the declared `Purvasm.Number.parseFloat` ABI-floor
-- leaf (ADR-0092 — a structural intrinsic references only declared foreigns, never an invented leaf),
-- then apply the caller's `isFinite`/`Just`/`Nothing`, so a NaN/non-finite parse becomes `Nothing`.
numberFromStringImpl :: Term
numberFromStringImpl = lams [ "str", "isFin", "just", "nothing" ]
  ( TmLet "n" (TmApp (TmForeign "Purvasm.Number.parseFloat") (v "str"))
      (TmIf (TmApp (v "isFin") (v "n")) (TmApp (v "just") (v "n")) (v "nothing"))
  )

-- Saturated left-application of `f` to the argument variables `xs` (`((f x0) x1) …`).
appArgs :: Term -> Array String -> Term
appArgs f xs = foldl (\acc x -> TmApp acc (v x)) f xs

argNames :: Int -> Array String
argNames n = map (\i -> "x" <> show i) (0 .. (n - 1))

-- `Data.Function.Uncurried` (ADR-0039): boot is all-curried, so `mkFnN = identity` and
-- `runFnN = saturated apply`. `Fn0 a` is the `Unit -> a` thunk (`mkFn0`/`runFn0` introduce/force it).
fnUncurried :: Array (String /\ Term)
fnUncurried =
  [ "Data.Function.Uncurried.mkFn0" /\ lams [ "fn", "$u" ] (TmApp (v "fn") unitLit)
  , "Data.Function.Uncurried.runFn0" /\ lams [ "f" ] (TmApp (v "f") unitLit)
  ] <> ([ 2, 3, 4, 5, 6, 7, 8, 9, 10 ] >>= nary)
  where
  nary n =
    let
      xs = argNames n
    in
      [ ("Data.Function.Uncurried.mkFn" <> show n) /\ lams [ "f" ] (v "f")
      , ("Data.Function.Uncurried.runFn" <> show n) /\ lams ([ "f" ] <> xs) (appArgs (v "f") xs)
      ]

-- `Control.Monad.ST.Uncurried` (ADR-0039): the `ST` analogue — `mkSTFnN f = \xs -> run (f xs)`
-- forces the thunk after applying; `runSTFnN g xs = \_ -> g xs` rebuilds it.
stUncurried :: Array (String /\ Term)
stUncurried = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] >>= \n ->
  let
    xs = argNames n
  in
    [ ("Control.Monad.ST.Uncurried.mkSTFn" <> show n) /\ lams ([ "f" ] <> xs) (runEff (appArgs (v "f") xs))
    , ("Control.Monad.ST.Uncurried.runSTFn" <> show n) /\ lams ([ "g" ] <> xs <> [ "$u" ]) (appArgs (v "g") xs)
    ]

structural :: Map String Term
structural = Map.fromFoldable
  ( [ "Data.Functor.arrayMap" /\ arrayMap
    , "Data.Number.fromStringImpl" /\ numberFromStringImpl
    , "Record.Builder.unsafeModify" /\ recordModify
    , "Record.Builder.unsafeRename" /\ recordRename
    , "Data.Eq.eqArrayImpl" /\ eqArray
    , "Data.Ord.ordIntImpl" /\ ordCmp LtInt EqInt
    , "Data.Ord.ordNumberImpl" /\ ordCmp LtNumber EqNumber
    , "Data.Ord.ordStringImpl" /\ ordCmp LtString EqString
    , "Data.Ord.ordCharImpl" /\ ordCmp LtInt EqInt
    , "Data.Ord.ordBooleanImpl" /\ ordBoolean
    , "Effect.pureE" /\ effPure
    , "Effect.bindE" /\ effBind
    , "Effect.untilE" /\ effUntil
    , "Effect.whileE" /\ effWhile
    , "Effect.forE" /\ effFor
    , "Effect.foreachE" /\ effForeach
    , "Effect.Ref._new" /\ refNew
    , "Effect.Ref.read" /\ refRead
    , "Effect.Ref.write" /\ refWrite
    , "Effect.Ref.modifyImpl" /\ refModify
    -- `_unsafePartial f = f unit`: discharge the phantom `Partial` constraint (no methods) by
    -- applying the partial computation to the immediate-0 dummy dictionary.
    , "Partial.Unsafe._unsafePartial" /\ lams [ "f" ] (TmApp (v "f") unitLit)
    -- `Control.Monad.ST.Internal`: the same thunk/cell model as `Effect` (ADR-0023).
    , "Control.Monad.ST.Internal.map_" /\ stMap
    , "Control.Monad.ST.Internal.pure_" /\ effPure
    , "Control.Monad.ST.Internal.bind_" /\ effBind
    , "Control.Monad.ST.Internal.run" /\ stRun
    , "Control.Monad.ST.Internal.while" /\ effWhile
    , "Control.Monad.ST.Internal.for" /\ effFor
    , "Control.Monad.ST.Internal.foreach" /\ effForeach
    , "Control.Monad.ST.Internal.new" /\ refNew
    , "Control.Monad.ST.Internal.read" /\ refRead
    , "Control.Monad.ST.Internal.write" /\ stWrite
    , "Control.Monad.ST.Internal.modifyImpl" /\ refModify
    ] <> stUncurried <> fnUncurried
  )

-- | The link resolver: intrinsic rung, then structural rung (first match).
resolver :: String -> Maybe Term
resolver key = Map.lookup key intrinsics <|> Map.lookup key structural

-- | The **intrinsic** rung only (not structural) — the compiler-kept foreigns (`Purvasm.*`/registry
-- | primops, `charId`/`intDegree`, `unsafeCoerce`, the literal builtins) whose definition the native
-- | backend materialises as a synthesised gdef (boot's link `runtimeMembers`). Structural higher-order
-- | foreigns (category C) are deliberately excluded — they head for a `ulib` native `.c`.
intrinsicTerm :: String -> Maybe Term
intrinsicTerm key = Map.lookup key intrinsics

-- | The compile-time view of the intrinsic rung: the primop (and its arity) a foreign key
-- | eta-expands to, or `Nothing` for non-eta intrinsics (constants, `charId`, `intDegree`),
-- | structural foreigns, and native leaves. Derived from the `intrinsics` terms themselves —
-- | not a second table — so it can never drift from what the linker resolves. This is what
-- | lets the optimiser collapse a saturated intrinsic-foreign call to its primop per-module
-- | (ADR-0027/0028's story, carried today by `DictElim` + the NbE inliner's saturation) — under
-- | B2 separate compilation the eta body is a *link-time* binding the module-local pass would
-- | otherwise never see.
intrinsicPrim :: String -> Maybe { op :: PrimOp, arity :: Int }
intrinsicPrim key = Map.lookup key intrinsics >>= etaPrim []
  where
  -- Match exactly the `eta`-built shape: `\$0 … $n-1 -> Prim(op, [$0; …; $n-1])`, the operands
  -- the parameters in order — anything else (identity lambdas, composite bodies) declines.
  etaPrim ps = case _ of
    TmLam p body -> etaPrim (Array.snoc ps p) body
    TmPrim op args | not (Array.null ps), args == map TmVar ps ->
      Just { op, arity: Array.length ps }
    _ -> Nothing
