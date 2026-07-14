-- | Generalized effect reflection / impurification (GER, ADR-0099): rewrite the `Effect` monad glue
-- | into canonical ANF carrying the explicit `CPerform` run-marker, so the reduction core (NbE) and
-- | `EffectAnalysis` reason about *where* an effect fires instead of a backend runtime representation
-- | leaking through the middle end.
-- |
-- | A single `GerDescriptor` per key is the source of truth: its `key`, its `semanticArity` (the
-- | source/FSR exact-saturation arguments the canonical rewrite needs — *not* the trailing run unit),
-- | its `leavesRung` flag (§3 rung exclusion), and its `lowering`. `ownedKeys` is every descriptor;
-- | `structuralExclusions` is the `leavesRung` subset that leaves the NbE structural rung (ADR-0099 §3
-- | — so NbE never re-hides the node by unfolding it to the guest term). The `--no-opt` fallback
-- | tracks the same bit: a `leavesRung` key is structural-backed and keeps its link-time `resolver`
-- | guest term (`Effect.pureE`/`bindE`), whereas a non-`leavesRung` key (`unsafePerformEffect`) is a
-- | `ulib` PS shadow that `--no-opt` compiles normally.
-- |
-- | The pass recognises a GER key **two ways** (ADR-0099 §5/§6): a bare GER foreign atom at any
-- | saturation, *and* a statically-known `Effect` dispatch (`accessor dict args`) resolved directly
-- | through `DictMachinery` — the main path, so lowering does not wait for NbE to expose the bare
-- | foreign. Under-applied occurrences are η-expanded (never declined); over-applied ones splice a
-- | `let` for the surplus. The pass is idempotent (a lowered `CPerform` tree holds no GER foreign
-- | application) and runs at two points around the reduction core (`early`/`close`, wired by
-- | `optimizeModule`) so no GER-owned foreign reaches codegen even at `--opt-max-iter 1`.
module Purvasm.Compiler.MiddleEnd.Optimizer.Impurify
  ( ownedKeys
  , structuralExclusions
  , effectFamilyOf
  , impurifyExpr
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, modify_)
import Data.Array as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), stripPrefix)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.Compiler.Literal (Literal(..))
import Purvasm.Compiler.MiddleEnd.ANF (Atom(..), CExpr(..), Expr(..), Rhs(..))
import Purvasm.Compiler.MiddleEnd.Module (Decl)
import Purvasm.Compiler.MiddleEnd.Optimizer.DictElim (DictMachinery)
import Purvasm.Compiler.Primitive (PrimOp(..))

-- | A GER-owned key's rewrite spec. `lowering` receives **exactly** `semanticArity` atoms (the pass
-- | guarantees it, η-expanding fewer and splicing more) and returns the canonical computation.
-- |
-- | `leavesRung` is the one **operational** classification the pass acts on (it is *not* a semantic
-- | role): does this key leave the NbE structural rung (ADR-0099 §3)? A key leaves the rung **iff** it
-- | is structural-backed — provided as an `Ffi.structural` guest term that NbE would otherwise unfold,
-- | re-hiding the `CPerform` (all the `Effect`/`ST` glue, loops, `ST` reference combinators, and `ST`
-- | `run`). `run` *eliminates* semantically yet still `leavesRung`, because the deciding fact is
-- | provider form (structural-backed), not what the combinator does — so a future descriptor sets
-- | `leavesRung` from "is there a structural guest term to hide behind", nothing else.
-- | `unsafePerformEffect` is **not** structural-backed (a `ulib` PS shadow), so `leavesRung = false`;
-- | its `foreign import` flip is a follow-up (ADR-0099 Correction). The synthesised `Functor`/`Apply`
-- | `map`/`apply` descriptors (Slice 3, `recognize` on an Effect-family dict) are never in the table
-- | and own no foreign key, so they carry `leavesRung = false` and never touch `structuralExclusions`.
type GerDescriptor =
  { key :: String
  , semanticArity :: Int
  , leavesRung :: Boolean
  , lowering :: Array Atom -> Fresh CExpr
  }

-- | A per-binding fresh-name supply for the binders the canonical rewrites introduce. `$ge` is
-- | disjoint from every other producer (`$q`/`$a`/`$dt`/`$spec$`/`$q0`, and source names, which cannot
-- | start with `$`); `early`'s output is re-quoted by NbE, `close`'s survives to codegen as ordinary
-- | binders.
type Fresh = State Int

fresh :: Fresh String
fresh = do
  modify_ (_ + 1)
  n <- get
  pure ("$ge" <> show n)

-- | `pureE a → \$u -> a` (ADR-0099 §5).
lowerPure :: Array Atom -> Fresh CExpr
lowerPure = case _ of
  [ a ] -> do
    u <- fresh
    pure (CLam [ u ] (Ret (CAtom a)))
  _ -> unsafeCrashWith "Impurify.lowerPure: expected exactly 1 argument"

-- | `bindE m k → \$u -> let x = perform m in let kx = k x in perform kx` (ADR-0099 §5). The
-- | continuation application is `let`-bound so `CPerform`'s operand stays an atom.
lowerBind :: Array Atom -> Fresh CExpr
lowerBind = case _ of
  [ m, k ] -> do
    u <- fresh
    x <- fresh
    kx <- fresh
    pure (CLam [ u ] (Let x (CPerform m) (Let kx (CApp k [ AtomVar x ]) (Ret (CPerform (AtomVar kx))))))
  _ -> unsafeCrashWith "Impurify.lowerBind: expected exactly 2 arguments"

-- | `unsafePerformEffect e → perform e` (ADR-0099 §5): the eliminator *runs* the thunk, so — unlike a
-- | glue combinator — it is not wrapped in a `\$u`; it yields the performed value directly.
lowerUnsafePerform :: Array Atom -> Fresh CExpr
lowerUnsafePerform = case _ of
  [ e ] -> pure (CPerform e)
  _ -> unsafeCrashWith "Impurify.lowerUnsafePerform: expected exactly 1 argument"

-- | `functorEffect.map f m → \$u -> let a = perform m in f a` (ADR-0099 §5(a), Slice 3).
lowerMap :: Array Atom -> Fresh CExpr
lowerMap = case _ of
  [ f, m ] -> do
    u <- fresh
    a <- fresh
    pure (CLam [ u ] (Let a (CPerform m) (Ret (CApp f [ AtomVar a ]))))
  _ -> unsafeCrashWith "Impurify.lowerMap: expected exactly 2 arguments"

-- | `applyEffect.apply mf ma → \$u -> let f = perform mf in let a = perform ma in f a` (§5(a)).
lowerApply :: Array Atom -> Fresh CExpr
lowerApply = case _ of
  [ mf, ma ] -> do
    u <- fresh
    f <- fresh
    a <- fresh
    pure (CLam [ u ] (Let f (CPerform mf) (Let a (CPerform ma) (Ret (CApp (AtomVar f) [ AtomVar a ])))))
  _ -> unsafeCrashWith "Impurify.lowerApply: expected exactly 2 arguments"

-- | The synthetic descriptors for the `Functor`/`Apply` methods, emitted by `recognize` when the
-- | dispatch dict is Effect-family (Slice 3). Not in the descriptor table (no owned foreign key), and
-- | `leavesRung = false` — they own no structural key, so they never touch `structuralExclusions`.
mapDescriptor :: GerDescriptor
mapDescriptor = { key: "Effect.functorEffect.map", semanticArity: 2, leavesRung: false, lowering: lowerMap }

applyDescriptor :: GerDescriptor
applyDescriptor = { key: "Effect.applyEffect.apply", semanticArity: 2, leavesRung: false, lowering: lowerApply }

-- | The synthetic `map`/`apply` descriptor for a `Functor`/`Apply` method field name, else `Nothing`.
methodDescriptor :: String -> Maybe GerDescriptor
methodDescriptor = case _ of
  "map" -> Just mapDescriptor
  "apply" -> Just applyDescriptor
  _ -> Nothing

-- | The unit value (`Int` immediate 0, matching `Ffi.unitLit`).
unitAtom :: Atom
unitAtom = AtomLit (LInt 0)

-- | The `Effect`/`ST` **structural loop combinators** (ADR-0099 §5, Slice 4) lowered to ordinary
-- | `LetRec`/`CIf` ANF with `CPerform` at each thunk force — the `Ffi` guest terms
-- | (`effFor`/`effWhile`/`effUntil`/`effForeach`), with the unit-application force replaced by the
-- | run marker so the loop's effects are explicit and no `Effect.forE`-class structural foreign
-- | survives to the backend. `$u` is the (unused) run binder the combinator's thunk takes.

-- | `untilE f → \$u -> letrec go _ = if perform f then unit else go unit in go unit`.
lowerUntil :: Array Atom -> Fresh CExpr
lowerUntil = case _ of
  [ f ] -> do
    u <- fresh
    go <- fresh
    g <- fresh
    b <- fresh
    pure
      ( CLam [ u ]
          ( LetRec
              [ { var: go
                , rhs: Ret
                    ( CLam [ g ]
                        ( Let b (CPerform f)
                            (Ret (CIf (AtomVar b) (Ret (CAtom unitAtom)) (Ret (CApp (AtomVar go) [ unitAtom ]))))
                        )
                    )
                }
              ]
              (Ret (CApp (AtomVar go) [ unitAtom ]))
          )
      )
  _ -> unsafeCrashWith "Impurify.lowerUntil: expected exactly 1 argument"

-- | `whileE cond body → \$u -> letrec go _ = if perform cond then (let _ = perform body in go unit)
-- | else unit in go unit`.
lowerWhile :: Array Atom -> Fresh CExpr
lowerWhile = case _ of
  [ cond, body ] -> do
    u <- fresh
    go <- fresh
    g <- fresh
    b <- fresh
    dropv <- fresh
    pure
      ( CLam [ u ]
          ( LetRec
              [ { var: go
                , rhs: Ret
                    ( CLam [ g ]
                        ( Let b (CPerform cond)
                            ( Ret
                                ( CIf (AtomVar b)
                                    (Let dropv (CPerform body) (Ret (CApp (AtomVar go) [ unitAtom ])))
                                    (Ret (CAtom unitAtom))
                                )
                            )
                        )
                    )
                }
              ]
              (Ret (CApp (AtomVar go) [ unitAtom ]))
          )
      )
  _ -> unsafeCrashWith "Impurify.lowerWhile: expected exactly 2 arguments"

-- | `forE lo hi f → \$u -> letrec go i = if i < hi then (let _ = perform (f i) in go (i+1)) else unit
-- | in go lo`.
lowerFor :: Array Atom -> Fresh CExpr
lowerFor = case _ of
  [ lo, hi, f ] -> do
    u <- fresh
    go <- fresh
    i <- fresh
    cmp <- fresh
    fi <- fresh
    dropv <- fresh
    i1 <- fresh
    pure
      ( CLam [ u ]
          ( LetRec
              [ { var: go
                , rhs: Ret
                    ( CLam [ i ]
                        ( Let cmp (CPrim LtInt [ AtomVar i, hi ])
                            ( Ret
                                ( CIf (AtomVar cmp)
                                    ( Let fi (CApp f [ AtomVar i ])
                                        ( Let dropv (CPerform (AtomVar fi))
                                            ( Let i1 (CPrim AddInt [ AtomVar i, AtomLit (LInt 1) ])
                                                (Ret (CApp (AtomVar go) [ AtomVar i1 ]))
                                            )
                                        )
                                    )
                                    (Ret (CAtom unitAtom))
                                )
                            )
                        )
                    )
                }
              ]
              (Ret (CApp (AtomVar go) [ lo ]))
          )
      )
  _ -> unsafeCrashWith "Impurify.lowerFor: expected exactly 3 arguments"

-- | `foreachE as f → \$u -> let n = length as in letrec go i = if i < n then (let _ = perform
-- | (f as[i]) in go (i+1)) else unit in go 0`.
lowerForeach :: Array Atom -> Fresh CExpr
lowerForeach = case _ of
  [ as, f ] -> do
    u <- fresh
    n <- fresh
    go <- fresh
    i <- fresh
    cmp <- fresh
    el <- fresh
    fi <- fresh
    dropv <- fresh
    i1 <- fresh
    pure
      ( CLam [ u ]
          ( Let n (CPrim LengthArray [ as ])
              ( LetRec
                  [ { var: go
                    , rhs: Ret
                        ( CLam [ i ]
                            ( Let cmp (CPrim LtInt [ AtomVar i, AtomVar n ])
                                ( Ret
                                    ( CIf (AtomVar cmp)
                                        ( Let el (CPrim IndexArray [ as, AtomVar i ])
                                            ( Let fi (CApp f [ AtomVar el ])
                                                ( Let dropv (CPerform (AtomVar fi))
                                                    ( Let i1 (CPrim AddInt [ AtomVar i, AtomLit (LInt 1) ])
                                                        (Ret (CApp (AtomVar go) [ AtomVar i1 ]))
                                                    )
                                                )
                                            )
                                        )
                                        (Ret (CAtom unitAtom))
                                    )
                                )
                            )
                        )
                    }
                  ]
                  (Ret (CApp (AtomVar go) [ AtomLit (LInt 0) ]))
              )
          )
      )
  _ -> unsafeCrashWith "Impurify.lowerForeach: expected exactly 2 arguments"

-- | `run e → perform e` (ADR-0099 §5/§9): the `ST` eliminator forces the thunk and yields its value
-- | — like `unsafePerformEffect`, so no `\$u` wrapper — but, being an `Ffi.structural` guest
-- | (`stRun = \f -> f unit`), it has `leavesRung = true` (else NbE unfolds it and re-hides the run as a
-- | plain unit-application). `run` eliminates semantically yet leaves the rung on the provider-form
-- | fact alone — the `leavesRung` flag is not a semantic role (see `GerDescriptor`).
lowerRun :: Array Atom -> Fresh CExpr
lowerRun = case _ of
  [ e ] -> pure (CPerform e)
  _ -> unsafeCrashWith "Impurify.lowerRun: expected exactly 1 argument"

-- | `ST` reference combinators (ADR-0099 §9, Slice 5) — the `STRef` cell is a one-element array (the
-- | same model as `Effect.Ref`, ADR-0023): the mutation happens in the `\$u` thunk body, so no
-- | `CPerform` appears here (there is no inner thunk to force); the run boundary is supplied by the
-- | enclosing `bind_`/`run`. `read`/`modifyImpl` read the cell with `IndexArray`, which
-- | `EffectAnalysis.mtouchC` already pins as `mtouch = true` (`pinnedPrim`) — so the ADR-0096 §2 motion
-- | hazard directly forbids sinking / reordering the read across a `SetArray`; ordering is *not*
-- | reliant on the surrounding `CPerform`. What stays deferred (region-local track) is the *precise
-- | relaxation* that would let a provably region-local read move. The adversarial gate exercises the
-- | end-to-end lowering behaviour, not this ordering (which the pin already guarantees).

-- | `new val → \$u -> let arr = newArray 1 in setArray arr 0 val` (the cell is the mutated array).
lowerNew :: Array Atom -> Fresh CExpr
lowerNew = case _ of
  [ val ] -> do
    u <- fresh
    arr <- fresh
    pure (CLam [ u ] (Let arr (CPrim NewArray [ AtomLit (LInt 1) ]) (Ret (CPrim SetArray [ AtomVar arr, AtomLit (LInt 0), val ]))))
  _ -> unsafeCrashWith "Impurify.lowerNew: expected exactly 1 argument"

-- | `read ref → \$u -> indexArray ref 0`.
lowerRead :: Array Atom -> Fresh CExpr
lowerRead = case _ of
  [ ref ] -> do
    u <- fresh
    pure (CLam [ u ] (Ret (CPrim IndexArray [ ref, AtomLit (LInt 0) ])))
  _ -> unsafeCrashWith "Impurify.lowerRead: expected exactly 1 argument"

-- | `write val ref → \$u -> let _ = setArray ref 0 val in val` — the `ST` `write` returns the written
-- | value (unlike `Effect.Ref.write`, which returns unit).
lowerWrite :: Array Atom -> Fresh CExpr
lowerWrite = case _ of
  [ val, ref ] -> do
    u <- fresh
    dropv <- fresh
    pure (CLam [ u ] (Let dropv (CPrim SetArray [ ref, AtomLit (LInt 0), val ]) (Ret (CAtom val))))
  _ -> unsafeCrashWith "Impurify.lowerWrite: expected exactly 2 arguments"

-- | `modifyImpl f ref → \$u -> let old = indexArray ref 0 in let t = f old in let s = t.state in
-- | let _ = setArray ref 0 s in t.value`.
lowerModify :: Array Atom -> Fresh CExpr
lowerModify = case _ of
  [ f, ref ] -> do
    u <- fresh
    old <- fresh
    t <- fresh
    s <- fresh
    dropv <- fresh
    pure
      ( CLam [ u ]
          ( Let old (CPrim IndexArray [ ref, AtomLit (LInt 0) ])
              ( Let t (CApp f [ AtomVar old ])
                  ( Let s (CAccessor (AtomVar t) "state")
                      ( Let dropv (CPrim SetArray [ ref, AtomLit (LInt 0), AtomVar s ])
                          (Ret (CAccessor (AtomVar t) "value"))
                      )
                  )
              )
          )
      )
  _ -> unsafeCrashWith "Impurify.lowerModify: expected exactly 2 arguments"

-- | The GER descriptor table — the owned keys landed across slices, every one structural-backed
-- | (`leavesRung = true`) except `unsafePerformEffect`. By semantic grouping (documentation only —
-- | the pass acts on `leavesRung`, not the grouping): the glue `pureE`/`bindE` (Slice 2); the
-- | structural loop combinators `forE`/`whileE`/`untilE`/`foreachE` + their `Control.Monad.ST.Internal`
-- | twins (Slice 4); the rest of `Control.Monad.ST.Internal`'s glue / eliminator / reference
-- | combinators `pure_`/`bind_`/`run`/`new`/`read`/`write`/`modifyImpl` (Slice 5; `ST r a` is the same
-- | unit thunk, ADR-0099 §9); and `unsafePerformEffect` (Slice 2, `ulib`-shadow-backed → `leavesRung
-- | = false`). The `Functor`/`Apply` `map`/`apply` rewrites (Slice 3) are **not** here — they carry no
-- | owned foreign key and are synthesised by `recognize` on an Effect-family dict. `ST`'s `map_`/`apply`
-- | are out of Slice 5's §9 scope (`map_` stays structural; `apply` is the generic `ap`); the
-- | `Control.Monad.ST.Uncurried` `STFnN` adapters are deferred (ADR-0099 §9 Correction).
descriptorList :: Array GerDescriptor
descriptorList =
  [ { key: "Effect.pureE", semanticArity: 1, leavesRung: true, lowering: lowerPure }
  , { key: "Effect.bindE", semanticArity: 2, leavesRung: true, lowering: lowerBind }
  , { key: "Effect.Unsafe.unsafePerformEffect", semanticArity: 1, leavesRung: false, lowering: lowerUnsafePerform }
  -- structural loop combinators (Slice 4): lowered to `LetRec`/`CIf` ANF. The `ST` loop twins
  -- share the guest terms (`Ffi.structural`) and so the same lowerings.
  , { key: "Effect.untilE", semanticArity: 1, leavesRung: true, lowering: lowerUntil }
  , { key: "Effect.whileE", semanticArity: 2, leavesRung: true, lowering: lowerWhile }
  , { key: "Effect.forE", semanticArity: 3, leavesRung: true, lowering: lowerFor }
  , { key: "Effect.foreachE", semanticArity: 2, leavesRung: true, lowering: lowerForeach }
  , { key: "Control.Monad.ST.Internal.while", semanticArity: 2, leavesRung: true, lowering: lowerWhile }
  , { key: "Control.Monad.ST.Internal.for", semanticArity: 3, leavesRung: true, lowering: lowerFor }
  , { key: "Control.Monad.ST.Internal.foreach", semanticArity: 2, leavesRung: true, lowering: lowerForeach }
  -- `ST` glue / eliminator / reference combinators (Slice 5): the same unit-thunk model as `Effect`.
  , { key: "Control.Monad.ST.Internal.pure_", semanticArity: 1, leavesRung: true, lowering: lowerPure }
  , { key: "Control.Monad.ST.Internal.bind_", semanticArity: 2, leavesRung: true, lowering: lowerBind }
  , { key: "Control.Monad.ST.Internal.run", semanticArity: 1, leavesRung: true, lowering: lowerRun }
  , { key: "Control.Monad.ST.Internal.new", semanticArity: 1, leavesRung: true, lowering: lowerNew }
  , { key: "Control.Monad.ST.Internal.read", semanticArity: 1, leavesRung: true, lowering: lowerRead }
  , { key: "Control.Monad.ST.Internal.write", semanticArity: 2, leavesRung: true, lowering: lowerWrite }
  , { key: "Control.Monad.ST.Internal.modifyImpl", semanticArity: 2, leavesRung: true, lowering: lowerModify }
  ]

gerDescriptors :: Map String GerDescriptor
gerDescriptors = Map.fromFoldable (map (\d -> Tuple d.key d) descriptorList)

-- | Every GER-owned key `Impurify` recognises and lowers.
ownedKeys :: Set String
ownedKeys = Set.fromFoldable (map _.key descriptorList)

-- | The keys that leave the NbE structural rung (ADR-0099 §3): the `leavesRung` subset only.
structuralExclusions :: Set String
structuralExclusions = Set.fromFoldable (map _.key (Array.filter _.leavesRung descriptorList))

-- | The GER **Effect-family** classifier (ADR-0099 Slice 3, sidenote 0014, fail-closed): from a
-- | module's raw decls, the instance-dict keys of any recursive group whose whole shape matches the
-- | `Effect` `Monad`-hierarchy structural ABI. This validates the compiler-owned `Effect` module's
-- | instance-group shape — **not** a type-derived proof — so it is deliberately strict and
-- | fail-closed: a mis-shaped or third-party group is never admitted (GER then leaves `map`/`apply`
-- | to the slower-but-correct Slice-2 collapse).
-- |
-- | A recursive group is admitted **iff**: it has exactly five members, each a dict in
-- | `machinery.instances`; the five roles below are each present exactly once (by their method
-- | field, robust to `purs`'s superclass-accessor renaming); and the `Applicative`/`Bind` members
-- | hold the `Effect.pureE` / `Effect.bindE` structural-guest anchors directly. More than one
-- | matching group in a module fails closed (never expected).
-- |
-- | Roles (field-name sets, verified against the compiled `Effect` corefn): `Functor` `{map}`,
-- | `Apply` `{apply, _sc}`, `Applicative` `{pure, _sc}`, `Bind` `{bind, _sc}`, `Monad`
-- | `{_sc, _sc}` (no method).
effectFamilyOf :: Array Decl -> DictMachinery -> Set String
effectFamilyOf decls machinery =
  case Array.mapMaybe admit recGroups of
    [ ks ] -> ks
    _ -> Set.empty -- zero admitted, or (never expected) more than one → fail closed
  where
  recGroups :: Array (Array String)
  recGroups = Array.mapMaybe (\d -> if d.recursive then Just (map fst d.members) else Nothing) decls

  admit :: Array String -> Maybe (Set String)
  admit keys
    | Array.length keys /= 5 = Nothing
    | otherwise = case traverse (\k -> Map.lookup k machinery.instances) keys of
        Nothing -> Nothing -- a member is not a known dict
        Just fieldMaps ->
          if Array.sort (map roleOf fieldMaps) == [ "Ap", "App", "B", "F", "M" ] then Just (Set.fromFoldable keys)
          else Nothing

  -- The role of an instance dict by its field set; `"invalid"` for anything off the expected shape
  -- (a duplicate or invalid role makes the sorted-roles equality fail, so the group is rejected).
  roleOf :: Map String Atom -> String
  roleOf fm =
    let
      fields = Set.fromFoldable (Map.keys fm)
      n = Set.size fields
      -- the class methods present; each role admits *exactly one* method (plus superclass fields),
      -- so a mis-shaped dict like `{ apply, map }` (two methods) is rejected — not read as `Apply`.
      methodFields = Set.intersection fields (Set.fromFoldable [ "map", "apply", "pure", "bind" ])
      oneMethod x = methodFields == Set.singleton x
      fieldKey x = case Map.lookup x fm of
        Just (AtomForeign k) -> k
        Just (AtomVar k) -> k
        _ -> ""
    in
      if fields == Set.singleton "map" then "F"
      else if oneMethod "apply" && n == 2 then "Ap"
      else if oneMethod "pure" && n == 2 && fieldKey "pure" == "Effect.pureE" then "App"
      else if oneMethod "bind" && n == 2 && fieldKey "bind" == "Effect.bindE" then "B"
      else if Set.isEmpty methodFields && n == 2 then "M"
      else "invalid"

-- | A key's descriptor, on either atom spelling (a foreign rides `AtomForeign` or a plain qualified
-- | `AtomVar`, ADR-0096 review).
descriptorOf :: Atom -> Maybe GerDescriptor
descriptorOf = case _ of
  AtomForeign k -> Map.lookup k gerDescriptors
  AtomVar k -> Map.lookup k gerDescriptors
  _ -> Nothing

-- | Recognise a computation as a GER application, returning its descriptor and the value arguments
-- | (after the dictionary, for a dispatch). Three shapes: a bare GER foreign applied to args
-- | (ADR-0099 §5), a statically-known `Effect` dispatch `accessor dict args` whose impl is a GER key
-- | (§6), and — Slice 3 — a `map`/`apply` dispatch on an **Effect-family** dict (`§5(a)`), whose impl
-- | is the generic `liftA1`/`ap` (not a GER key) but whose canonical Effect `map`/`apply` GER emits
-- | directly. The field-name is only the *selector*; the `effectFamily` membership is the gate (a
-- | `map` on a non-family dict is left alone).
recognize :: DictMachinery -> Atom -> Array Atom -> Maybe (Tuple GerDescriptor (Array Atom))
recognize machinery head args = case descriptorOf head of
  Just desc -> Just (Tuple desc args)
  Nothing -> case head, Array.uncons args of
    AtomVar acc, Just { head: AtomVar d, tail: rest } -> case Map.lookup acc machinery.accessors of
      Nothing -> Nothing
      Just field ->
        case Map.lookup d machinery.instances >>= Map.lookup field >>= descriptorOf of
          Just desc -> Just (Tuple desc rest) -- impl is a GER foreign
          Nothing
            | Set.member d machinery.effectFamily
            , Just desc <- methodDescriptor field -> Just (Tuple desc rest)
            | otherwise -> Nothing
    _, _ -> Nothing

-- | The canonical placement of a recognised GER application: `pre` = `let`-bindings to emit ahead of
-- | it (non-empty only for an over-application, which binds the produced thunk to a fresh var and
-- | applies the surplus to it), and `comp` = the computation at the site. Exact and η-expanded cases
-- | have no `pre`.
applyGer :: GerDescriptor -> Array Atom -> Fresh { pre :: Array (Tuple String CExpr), comp :: CExpr }
applyGer desc args =
  let
    n = Array.length args
    sa = desc.semanticArity
  in
    case compare n sa of
      EQ -> do
        comp <- desc.lowering args
        pure { pre: [], comp }
      LT -> do
        ps <- traverse (const fresh) (Array.range 1 (sa - n))
        comp <- desc.lowering (args <> map AtomVar ps)
        pure { pre: [], comp: CLam ps (Ret comp) }
      GT -> do
        thunk <- desc.lowering (Array.take sa args)
        t <- fresh
        pure { pre: [ Tuple t thunk ], comp: CApp (AtomVar t) (Array.drop sa args) }

-- | Impurify one binding body: raise every recognised `Effect` glue / eliminator to canonical
-- | `CPerform` ANF — an *applied* GER key (recognised head), a *bare* GER key in value position
-- | (a `CAtom`, an argument, a record/array/constructor field, a scrutinee), or a known dispatch.
-- | A bare key is the 0-argument under-application: η-expanded to a closed thunk-builder and
-- | (except at a `CAtom`) `let`-hoisted, so no GER-owned structural foreign ever survives as a data
-- | reference (which, excluded from the structural rung and absent from the LLVM provider, would
-- | reach codegen unresolved).
-- |
-- | The fresh supply seeds **above the highest `$ge` binder already in the body** (`maxGe`), so a
-- | second-point / next-round application whose input already holds `$ge` binders cannot mint a name
-- | that captures one (the `$ge` namespace is disjoint from every other producer, but not from a
-- | prior `Impurify`'s own output).
-- |
-- | Ordinary structural recursion (depth = control-flow / `let`-spine nesting); the constant-stack
-- | hardening the ADR flags for compiler-sized modules is owed, deferred to match the sibling
-- | passes (`Quote`/`FreeVars`) — self-compile stays within the native stack.
impurifyExpr :: DictMachinery -> Expr -> Expr
impurifyExpr machinery e0 = evalState (goE e0) (maxGe e0)
  where
  goE :: Expr -> Fresh Expr
  goE = case _ of
    Ret c -> do
      { pre, comp } <- goC c
      pure (withPre pre (Ret comp))
    Let x c rest -> do
      { pre, comp } <- goC c
      rest' <- goE rest
      pure (withPre pre (Let x comp rest'))
    LetRec bs rest -> do
      bs' <- traverse (\b -> (\rhs -> b { rhs = rhs }) <$> goE b.rhs) bs
      rest' <- goE rest
      pure (LetRec bs' rest')

  -- Recognise/lower a computation; hoist any bare GER-key atom in a value position; recurse into
  -- nested `Expr`s. `pre` carries `let`-bindings to emit ahead of the computation.
  goC :: CExpr -> Fresh { pre :: Array (Tuple String CExpr), comp :: CExpr }
  goC c = case c of
    -- applied GER key (recognised head / dispatch); value arguments are still scanned (an argument
    -- may itself be a bare GER key, e.g. a continuation `Effect.pureE`).
    CApp head args
      | Just (Tuple desc rest) <- recognize machinery head args -> do
          h <- hoistAtoms rest
          r <- applyGer desc h.atoms
          pure { pre: h.pre <> r.pre, comp: r.comp }
    -- a bare GER key in value/return position η-expands to a closed thunk-builder (already a
    -- computation — no `let` needed here).
    CAtom a
      | Just desc <- descriptorOf a -> applyGer desc []
    CAtom a -> pure { pre: [], comp: CAtom a }
    CApp head args -> do
      h <- hoistAtoms args
      pure { pre: h.pre, comp: CApp head h.atoms }
    CPrim op args -> do
      h <- hoistAtoms args
      pure { pre: h.pre, comp: CPrim op h.atoms }
    CCtor n ar args -> do
      h <- hoistAtoms args
      pure { pre: h.pre, comp: CCtor n ar h.atoms }
    CArray args -> do
      h <- hoistAtoms args
      pure { pre: h.pre, comp: CArray h.atoms }
    CRecord fs -> do
      h <- hoistAtoms (map _.val fs)
      pure { pre: h.pre, comp: CRecord (Array.zipWith (\f a -> f { val = a }) fs h.atoms) }
    -- The `map`/`apply` **field projected off an Effect-family dict** (Slice 3): NbE turns the
    -- `Data.Functor.map functorEffect` dispatch into this runtime projection (the group-recursive
    -- `map` field never folds, ADR-0098), so the dispatch arm of `recognize` never sees it. Replace
    -- the projection with the canonical Effect `map`/`apply` function (η-expanded); the enclosing
    -- application reduces in the next NbE round.
    CAccessor (AtomVar d) field
      | Set.member d machinery.effectFamily
      , Just desc <- methodDescriptor field -> applyGer desc []
    CAccessor a l -> do
      h <- hoistAtom a
      pure { pre: h.pre, comp: CAccessor h.atom l }
    CUpdate a fs -> do
      ha <- hoistAtom a
      hf <- hoistAtoms (map _.val fs)
      pure { pre: ha.pre <> hf.pre, comp: CUpdate ha.atom (Array.zipWith (\f a' -> f { val = a' }) fs hf.atoms) }
    CPerform a -> do
      h <- hoistAtom a
      pure { pre: h.pre, comp: CPerform h.atom }
    CLam ps body -> do
      body' <- goE body
      pure { pre: [], comp: CLam ps body' }
    CIf a t e -> do
      h <- hoistAtom a
      t' <- goE t
      e' <- goE e
      pure { pre: h.pre, comp: CIf h.atom t' e' }
    CCase ss alts -> do
      h <- hoistAtoms ss
      alts' <- traverse goAlt alts
      pure { pre: h.pre, comp: CCase h.atoms alts' }

  goAlt alt = (\result -> alt { result = result }) <$> goRhs alt.result

  goRhs = case _ of
    Uncond e -> Uncond <$> goE e
    Guarded gs -> Guarded <$> traverse (\g -> { guard: _, rhs: _ } <$> goE g.guard <*> goE g.rhs) gs

  -- η-expand a bare GER-key atom to a `let`-bound closed thunk-builder, substituting a fresh var;
  -- leave a non-GER atom untouched.
  hoistAtom :: Atom -> Fresh { pre :: Array (Tuple String CExpr), atom :: Atom }
  hoistAtom a = case descriptorOf a of
    Just desc -> do
      r <- applyGer desc []
      t <- fresh
      pure { pre: r.pre <> [ Tuple t r.comp ], atom: AtomVar t }
    Nothing -> pure { pre: [], atom: a }

  hoistAtoms :: Array Atom -> Fresh { pre :: Array (Tuple String CExpr), atoms :: Array Atom }
  hoistAtoms as = do
    rs <- traverse hoistAtom as
    pure { pre: Array.concatMap _.pre rs, atoms: map _.atom rs }

-- | Emit the `pre` `let`-bindings ahead of a computation-position expression.
withPre :: Array (Tuple String CExpr) -> Expr -> Expr
withPre pre inner = Array.foldr (\(Tuple x rhs) acc -> Let x rhs acc) inner pre

-- | The highest `n` of any `$ge<n>` binder or reference already in the term (0 if none) — the seed
-- | above which a fresh `$ge` cannot capture a `$ge` a prior `Impurify` left behind.
maxGe :: Expr -> Int
maxGe = expr 0
  where
  geNum s = case stripPrefix (Pattern "$ge") s of
    Just rest -> fromMaybe 0 (Int.fromString rest)
    Nothing -> 0

  expr acc = case _ of
    Ret c -> cexpr acc c
    Let x c rest -> expr (cexpr (max acc (geNum x)) c) rest
    LetRec bs rest -> expr (Array.foldl (\a b -> expr (max a (geNum b.var)) b.rhs) acc bs) rest

  cexpr acc = case _ of
    CAtom a -> atom acc a
    CLam ps body -> expr (Array.foldl (\a p -> max a (geNum p)) acc ps) body
    CApp h as -> Array.foldl atom (atom acc h) as
    CPrim _ as -> Array.foldl atom acc as
    CCtor _ _ as -> Array.foldl atom acc as
    CArray as -> Array.foldl atom acc as
    CRecord fs -> Array.foldl (\a f -> atom a f.val) acc fs
    CAccessor a _ -> atom acc a
    CUpdate a fs -> Array.foldl (\ac f -> atom ac f.val) (atom acc a) fs
    CIf a t e -> expr (expr (atom acc a) t) e
    CCase ss alts -> Array.foldl alt (Array.foldl atom acc ss) alts
    CPerform a -> atom acc a

  atom acc = case _ of
    AtomVar x -> max acc (geNum x)
    _ -> acc

  alt acc a = case a.result of
    Uncond e -> expr acc e
    Guarded gs -> Array.foldl (\ac g -> expr (expr ac g.guard) g.rhs) acc gs
