-- | The owned VM's runtime values ([ADR-0110](../../../docs/design-decisions/0110-owned-vm-purescript-native.md) §3).
-- |
-- | The VM owns its representation. That is a principle, not a compromise: the LLVM/native ABI is one
-- | backend's representation among several possible ones, and this interpreter is another target —
-- | the relationship is wasm's value representation to a native binary's, not shadow to original. So
-- | no native encoding appears here, and the one place the two meet is the FFI boundary
-- | ([ADR-0111](../../../docs/design-decisions/0111-vm-dynamic-native-ffi.md) §3).
-- |
-- | Two invariants from §3 shape the definitions below:
-- |
-- |   * **A value that came from a native leaf stays opaque.** `purvasm.h` has typed accessors but no
-- |     introspection, so such a value cannot be walked into a `Value`; it is carried (`VCarrier`) and
-- |     decoded at the site that eliminates it, which already knows the shape it demands.
-- |   * **Identity-bearing values are never copied.** purvasm arrays are mutated in place
-- |     (`NewArray`/`SetArray`, ADR-0019) and `Ref`/`STRef` *are* one-element arrays, so every array is
-- |     identity-bearing — which is why `VArray` holds an `ArrayCell` shared by all aliases rather
-- |     than the storage directly. Crossing the FFI boundary *promotes* the cell in place; every alias
-- |     observes the promotion because they share the cell.
module Purvasm.VM.Value
  ( ArrayCell
  , ArrayStorage(..)
  , Closure
  , ForeignValue
  , Thunk(..)
  , Value(..)
  ) where

import Data.List (List)
import Data.Map (Map)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Ref (Ref)
import Purvasm.VM.Instruction (CodeBlock)

-- | A runtime value that crossed the FFI boundary, held exactly as the leaf produced it (ADR-0111 §3).
-- | Opaque on purpose: the supported ABI answers typed questions ("this is an `Int`; its payload?"),
-- | never "what kind is this word?", so there is nothing to decode it *into* on arrival. Nothing in
-- | this slice constructs one — the foreign frontier does.
foreign import data ForeignValue :: Type

-- | A function value: its body block, its parameter names (arity = their count), and the environment
-- | captured where the lambda was built. The environment is a `Ref` so a recursive group can tie the
-- | knot — build the members, then publish the group into the shared environment.
type Closure =
  { params :: Array String
  , body :: CodeBlock
  , env :: Ref (Map String Value)
  }

-- | An array's storage, and the whole of the promotion mechanism's state (ADR-0111 §3). `Local` is
-- | the ordinary case; `Promoted` is what the cell becomes, once and permanently, when the array
-- | crosses to a native leaf — thereafter the VM's own reads and writes go through the runtime's
-- | field accessors, so leaf-side and VM-side writes are the same writes.
data ArrayStorage
  = Local (Array Value)
  | Promoted ForeignValue

-- | The indirection every alias of an array shares. Promotion writes *this*, which is why it is
-- | visible through every binding and every data field holding the array — switching a single
-- | `VArray` binding to a carrier would leave the others behind.
type ArrayCell = Ref ArrayStorage

data Value
  = VInt Int
  | VNumber Number
  | VBool Boolean
  | VString String
  | VArray ArrayCell
  | VRecord (Map String Value)
  -- A saturated constructor: its tag name and fields. The tag stays a *name* — the native tag is
  -- `fnv1a64(name)`, a pure function of it, so binding the format to a numeric encoding would leak
  -- one backend's choice into all of them (ADR-0110 §4).
  | VData String (Array Value)
  -- An under-applied constructor: tag, full arity, args collected so far.
  | VCtor String Int (List Value)
  | VClosure Closure
  -- An under-applied closure: the closure and the args collected so far (eval/apply, ADR-0025).
  | VPap Closure (List Value)
  -- A value produced by a native leaf, carried rather than decoded. Also how a leaf's own closure and
  -- the effect thunks it returns are held: they are applied with the runtime's `apply`, not the VM's
  -- (ADR-0110 §1.1).
  | VCarrier ForeignValue
  -- A by-need cell (ADR-0024/0030): published before it is built so a cyclic instance-dictionary
  -- group can refer to its siblings; forcing an `Unbuilt` cell builds it once and memoises. A
  -- genuinely self-forcing cycle reaches `Building` — a black hole.
  | VThunk (Ref Thunk)

-- | Building a by-need cell runs guest code, so the suspension is an `Effect` action rather than a
-- | pure one. `Building` is the black hole: a cell reached while it is being built.
data Thunk
  = Unbuilt (Unit -> Effect Value)
  | Building
  | Built Value
