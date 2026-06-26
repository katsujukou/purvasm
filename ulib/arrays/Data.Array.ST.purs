-- | ulib SHADOW of `arrays`' `Data.Array.ST` (ADR-0038 / ADR-0039), targeting arrays 7.3.x.
-- |
-- | The exported interface (types, signatures, the `run`/`withArray`/`sort*`/`modify`/… wrappers)
-- | is unchanged, so this shadows the registry module. The foreigns are reimplemented in PureScript
-- | over the `purvasm-base` array primitives instead of JS mutable arrays: an `STArray` is reified
-- | (ADR-0039, Option 1) as an `STRef` holding a fixed-length `Purvasm.Array` plus a logical length,
-- | the capacity doubling on overflow — the growable-buffer pattern the `ulib` already uses in
-- | `Data.Unfoldable`. Every mutation's result is threaded back into the `STRef`, so it stays live.
-- |
-- | Accepted cost (ADR-0039): `unsafeFreeze`/`unsafeThaw` are no longer O(1) zero-copy — freeze
-- | trim-copies to an exact-length array — a confined constant-factor regression for the bootstrap.
-- |
-- | The private `popImpl`/`shiftImpl` keep the registry's *behaviour* but a **monomorphic** signature
-- | (`a -> Maybe a` rather than the rank-2 `forall b. b -> Maybe b`): the rank-2 argument cannot be
-- | fed through `mkSTFn3` without impredicative instantiation, and these are not exported, so the
-- | public interface (`pop`/`shift`) is unaffected.
module Data.Array.ST
  ( STArray(..)
  , Assoc
  , run
  , withArray
  , new
  , peek
  , poke
  , modify
  , length
  , pop
  , push
  , pushAll
  , shift
  , unshift
  , unshiftAll
  , splice
  , sort
  , sortBy
  , sortWith
  , freeze
  , thaw
  , clone
  , unsafeFreeze
  , unsafeThaw
  , toAssocArray
  ) where

import Prelude

import Control.Monad.ST (ST, Region)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STI
import Control.Monad.ST.Uncurried (STFn1, STFn2, STFn3, STFn4, mkSTFn1, mkSTFn2, mkSTFn3, mkSTFn4, runSTFn1, runSTFn2, runSTFn3, runSTFn4)
import Data.Maybe (Maybe(..))
import Purvasm.Array as PA
import Purvasm.Int as PI

-- | A reference to a mutable array.
-- |
-- | The first type parameter represents the memory region which the array belongs to.
-- | The second type parameter defines the type of elements of the mutable array.
-- |
-- | ulib representation (ADR-0039): an `STRef` holding a fixed-length `Purvasm.Array` buffer and a
-- | logical length; `length buf` is the capacity, `len` the number of live elements.
newtype STArray :: Region -> Type -> Type
newtype STArray h a = STArray (STI.STRef h { buf :: Array a, len :: Int })

type role STArray nominal representational

-- | An element and its index.
type Assoc a = { value :: a, index :: Int }

-- | A safe way to create and work with a mutable array before returning an
-- | immutable array for later perusal. This function avoids copying the array
-- | before returning it - it uses unsafeFreeze internally, but this wrapper is
-- | a safe interface to that function.
run :: forall a. (forall h. ST h (STArray h a)) -> Array a
run st = ST.run (st >>= unsafeFreeze)

-- | Perform an effect requiring a mutable array on a copy of an immutable array,
-- | safely returning the result as an immutable array.
withArray
  :: forall h a b
   . (STArray h a -> ST h b)
  -> Array a
  -> ST h (Array a)
withArray f xs = do
  result <- thaw xs
  _ <- f result
  unsafeFreeze result

-- | O(1). Convert a mutable array to an immutable array, without copying. The mutable
-- | array must not be mutated afterwards.
unsafeFreeze :: forall h a. STArray h a -> ST h (Array a)
unsafeFreeze = runSTFn1 unsafeFreezeImpl

-- ulib shadow (was a foreign): with doubling slack the buffer cannot be coerced, so trim-copy to an
-- exact-length array (ADR-0039 accepted cost). Same as `freeze` in this representation.
unsafeFreezeImpl :: forall h a. STFn1 (STArray h a) h (Array a)
unsafeFreezeImpl = mkSTFn1 \(STArray ref) -> do
  s <- STI.read ref
  pure (copyRange s.buf 0 s.len)

-- | O(1) Convert an immutable array to a mutable array, without copying. The input
-- | array must not be used afterward.
unsafeThaw :: forall h a. Array a -> ST h (STArray h a)
unsafeThaw = runSTFn1 unsafeThawImpl

-- ulib shadow: wrap the array as the initial buffer (capacity = its length) without copying. As the
-- "unsafe" contract requires, the source must not be used afterward (mutation aliases it).
unsafeThawImpl :: forall h a. STFn1 (Array a) h (STArray h a)
unsafeThawImpl = mkSTFn1 \xs -> map STArray (STI.new { buf: xs, len: PA.length xs })

-- | Create a new, empty mutable array.
new :: forall h a. ST h (STArray h a)
new = map STArray (STI.new { buf: PA.unsafeNew 0, len: 0 })

thaw
  :: forall h a
   . Array a
  -> ST h (STArray h a)
thaw = runSTFn1 thawImpl

-- ulib shadow: a mutable copy — fresh exact-length buffer.
thawImpl :: forall h a. STFn1 (Array a) h (STArray h a)
thawImpl = mkSTFn1 \xs ->
  let n = PA.length xs in map STArray (STI.new { buf: copyRange xs 0 n, len: n })

clone
  :: forall h a
   . STArray h a
  -> ST h (STArray h a)
clone = runSTFn1 cloneImpl

-- ulib shadow: a mutable copy of the live prefix into a fresh buffer.
cloneImpl :: forall h a. STFn1 (STArray h a) h (STArray h a)
cloneImpl = mkSTFn1 \(STArray ref) -> do
  s <- STI.read ref
  map STArray (STI.new { buf: copyRange s.buf 0 s.len, len: s.len })

-- | Sort a mutable array in place. Sorting is stable: the order of equal
-- | elements is preserved.
sort :: forall a h. Ord a => STArray h a -> ST h (STArray h a)
sort = sortBy compare

-- | Remove the first element from an array and return that element.
shift :: forall h a. STArray h a -> ST h (Maybe a)
shift = runSTFn3 shiftImpl Just Nothing

-- ulib shadow (monomorphic private signature; see module header): drop the head and shift the rest
-- left by one (O(n), as upstream JS already is).
shiftImpl :: forall h a. STFn3 (a -> Maybe a) (Maybe a) (STArray h a) h (Maybe a)
shiftImpl = mkSTFn3 \just nothing (STArray ref) -> do
  s <- STI.read ref
  if s.len > 0 then do
    let head = PA.unsafeIndex s.buf 0
    _ <- STI.write { buf: shiftLeft s.buf s.len, len: s.len - 1 } ref
    pure (just head)
  else pure nothing

-- | Sort a mutable array in place using a comparison function. Sorting is
-- | stable: the order of elements is preserved if they are equal according to
-- | the comparison function.
sortBy
  :: forall a h
   . (a -> a -> Ordering)
  -> STArray h a
  -> ST h (STArray h a)
sortBy comp = runSTFn3 sortByImpl comp case _ of
  GT -> 1
  EQ -> 0
  LT -> -1

-- ulib shadow: a stable bottom-up merge sort over the buffer, using `fromOrdering` to read the
-- comparator. Result is written back into the `STRef` and the same `STArray` returned.
sortByImpl :: forall a h. STFn3 (a -> a -> Ordering) (Ordering -> Int) (STArray h a) h (STArray h a)
sortByImpl = mkSTFn3 \comp fromOrdering (STArray ref) -> do
  s <- STI.read ref
  let le x y = fromOrdering (comp x y) <= 0
  _ <- STI.write { buf: sortBuf le s.buf s.len, len: s.len } ref
  pure (STArray ref)

-- | Sort a mutable array in place based on a projection. Sorting is stable: the
-- | order of elements is preserved if they are equal according to the projection.
sortWith
  :: forall a b h
   . Ord b
  => (a -> b)
  -> STArray h a
  -> ST h (STArray h a)
sortWith f = sortBy (comparing f)

-- | Create an immutable copy of a mutable array.
freeze
  :: forall h a
   . STArray h a
  -> ST h (Array a)
freeze = runSTFn1 freezeImpl

freezeImpl :: forall h a. STFn1 (STArray h a) h (Array a)
freezeImpl = unsafeFreezeImpl

-- | Read the value at the specified index in a mutable array.
peek
  :: forall h a
   . Int
  -> STArray h a
  -> ST h (Maybe a)
peek = runSTFn4 peekImpl Just Nothing

peekImpl :: forall h a r. STFn4 (a -> r) r Int (STArray h a) h r
peekImpl = mkSTFn4 \just nothing i (STArray ref) -> do
  s <- STI.read ref
  pure (if 0 <= i && i < s.len then just (PA.unsafeIndex s.buf i) else nothing)

poke
  :: forall h a
   . Int
  -> a
  -> STArray h a
  -> ST h Boolean
poke = runSTFn3 pokeImpl

-- ulib shadow: in-place set when the index is live; the mutated buffer is threaded back so the write
-- is not eliminated.
pokeImpl :: forall h a. STFn3 Int a (STArray h a) h Boolean
pokeImpl = mkSTFn3 \i a (STArray ref) -> do
  s <- STI.read ref
  if 0 <= i && i < s.len then do
    _ <- STI.write (s { buf = PA.unsafeSet s.buf i a }) ref
    pure true
  else pure false

lengthImpl :: forall h a. STFn1 (STArray h a) h Int
lengthImpl = mkSTFn1 \(STArray ref) -> map _.len (STI.read ref)

-- | Get the number of elements in a mutable array.
length :: forall h a. STArray h a -> ST h Int
length = runSTFn1 lengthImpl

-- | Remove the last element from an array and return that element.
pop :: forall h a. STArray h a -> ST h (Maybe a)
pop = runSTFn3 popImpl Just Nothing

-- ulib shadow (monomorphic private signature; see module header): read the last element and shrink
-- the logical length (the buffer keeps its slack).
popImpl :: forall h a. STFn3 (a -> Maybe a) (Maybe a) (STArray h a) h (Maybe a)
popImpl = mkSTFn3 \just nothing (STArray ref) -> do
  s <- STI.read ref
  if s.len > 0 then do
    let last = PA.unsafeIndex s.buf (s.len - 1)
    _ <- STI.write (s { len = s.len - 1 }) ref
    pure (just last)
  else pure nothing

-- | Append an element to the end of a mutable array. Returns the new length of
-- | the array.
push :: forall h a. a -> (STArray h a) -> ST h Int
push = runSTFn2 pushImpl

-- ulib shadow: amortised O(1) append — grow (double) on overflow, set the new slot, bump the length.
pushImpl :: forall h a. STFn2 a (STArray h a) h Int
pushImpl = mkSTFn2 \a (STArray ref) -> do
  s <- STI.read ref
  let len' = s.len + 1
  let buf' = ensureCap s.buf s.len len'
  _ <- STI.write { buf: PA.unsafeSet buf' s.len a, len: len' } ref
  pure len'

-- | Append the values in an immutable array to the end of a mutable array.
-- | Returns the new length of the mutable array.
pushAll
  :: forall h a
   . Array a
  -> STArray h a
  -> ST h Int
pushAll = runSTFn2 pushAllImpl

pushAllImpl :: forall h a. STFn2 (Array a) (STArray h a) h Int
pushAllImpl = mkSTFn2 \as (STArray ref) -> do
  s <- STI.read ref
  let m = PA.length as
  let len' = s.len + m
  let buf' = ensureCap s.buf s.len len'
  _ <- STI.write { buf: blitOff as buf' s.len m, len: len' } ref
  pure len'

-- | Append an element to the front of a mutable array. Returns the new length of
-- | the array.
unshift :: forall h a. a -> STArray h a -> ST h Int
unshift a = runSTFn2 unshiftAllImpl [ a ]

-- | Append the values in an immutable array to the front of a mutable array.
-- | Returns the new length of the mutable array.
unshiftAll
  :: forall h a
   . Array a
  -> STArray h a
  -> ST h Int
unshiftAll = runSTFn2 unshiftAllImpl

unshiftAllImpl :: forall h a. STFn2 (Array a) (STArray h a) h Int
unshiftAllImpl = mkSTFn2 \as (STArray ref) -> do
  s <- STI.read ref
  let m = PA.length as
  let len' = s.len + m
  let buf0 = ensureCap s.buf s.len len'
  let buf1 = shiftRight buf0 s.len m
  _ <- STI.write { buf: blitOff as buf1 0 m, len: len' } ref
  pure len'

-- | Mutate the element at the specified index using the supplied function.
modify :: forall h a. Int -> (a -> a) -> STArray h a -> ST h Boolean
modify i f xs = do
  entry <- peek i xs
  case entry of
    Just x -> poke i (f x) xs
    Nothing -> pure false

-- | Remove and/or insert elements from/into a mutable array at the specified index.
splice
  :: forall h a
   . Int
  -> Int
  -> Array a
  -> STArray h a
  -> ST h (Array a)
splice = runSTFn4 spliceImpl

spliceImpl :: forall h a. STFn4 Int Int (Array a) (STArray h a) h (Array a)
spliceImpl = mkSTFn4 \i howMany bs (STArray ref) -> do
  s <- STI.read ref
  let len = s.len
  let start = clamp 0 len i
  let rem = clamp 0 (len - start) howMany
  let m = PA.length bs
  let len' = len - rem + m
  let removed = copyRange s.buf start rem
  let buf0 = ensureCap s.buf len (max len len')
  let buf1 = moveBlock buf0 (start + rem) (start + m) (len - (start + rem))
  _ <- STI.write { buf: blitOff bs buf1 start m, len: len' } ref
  pure removed

-- | Create an immutable copy of a mutable array, where each element
-- | is labelled with its index in the original array.
toAssocArray
  :: forall h a
   . STArray h a
  -> ST h (Array (Assoc a))
toAssocArray = runSTFn1 toAssocArrayImpl

toAssocArrayImpl :: forall h a. STFn1 (STArray h a) h (Array (Assoc a))
toAssocArrayImpl = mkSTFn1 \(STArray ref) -> do
  s <- STI.read ref
  pure (assoc s.buf s.len)

--------------------------------------------------------------------------------
-- Private growable-buffer helpers over `Purvasm.Array` (ADR-0039). Each loop threads the
-- destination accumulator so the in-place `unsafeSet` writes stay live and ordered.
--------------------------------------------------------------------------------

-- Ensure a buffer with capacity >= `need`, copying the first `keep` elements; doubles the capacity
-- (or jumps straight to `need` when that is larger).
ensureCap :: forall a. Array a -> Int -> Int -> Array a
ensureCap buf keep need =
  if need <= cap then buf else blitInto buf (PA.unsafeNew newCap) keep
  where
  cap = PA.length buf
  doubled = if cap == 0 then 1 else cap * 2
  newCap = if doubled >= need then doubled else need

-- Copy `src[0..n)` into `dst[0..n)`; returns `dst`.
blitInto :: forall a. Array a -> Array a -> Int -> Array a
blitInto src dst n = go 0 dst
  where
  go i acc = if i >= n then acc else go (i + 1) (PA.unsafeSet acc i (PA.unsafeIndex src i))

-- Copy `src[0..n)` into `dst[off..off+n)`; returns `dst`.
blitOff :: forall a. Array a -> Array a -> Int -> Int -> Array a
blitOff src dst off n = go 0 dst
  where
  go i acc = if i >= n then acc else go (i + 1) (PA.unsafeSet acc (off + i) (PA.unsafeIndex src i))

-- Fresh exact-length copy of `src[start..start+n)`.
copyRange :: forall a. Array a -> Int -> Int -> Array a
copyRange src start n = go 0 (PA.unsafeNew n)
  where
  go i acc = if i >= n then acc else go (i + 1) (PA.unsafeSet acc i (PA.unsafeIndex src (start + i)))

-- In place, move `buf[i]` to `buf[i-1]` for `i` in `[1..len)` (drop the head); returns `buf`.
shiftLeft :: forall a. Array a -> Int -> Array a
shiftLeft buf len = go 1 buf
  where
  go i acc = if i >= len then acc else go (i + 1) (PA.unsafeSet acc (i - 1) (PA.unsafeIndex acc i))

-- In place, move `buf[i]` to `buf[i+m]` for `i` in `[len-1..0]` (open a gap of `m` at the front);
-- right-to-left so live elements are not clobbered. Returns `buf`.
shiftRight :: forall a. Array a -> Int -> Int -> Array a
shiftRight buf len m = go (len - 1) buf
  where
  go i acc = if i < 0 then acc else go (i - 1) (PA.unsafeSet acc (i + m) (PA.unsafeIndex acc i))

-- In place, move `count` elements from index `from` to index `to` within `buf`, picking the safe
-- direction so overlapping source/destination ranges are not clobbered. Returns `buf`.
moveBlock :: forall a. Array a -> Int -> Int -> Int -> Array a
moveBlock buf from to count =
  if to == from || count <= 0 then buf
  else if to < from then leftward 0 buf
  else rightward (count - 1) buf
  where
  leftward k acc =
    if k >= count then acc
    else leftward (k + 1) (PA.unsafeSet acc (to + k) (PA.unsafeIndex acc (from + k)))
  rightward k acc =
    if k < 0 then acc
    else rightward (k - 1) (PA.unsafeSet acc (to + k) (PA.unsafeIndex acc (from + k)))

-- Fresh `Assoc` array `{ value, index }` for `buf[0..n)`.
assoc :: forall a. Array a -> Int -> Array (Assoc a)
assoc buf n = go 0 (PA.unsafeNew n)
  where
  go i acc = if i >= n then acc else go (i + 1) (PA.unsafeSet acc i { value: PA.unsafeIndex buf i, index: i })

-- Stable bottom-up merge sort of `buf[0..n)` (`le x y` means `x` may precede `y`); ping-pongs
-- between `buf` and a fresh auxiliary buffer, returning whichever holds the sorted result.
sortBuf :: forall a. (a -> a -> Boolean) -> Array a -> Int -> Array a
sortBuf le buf n =
  if n <= 1 then buf else passes 1 buf (PA.unsafeNew n)
  where
  passes width src aux =
    if width >= n then src else passes (width * 2) (mergePass src aux width) aux

  mergePass src dst width = go 0 dst
    where
    go lo acc =
      if lo >= n then acc
      else go (lo + width + width) (merge src acc lo (min n (lo + width)) (min n (lo + width + width)))

  merge src dst lo mid hi = loop lo mid lo dst
    where
    loop i j k acc =
      if i < mid && j < hi then
        let
          xi = PA.unsafeIndex src i
          xj = PA.unsafeIndex src j
        in
          if le xi xj then loop (i + 1) j (k + 1) (PA.unsafeSet acc k xi)
          else loop i (j + 1) (k + 1) (PA.unsafeSet acc k xj)
      else if i < mid then loop (i + 1) j (k + 1) (PA.unsafeSet acc k (PA.unsafeIndex src i))
      else if j < hi then loop i (j + 1) (k + 1) (PA.unsafeSet acc k (PA.unsafeIndex src j))
      else acc
