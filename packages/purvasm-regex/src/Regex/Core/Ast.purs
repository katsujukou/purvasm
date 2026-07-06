-- | The regex AST (ADR-0081 §1) — exactly the demanded feature floor, nothing more. A pattern
-- | outside this shape never constructs; it fails loudly in `Regex.Core.Parser`.
module Regex.Core.Ast
  ( Node(..)
  , ClsItem(..)
  ) where

import Data.Maybe (Maybe)
import Regex.Core.Unicode (Category)

-- | One regex node. Code points (not code units): the demand compiles everything with the
-- | `u` flag, and purvasm strings are code-point-semantic anyway (ADR-0006).
data Node
  = Seq (Array Node)
  -- | Ordered alternatives (leftmost-first, ES disjunction semantics).
  | Alt (Array Node)
  -- | A literal code point.
  | Lit Int
  -- | `.` — any code point except a LineTerminator (ES 22.2.2.8).
  | Dot
  -- | A character class: negated?, items. A single literal is a degenerate range.
  | Cls Boolean (Array ClsItem)
  -- | A capturing group with its 1-based index. `(?:…)` desugars away at parse time.
  | Group Int Node
  -- | Greedy repetition: node, min, max (`Nothing` = unbounded). `*`/`+`/`?`/`{m,n}` all
  -- | normalise here; the floor has no lazy quantifiers.
  | Rep Node Int (Maybe Int)
  | AnchorStart
  | AnchorEnd
  -- | Negative lookahead `(?!…)`: succeeds iff the sub-pattern fails; consumes nothing and
  -- | leaves captures untouched.
  | NegAhead Node

-- | A class member: an inclusive code-point range, or a Unicode general category (`\p{…}`).
data ClsItem
  = CiRange Int Int
  | CiCat Category
