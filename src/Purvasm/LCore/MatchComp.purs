module Purvasm.LCore.MatchComp where

-- | Pattern matching compiler.

import Prelude

import Data.Array (mapWithIndex, (!!))
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import Purvasm.ECore.Syntax as ECF
import Purvasm.Types (AtomicConstant, ConstructorTag)

type Expr = ECF.Expr ECF.Ann

newtype PatternMatching = PatternMatching
  { pmHeads :: Array Expr
  , pmMatrix :: Array PatternMatrix
  }

instance Show PatternMatching where
  show (PatternMatching pm) = "(PatternMatching " <> show pm <> ")"

type PatternMatrix =
  { pats :: Array ECF.Pattern
  , action :: Int
  }

data DecisionTree
  = MatchLeaf Int
  | Conditional Expr (Array (Tuple AtomicConstant DecisionTree))
  | JumpThru Expr (Array (ConstructorTag /\ DecisionTree))
  | MatchFail
  | MatchTry DecisionTree DecisionTree

derive instance Generic DecisionTree _
instance Show DecisionTree where
  show det = genericShow det

alwaysMatch :: ECF.Pattern -> Boolean
alwaysMatch = case _ of
  ECF.PatWildcard -> true
  ECF.PatVar _ -> true
  ECF.PatAliase _ pat -> alwaysMatch pat
  _ -> false

decomposePatternMatch :: PatternMatching -> DecisionTree
decomposePatternMatch pm =
  case splitPatternMatch pm of
    Left leaf -> leaf
    Right (vars /\ nonVars)
      | failedMatch nonVars -> decomposePatternMatch vars
      | failedMatch vars -> reduceMatrix nonVars
      | otherwise ->
          combine
            (decomposePatternMatch vars)
            (reduceMatrix nonVars)
  where
  failedMatch (PatternMatching { pmMatrix }) = Array.null pmMatrix
  reduceMatrix (PatternMatching { pmHeads, pmMatrix }) =
    case Array.head pmMatrix, Array.head pmHeads of
      Nothing, _ -> MatchFail
      Just { action: act0 }, Nothing -> MatchLeaf act0
      Just _, Just pmHead ->
        let
          -- split horizontally  
          { init: nonVars, rest } =
            Array.span
              (_.pats >>> Array.head >>> maybe true (not <<< alwaysMatch))
              pmMatrix
          jumpTable = PatternMatching { pmHeads, pmMatrix: nonVars }
            # reducePatternMatch
            # partitionEither
            # \{ left, right } ->
                case left, right of
                  subMatching, [] ->
                    Conditional pmHead $
                      ((rmap decomposePatternMatch) <$> subMatching)
                  [], subMatching ->
                    JumpThru pmHead $
                      ((rmap decomposePatternMatch) <$> subMatching)
                  _, _ -> unsafeCrashWith "decomposePatternMatching: Impossible!"
        in
          combine
            (jumpTable)
            (decomposePatternMatch $ PatternMatching { pmHeads, pmMatrix: rest })
  combine l r
    | MatchLeaf _ <- l = l
    | MatchFail <- l = r
    | MatchFail <- r = l
    | otherwise = MatchTry l r

splitPatternMatch :: PatternMatching -> Either DecisionTree (PatternMatching /\ PatternMatching)
splitPatternMatch (PatternMatching { pmHeads, pmMatrix }) =
  case Array.uncons pmMatrix of
    Nothing -> Left MatchFail
    Just { head }
      | Array.null head.pats -> Left $ MatchLeaf head.action
      | otherwise ->
          let
            { init: varMatrix, rest } = Array.span (_.pats >>> Array.head >>> maybe true alwaysMatch) pmMatrix
            reducedMatch = PatternMatching
              { pmHeads: Array.drop 1 pmHeads
              , pmMatrix: map (\mat -> mat { pats = Array.drop 1 mat.pats }) varMatrix
              }
          in
            Right $
              reducedMatch /\ PatternMatching { pmHeads, pmMatrix: rest }

reducePatternMatch
  :: PatternMatching
  -> Array
       ( Either
           (AtomicConstant /\ PatternMatching)
           (ConstructorTag /\ PatternMatching)
       )
reducePatternMatch (PatternMatching { pmHeads, pmMatrix }) = leftMostCol
  # groupBySameToplevelSymbol
  <#>
    ( \(pat /\ lnIndices) ->
        let
          subMatrix = subMatchMatrix lnIndices pmMatrix
        in
          case pat of
            ECF.PatLiteral lit -> Left $ lit /\
              ( PatternMatching
                  { pmHeads: Array.drop 1 pmHeads
                  , pmMatrix: subMatrix
                  }
              )
            ECF.PatConstruct { tag } subs
              | Just head <- Array.head pmHeads ->
                  let
                    expandedHeads = subs # mapWithIndex \i _ -> ECF.ExprGetField ECF.emptyAnn i head
                  in
                    Right $ tag /\
                      ( PatternMatching
                          { pmHeads: expandedHeads <> Array.drop 1 pmHeads
                          , pmMatrix: subMatrix
                          }
                      )
            _ -> unsafeCrashWith "reducePatternMatching: Impossible!"

    )
  where
  leftMostCol = case traverse (_.pats >>> Array.head) pmMatrix of
    Nothing -> unsafeCrashWith "decomposePatternMatch: zero-width matrix"
    Just it -> it

  subMatchMatrix :: Array (Int /\ Array ECF.Pattern) -> _ -> _
  subMatchMatrix indices matrix = traverse (selectLines matrix) indices
    # case _ of
        Nothing -> unsafeCrashWith "decomposePatternMatch: Impossible!"
        Just lines -> lines
    where
    selectLines mat (ln /\ subPats) = do
      matLine <- mat !! ln
      pure $ matLine { pats = subPats <> Array.drop 1 matLine.pats }

groupBySameToplevelSymbol
  :: Array ECF.Pattern
  -> Array
       ( Tuple
           -- left representative pattern of group
           ECF.Pattern
           -- the indices of lines grouped w.r.t. the left-most pattern.
           -- paired with the sub-patterns expanded.
           (Array (Int /\ Array ECF.Pattern))
       )
groupBySameToplevelSymbol = go [] <<< mapWithIndex (/\)
  where
  go acc = Array.uncons >>> case _ of
    Nothing -> Array.reverse acc
    Just { head: i0 /\ pat0, tail } -> tail
      <#>
        ( \(i /\ pat) ->
            if sameToplevelSymbol pat0 pat then Left (i /\ subPats pat0)
            else Right (i /\ pat)
        )
      # partitionEither
      # \{ left, right } ->
          let
            acc' = Array.cons (pat0 /\ ([ i0 /\ subPats pat0 ] <> left)) acc
          in
            go acc' right

  sameToplevelSymbol = case _, _ of
    ECF.PatLiteral lit1, ECF.PatLiteral lit2 -> lit1 == lit2
    ECF.PatConstruct desc1 _, ECF.PatConstruct desc2 _ -> desc1.tag == desc2.tag
    ECF.PatArray pats1, ECF.PatArray pats2 -> Array.length pats1 == Array.length pats2
    _, _ -> false

  subPats :: ECF.Pattern -> Array ECF.Pattern
  subPats = case _ of
    ECF.PatConstruct _ pats -> pats
    ECF.PatAliase _ pat -> subPats pat
    _ -> []

partitionEither :: forall a b. Array (Either a b) -> { left :: Array a, right :: Array b }
partitionEither = go [] []
  where
  go left right = Array.uncons >>> case _ of
    Nothing -> { left, right }
    Just { head, tail } -> case head of
      Left a -> go (Array.snoc left a) right tail
      Right b -> go left (Array.snoc right b) tail
