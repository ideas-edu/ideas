-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A collection of strategy combinators: all lifted to work on different
-- data types
--
-----------------------------------------------------------------------------
module Common.Strategy.Combinators where

import Prelude hiding (not, repeat, fail, sequence)
import Common.Context
import Common.Transformation
import Common.Uniplate
import Common.Strategy.Core
import Common.Strategy.Abstract

-----------------------------------------------------------
--- Strategy combinators

-- Basic combinators --------------------------------------

infixr 3 <|>
infixr 4  |>
infixr 5 <*>

-- | Put two strategies in sequence (first do this, then do that)
(<*>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(<*>) = liftCore2 (:*:)

-- | Choose between the two strategies (either do this or do that)
(<|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(<|>) = liftCore2 (:|:)

-- | The strategy that always succeeds (without doing anything)
succeed :: Strategy a
succeed = fromCore Succeed

-- | The strategy that always fails
fail :: Strategy a
fail = fromCore Fail

-- | Puts a list of strategies into a sequence
sequence :: IsStrategy f => [f a] -> Strategy a
sequence = foldr ((<*>) . toStrategy) succeed

-- | Combines a list of alternative strategies
alternatives :: IsStrategy f => [f a] -> Strategy a
alternatives = foldr ((<|>) . toStrategy) fail

-- EBNF combinators --------------------------------------

-- | Repeat a strategy zero or more times (non-greedy)
many :: IsStrategy f => f a -> Strategy a
many = liftCore Many

-- | Apply a certain strategy at least once (non-greedy)
many1 :: IsStrategy f => f a -> Strategy a
many1 s = s <*> many s

-- | Apply a strategy a certain number of times
replicate :: IsStrategy f => Int -> f a -> Strategy a
replicate n = sequence . Prelude.replicate n

-- | Apply a certain strategy or do nothing (non-greedy)
option :: IsStrategy f => f a -> Strategy a
option s = s <|> succeed   

-- Negation and greedy combinators ----------------------

-- | Checks whether a predicate holds for the current term. The
--   check is considered to be a minor step.
check :: (a -> Bool) -> Strategy a
check p = toStrategy (checkRule p)

-- | Check whether or not the argument strategy cannot be applied: the result
--   strategy only succeeds if this is not the case (otherwise it fails).
not :: IsStrategy f => f a -> Strategy a
not = liftCore (Not . noLabels)

-- | Repeat a strategy zero or more times (greedy version of 'many')
repeat :: IsStrategy f => f a -> Strategy a
repeat s = many s <*> not s

-- | Apply a certain strategy at least once (greedy version of 'many1')
repeat1 :: IsStrategy f => f a -> Strategy a
repeat1 s = many1 s <*> not s

-- | Apply a certain strategy if this is possible (greedy version of 'option')
try :: IsStrategy f => f a -> Strategy a
try s = s <|> not s

-- | Left-biased choice: if the left-operand strategy can be applied, do so. Otherwise,
--   try the right-operand strategy
(|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(|>) = liftCore2 (:|>:)

-- | Apply the strategies from the list exhaustively (until this is no longer possible)
exhaustive :: IsStrategy f => [f a] -> Strategy a
exhaustive = repeat . alternatives

-- Traversal combinators --------------------------------------------

-- | A fix-point combinator on strategies (to model recursion). Powerful
-- (but dangerous) combinator
fix :: (Strategy a -> Strategy a) -> Strategy a
fix f = fromCore (fixCore (toCore . f . fromCore))

-- | Apply a strategy on (exactly) one of the term's direct children
once :: (IsStrategy f, Uniplate a) => f (Context a) -> Strategy (Context a)
once s = ruleMoveDown <*> s <*> ruleMoveUp
 where
   ruleMoveDown = minorRule $ makeSimpleRuleList "MoveDown" moveDown
   moveDown c = 
      let n = maybe 0 (pred . length . children) (currentFocus c)
      in [ changeLocation (locationDown i) c | i <- [0 .. n] ]
   
   ruleMoveUp = minorRule $ makeSimpleRule "MoveUp" moveUp
   moveUp c   = do
      new <- locationUp (location c)
      return $ setLocation new c

-- | Apply a strategy somewhere in the term
somewhere :: (IsStrategy f, Uniplate a) => f (Context a) -> Strategy (Context a)
somewhere s = fix $ \this -> s <|> once this

-- | Search for a suitable location in the term to apply the strategy using a
-- top-down approach
topDown :: (IsStrategy f, Uniplate a) => f (Context a) -> Strategy (Context a)
topDown s = fix $ \this -> s |> once this

-- | Search for a suitable location in the term to apply the strategy using a
-- bottom-up approach
bottomUp :: (IsStrategy f, Uniplate a) => f (Context a) -> Strategy (Context a)
bottomUp s = fix $ \this -> once this <|> (not (once (bottomUp s)) <*> s)

{- The ideal implementation does not yet work: there appears to be a strange
   interplay between the fixpoint operator (with variables) and the not combinator
   > bottomUp s = fix $ \this -> once this |> s -}
   
hide, fold, skip :: IsLabeled f => f a -> LabeledStrategy a
hide = changeInfo (\info -> info {hidden  = True})
fold = changeInfo (\info -> info {folded  = True})
skip = changeInfo (\info -> info {skipped = True})