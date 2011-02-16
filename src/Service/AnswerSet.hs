-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A diagnosis for answer sets
--
-----------------------------------------------------------------------------
module Service.AnswerSet
   ( AnswerSet(..), compareParts
   ) where

import Data.List

{- About efficiency: 
   * Use sets for difference (but this requires an ordering)
   * normalization is currently part of equivalence function (computed repeatly)
   * Diagnose uses stateSolution (instead of a more efficient calculation)
-}


data AnswerSet a = Correct           -- ^ the answer is correct
                 | Missing       [a] -- ^ a part of the answer is missing
                 | IncorrectPart [a] --
                 | Incorrect         -- ^ all parts of the answer are incorrect

instance Functor AnswerSet where
   fmap f answer =
      case answer of
         Correct          -> Correct
         Missing xs       -> Missing (map f xs)
         IncorrectPart xs -> IncorrectPart (map f xs)
         Incorrect        -> Incorrect

-- | Uses a function for splitting (into parts), and a function for
-- (semantic!) equivalence
compareParts :: (b -> b -> Bool) -> (a -> [b]) -> a -> a -> AnswerSet b
compareParts eq split a b = answerSet eq (split a) (split b)
   
-- | First argument is expected answer; second argument is student answer
answerSet :: (a -> a -> Bool) -> [a] -> [a] -> AnswerSet a
answerSet eq as bs
   | null xa && null xb = Correct
   | null xb            = Missing xa -- xa is not empty
   | null both          = Incorrect
   | otherwise          = IncorrectPart xb
 where
   (both, xa) = partition (contains bs) as
   xb         = filter (not . contains as) bs -- elements in b (but not in a)
   contains   = flip (any . eq)