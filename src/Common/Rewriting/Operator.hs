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
-----------------------------------------------------------------------------
module Common.Rewriting.Operator 
   ( Operator, Operators, constructor, destructor
   , newOperator, associativeOperator, commutativeOperator, acOperator
   , makeAssociative, makeCommutative, isAssociative, isCommutative
   , collectWithOperator, buildWithOperator
   , isOperator, findOperator
   , normalizeWith, equalWith
   ) where

import Common.Uniplate
import Common.Utils
import Data.List
import Data.Maybe

type Operators a = [Operator a]

data Operator a = O 
   { constructor   :: a -> a -> a
   , destructor    :: a -> Maybe (a, a)
   , isAssociative :: Bool
   , isCommutative :: Bool
   }
   
newOperator :: (a -> a -> a) ->  (a -> Maybe (a, a)) -> Operator a
newOperator f g = O f g False False

associativeOperator, commutativeOperator, acOperator :: (a -> a -> a) ->  (a -> Maybe (a, a)) -> Operator a
associativeOperator f = makeAssociative . newOperator f
commutativeOperator f = makeCommutative . newOperator f
acOperator          f = makeAssociative . commutativeOperator f

makeCommutative, makeAssociative :: Operator a -> Operator a
makeCommutative op = op { isCommutative = True }
makeAssociative op = op { isAssociative = True  }

collectWithOperator :: Operator a -> a -> [a]
collectWithOperator op a
   | isAssociative op = rec a []
   | otherwise        = maybe [a] (\(x, y) -> [x, y]) (destructor op a)
 where
   rec a = case destructor op a of
              Just (x, y) -> rec x . rec y
              Nothing     -> (a:)

buildWithOperator :: Operator a -> [a] -> a
buildWithOperator op xs 
   | null xs = 
        error "Rewriting.buildWithOperator: empty list"
   | not (isAssociative op) && length xs > 2 =
        error "Rewriting.buildWithOperator: non-associative operator"
   | otherwise = 
        foldr1 (constructor op) xs
   
isOperator :: Operator a -> a -> Bool
isOperator op = isJust . destructor op

findOperator :: Operators a -> a -> Maybe (Operator a)
findOperator ops a = safeHead $ filter (`isOperator` a) ops

normalizeWith :: (Uniplate a, Ord a) => Operators a -> a -> a
normalizeWith ops = rec
 where
   rec a = 
      case findOperator ops a of
         Just op -> 
            buildWithOperator op $ (if isCommutative op then sort else id) $ map rec $ collectWithOperator op a
         Nothing -> 
            descend rec a

equalWith :: (Uniplate a, Ord a) => Operators a -> a -> a -> Bool
equalWith ops x y = normalizeWith ops x == normalizeWith ops y