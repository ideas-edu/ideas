-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Context where

import Domain.LinearAlgebra.Matrix
import Common.Move
import Control.Monad

data MatrixInContext a = MIC
   { matrix    :: Matrix a
   , covered   :: Int
   , columnJ   :: Int
   , rowR      :: Int 
   , curRow    :: Int
   , curColumn :: Int
   , value     :: a
   }
 deriving Show

instance Move (MatrixInContext a) where
   moveLeft c = do
      guard (curColumn c > 0)
      return c {curColumn = curColumn c-1}
   moveRight c = do
      guard (curColumn c + 1 < snd (dimensions $ matrix c))
      return c {curColumn = curColumn c+1}
   moveUp c = do
      guard (curRow c > 0)
      return c {curRow = curRow c-1}
   moveDown c = do
      guard (curRow c + 1 < fst (dimensions $ matrix c))
      return c {curRow = curRow c+1}

instance Eq a => Eq (MatrixInContext a) where
   x==y = matrix x==matrix y 
   
inContext :: Num a => Matrix a -> MatrixInContext a
inContext m = MIC m 0 0 0 0 0 0
