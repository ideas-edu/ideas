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

import Common.Transformation
import Domain.LinearAlgebra.Matrix
import Common.Move
import Control.Monad

data MatrixInContext a = MIC
   { matrix  :: Matrix a
   , covered :: Int
   , columnJ :: Int
   , focus   :: (Int, Int)
   }
 deriving Show

subMatrix :: MatrixInContext a -> Matrix a
subMatrix c = makeMatrix $ drop (covered c) $ rows $ matrix c

instance Move (MatrixInContext a) where
   moveLeft c = do
      let (y, x) = focus c
      guard (x > 0)
      return c {focus = (y, x-1)}
   moveRight c = do
      let (y, x) = focus c
      guard (x + 1 < snd (dimensions $ matrix c))
      return c {focus = (y, x+1)}
   moveUp c = do
      let (y, x) = focus c
      guard (y > 0)
      return c {focus = (y-1, x)}
   moveDown c = do
      let (y, x) = focus c
      guard (y + 1 < fst (dimensions $ matrix c))
      return c {focus = (y+1, x)}

instance Eq a => Eq (MatrixInContext a) where
   x == y  =  matrix x == matrix y 
   
inContext :: Num a => Matrix a -> MatrixInContext a
inContext m = MIC m 0 0 (0, 0)
