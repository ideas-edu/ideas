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
   ( -- * Constants
     Constant, makeConstant, simpleConstant
   , constant, isConstant, constantView
     -- * Unary operators
   , UnaryOp, makeUnary, simpleUnary
   , unary, isUnary, unaryMatch, unaryView
     -- * Binary operators
   , BinaryOp, makeBinary, simpleBinary
   , binary, isBinary, binaryMatch, binaryView
   ) where

import Common.Id
import Common.Uniplate
import Common.View
import Data.Maybe
import Control.Monad

----------------------------------------------------------------------
-- Constants

data Constant a = C
   { constantId :: Id
   , constant   :: a
   , isConstant :: a -> Bool
   }

instance Show (Constant a) where
   show = showId

instance HasId (Constant a) where
   getId = constantId
   changeId f op = op {constantId = f (constantId op)}

makeConstant :: IsId n => n -> a -> (a -> Bool) -> Constant a
makeConstant = C . newId

simpleConstant :: (IsId n, Eq a) => n -> a -> Constant a
simpleConstant n a = makeConstant n a (==a)

constantView :: Constant a -> View a ()
constantView (C i a p) = newView i (guard . p) (const a)

----------------------------------------------------------------------
-- Unary operators

data UnaryOp a = U
   { unaryId    :: Id
   , unary      :: a -> a
   , unaryMatch :: a -> Maybe a
   }

instance Show (UnaryOp a) where
   show = showId

instance HasId (UnaryOp a) where
   getId = unaryId
   changeId f op = op {unaryId = f (unaryId op)}

makeUnary :: IsId n => n -> (a -> a) -> (a -> Maybe a) -> UnaryOp a
makeUnary = U . newId

simpleUnary :: (IsId n, Uniplate a, Eq a) => n -> (a -> a) -> UnaryOp a
simpleUnary n op = makeUnary n op f
 where
   f a = case children a of
            [x] | op x == a -> Just x
            _ -> Nothing

isUnary :: UnaryOp a -> a -> Bool
isUnary op = isJust . unaryMatch op

unaryView :: UnaryOp a -> View a a
unaryView (U i op m) = newView i m op

----------------------------------------------------------------------
-- Binary operators

data BinaryOp a = B 
   { binaryId    :: Id
   , binary      :: a -> a -> a
   , binaryMatch :: a -> Maybe (a, a)
   }

instance Show (BinaryOp a) where
   show = showId

instance HasId (BinaryOp a) where
   getId = binaryId
   changeId f op = op {binaryId = f (binaryId op)}

makeBinary :: IsId n => n -> (a -> a -> a) -> (a -> Maybe (a, a)) -> BinaryOp a
makeBinary = B . newId

simpleBinary :: (IsId n, Uniplate a, Eq a) => n -> (a -> a -> a) -> BinaryOp a
simpleBinary n op = makeBinary n op f
 where
   f a = case children a of
            [x, y] | op x y == a -> Just (x, y)
            _ -> Nothing

isBinary :: BinaryOp a -> a -> Bool
isBinary op = isJust . binaryMatch op

binaryView :: BinaryOp a -> View a (a, a)
binaryView (B n op m) = newView n m (uncurry op)