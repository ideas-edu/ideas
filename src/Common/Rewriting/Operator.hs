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
   , UnaryOp, makeUnaryOp, simpleUnaryOp
   , unaryOp, isUnaryOp, unaryOpMatch, unaryOpView
     -- * Binary operators
   , BinaryOp, makeBinaryOp, simpleBinaryOp
   , binaryOp, isBinaryOp, binaryOpMatch, binaryOpView
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

instance HasId (Constant a) where
   getId = constantId
   changeId f op = op {constantId = f (constantId op)}

makeConstant :: IsId n => n -> a -> (a -> Bool) -> Constant a
makeConstant = C . newId

simpleConstant :: (IsId n, Eq a) => n -> a -> Constant a
simpleConstant n a = makeConstant n a (==a)

constantView :: Constant a -> View a ()
constantView (C i a p) = i @> makeView (guard . p) (const a)

----------------------------------------------------------------------
-- Unary operators

data UnaryOp a = U
   { unaryOpId    :: Id
   , unaryOp      :: a -> a
   , unaryOpMatch :: a -> Maybe a
   }

instance HasId (UnaryOp a) where
   getId = unaryOpId
   changeId f op = op {unaryOpId = f (unaryOpId op)}

makeUnaryOp :: IsId n => n -> (a -> a) -> (a -> Maybe a) -> UnaryOp a
makeUnaryOp = U . newId

simpleUnaryOp :: (IsId n, Uniplate a, Eq a) => n -> (a -> a) -> UnaryOp a
simpleUnaryOp n op = makeUnaryOp n op f
 where
   f a = case children a of
            [x] | op x == a -> Just x
            _ -> Nothing

isUnaryOp :: UnaryOp a -> a -> Bool
isUnaryOp op = isJust . unaryOpMatch op

unaryOpView :: UnaryOp a -> View a a
unaryOpView (U i op m) = i @> makeView m op

----------------------------------------------------------------------
-- Binary operators

data BinaryOp a = B 
   { binaryOpId    :: Id
   , binaryOp      :: a -> a -> a
   , binaryOpMatch :: a -> Maybe (a, a)
   }

instance HasId (BinaryOp a) where
   getId = binaryOpId
   changeId f op = op {binaryOpId = f (binaryOpId op)}

makeBinaryOp :: IsId n => n -> (a -> a -> a) -> (a -> Maybe (a, a)) -> BinaryOp a
makeBinaryOp = B . newId

simpleBinaryOp :: (IsId n, Uniplate a, Eq a) => n -> (a -> a -> a) -> BinaryOp a
simpleBinaryOp n op = makeBinaryOp n op f
 where
   f a = case children a of
            [x, y] | op x y == a -> Just (x, y)
            _ -> Nothing

isBinaryOp :: BinaryOp a -> a -> Bool
isBinaryOp op = isJust . binaryOpMatch op

binaryOpView :: BinaryOp a -> View a (a, a)
binaryOpView (B n op m) = n @> makeView m (uncurry op)