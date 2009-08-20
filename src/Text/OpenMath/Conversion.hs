-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Defines a mapping between openmath objects and data-types
--
-----------------------------------------------------------------------------
module Text.OpenMath.Conversion where

import Common.Utils
import Text.OpenMath.Object
import Domain.LinearAlgebra.Vector
import Domain.LinearAlgebra.Matrix
import Domain.Math.Symbolic
import Domain.Math.Data.Equation
import Domain.Math.Expr
import Data.Maybe
import Data.Ratio
import Control.Monad

class IsOMOBJ a where
   toOMOBJ   :: a -> OMOBJ
   fromOMOBJ :: OMOBJ -> Maybe a

--------------------------------------------------------------------
-- OpenMath symbols

plusSymbol      = OMS "arith1" "plus"
timesSymbol     = OMS "arith1" "times"
minusSymbol     = OMS "arith1" "minus"
divideSymbol    = OMS "arith1" "divide"
negateSymbol    = OMS "arith1" "unary_minus"
rootSymbol      = OMS "arith1" "root"
powerSymbol     = OMS "arith1" "power"

rationalSymbol  = OMS "nums1" "rational"
piSymbol        = OMS "nums1" "pi"

vectorSymbol    = OMS "linalg2" "vector"
matrixSymbol    = OMS "linalg2" "matrix"
matrixRowSymbol = OMS "linalg2" "matrixrow"

sinSymbol       = OMS "transc1" "sin"
cosSymbol       = OMS "transc1" "cos"
lnSymbol        = OMS "transc1" "ln"

listSymbol      = OMS "list1" "list"
equationSymbol  = OMS "relation1" "eq"
diffSymbol      = OMS "calculus1" "diff"
lambdaSymbol    = OMS "fns1" "lambda"

orSymbol        = OMS "logic1" "or" -- n-ary symbol

type SymbolMap = [(String, OMOBJ, Maybe Int)]

symbolMap :: SymbolMap
symbolMap = 
   [ ("pi", piSymbol, Just 0), ("Diff", diffSymbol, Just 1), ("sin", sinSymbol, Just 1)
   , ("cos", cosSymbol, Just 1), ("ln", lnSymbol, Just 1) -- , ("power", powerSymbol, Just 2)
   , ("^", powerSymbol, Just 2)
   ]
   
findByName :: String -> Maybe (OMOBJ, Maybe Int)
findByName n = safeHead [ (s, ma) | (m, s, ma) <- symbolMap, n==m  ]

findBySymbol :: OMOBJ -> Maybe (String, Maybe Int)
findBySymbol s = safeHead [ (n, ma) | (n, t, ma) <- symbolMap, s==t  ]

unknown :: String -> OMOBJ
unknown = OMS "UNKNOWN"

--------------------------------------------------------------------
-- Utility functions

unop :: IsOMOBJ a => OMOBJ -> a -> OMOBJ
unop symbol x = OMA [symbol, toOMOBJ x]

binop :: (IsOMOBJ a, IsOMOBJ b) => OMOBJ -> a -> b -> OMOBJ
binop symbol x y = OMA [symbol, toOMOBJ x, toOMOBJ y]

listop :: IsOMOBJ a => OMOBJ -> [a] -> OMOBJ
listop symbol xs = OMA (symbol : map toOMOBJ xs)

from1 :: IsOMOBJ a => OMOBJ -> (a -> b) -> OMOBJ -> Maybe b
from1 symbol f obj =
   case obj of 
      OMA [s, x] | s==symbol -> liftM f (fromOMOBJ x)
      _ -> Nothing

from2 :: (IsOMOBJ a, IsOMOBJ b) => OMOBJ -> (a -> b -> c) -> OMOBJ -> Maybe c
from2 = from2s . return

from2s :: (IsOMOBJ a, IsOMOBJ b) => [OMOBJ] -> (a -> b -> c) -> OMOBJ -> Maybe c
from2s symbols f obj =
   case obj of 
      OMA [s, x, y] | s `elem` symbols -> liftM2 f (fromOMOBJ x) (fromOMOBJ y)
      _ -> Nothing

fromN :: IsOMOBJ a => OMOBJ -> ([a] -> b) -> OMOBJ -> Maybe b
fromN symbol f obj =
   case obj of 
      OMA (s:xs) | s==symbol -> liftM f (mapM fromOMOBJ xs)
      _ -> Nothing

(|>) :: (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
(f |> g) obj = f obj `mplus` g obj

-- local helper function
toRatio :: Integral a => a -> Ratio a
toRatio = fromIntegral

--------------------------------------------------------------------
-- Identity conversion

instance IsOMOBJ OMOBJ where
   toOMOBJ   = id
   fromOMOBJ = return

--------------------------------------------------------------------
-- Prelude types

instance IsOMOBJ Int where
   toOMOBJ   = toOMOBJ . toInteger
   fromOMOBJ = fmap fromInteger . fromOMOBJ

instance IsOMOBJ Integer where
   toOMOBJ = OMI
   fromOMOBJ (OMI n) = Just n
   fromOMOBJ obj     = from1 negateSymbol negate obj
  
instance (Integral a, IsOMOBJ a) => IsOMOBJ (Ratio a) where
   toOMOBJ r
      | j == 1    = toOMOBJ i
      | otherwise = binop divideSymbol i j
    where (i, j) = (numerator r, denominator r)
   fromOMOBJ =  from2s [divideSymbol, rationalSymbol] (\i j -> toRatio i / toRatio j)
             |> from1 negateSymbol negate
             |> (fmap toRatio . fromOMOBJ)

instance IsOMOBJ a => IsOMOBJ [a] where 
   toOMOBJ   = listop listSymbol
   fromOMOBJ = fromN listSymbol id

instance (IsOMOBJ a, IsOMOBJ b) => IsOMOBJ (Either a b) where
   toOMOBJ = either toOMOBJ toOMOBJ
   fromOMOBJ e =
      case fromOMOBJ e of
         Just a  -> Just (Left a)
         Nothing -> fmap Right (fromOMOBJ e)

--------------------------------------------------------------------
-- General type for mathematical expressions

instance IsOMOBJ Expr where 
   toOMOBJ expr =
      case expr of
         -- the special conversions are disabled after having verified the 
         -- prefered representation with Hans Cuypers
         -- Negate (Con n) -> toOMOBJ $ Con $ negate n
         -- Negate (Con n :/: a) -> toOMOBJ $ Con (negate n) :/: a
         
         -- normal conversions
         a :+: b  -> binop plusSymbol  a b
         a :*: b  -> binop timesSymbol a b
         a :-: b  -> binop minusSymbol a b
         Negate a -> unop  negateSymbol a
         Nat n    -> toOMOBJ n
         a :/: b  -> binop divideSymbol a b
         Sqrt a   -> binop rootSymbol a (2::Integer)
         Var s    -> OMV s
         Sym "Lambda" [Var x, e] -> OMBIND lambdaSymbol [x] (toOMOBJ e)
         Sym f xs
            | null xs      -> symbol
            | otherwise    -> listop symbol xs
          where
            symbol = maybe (unknown f) fst $ findByName f
   fromOMOBJ =  from2 plusSymbol  (+)
             |> from2 timesSymbol (*)
             |> from2 minusSymbol (-)
             |> from1 negateSymbol negate
             |> (liftM Nat . fromOMOBJ)
             |> from2s [divideSymbol, rationalSymbol] (/)
             |> fromSqrt
             |> fromVar
             |> fromLambda
             |> fromSym
    where
      fromSqrt (OMA [s,x,OMI 2]) | s == rootSymbol =
         liftM Sqrt (fromOMOBJ x)
      fromSqrt _ = Nothing
      
      fromVar (OMV s) = Just (Var s)
      fromVar _ = Nothing
      
      fromLambda (OMBIND s [x] body) | s == lambdaSymbol = 
         liftM (\e -> Sym "Lambda" [Var x, e]) (fromOMOBJ body)
      fromLambda _ = Nothing
      
      fromSym obj@(OMS _ _) = fmap (symbol . fst) (findBySymbol obj)
      fromSym (OMA (s:xs)) = 
         case findBySymbol s of 
            Just (name, ma) | maybe True (\a -> length xs == a) ma
               -> liftM (function name) (mapM fromOMOBJ xs)
            _ -> Nothing
      fromSym _ = Nothing

--------------------------------------------------------------------
-- Linear algebra types

instance IsOMOBJ a => IsOMOBJ (Vector a) where
   toOMOBJ   = listop vectorSymbol . toList
   fromOMOBJ = fromN vectorSymbol fromList
   
instance IsOMOBJ a => IsOMOBJ (Matrix a) where
   toOMOBJ   = listop matrixSymbol . map (listop matrixRowSymbol) . rows
   fromOMOBJ = 
      let fromRow = fromN matrixRowSymbol id 
      in  fmap makeMatrix . join . fromN matrixSymbol (mapM fromRow)
   
instance IsOMOBJ a => IsOMOBJ (Equation a) where
   toOMOBJ (x :==: y) = binop equationSymbol x y
   fromOMOBJ = from2 equationSymbol (:==:)

-- instance Num a => IsOMOBJ (LinearExpr a)
