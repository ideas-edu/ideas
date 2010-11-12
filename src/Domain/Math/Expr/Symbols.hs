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
-- Exports relevant OpenMath symbols
--
-----------------------------------------------------------------------------
module Domain.Math.Expr.Symbols
   ( -- Operators
     OperatorTable, Associativity(..), operatorTable
     -- OpenMath symbols
   , plusSymbol, timesSymbol, minusSymbol, divideSymbol, rootSymbol
   , powerSymbol, negateSymbol, sinSymbol, cosSymbol, lnSymbol
   , diffSymbol, piSymbol, lambdaSymbol, listSymbol
   , absSymbol, signumSymbol, logSymbol, expSymbol, tanSymbol, asinSymbol
   , atanSymbol, acosSymbol, sinhSymbol, tanhSymbol, coshSymbol, asinhSymbol
   , atanhSymbol, acoshSymbol, bottomSymbol, fcompSymbol
     -- Matching
   , isPlus, isTimes, isMinus, isDivide, isPower, isNegate
   , isPowerSymbol, isRootSymbol, isLogSymbol, isDivideSymbol
   , (^), root
   ) where

import Common.Rewriting
import Control.Monad
import Domain.Math.Data.Relation (relationSymbols)
import Prelude hiding ((^))
import Text.OpenMath.Dictionary.Arith1
import Text.OpenMath.Dictionary.Calculus1
import Text.OpenMath.Dictionary.Fns1
import Text.OpenMath.Dictionary.List1
import Text.OpenMath.Dictionary.Nums1
import Text.OpenMath.Dictionary.Transc1
import qualified Text.OpenMath.Symbol as OM

-------------------------------------------------------------
-- Operator fixities

type OperatorTable = [(Associativity, [(OM.Symbol, String)])]

data Associativity = InfixLeft | InfixRight | PrefixNon
                   | InfixNon
   deriving (Show, Eq)

operatorTable :: OperatorTable
operatorTable =
     (InfixNon, [ (s, op) | (_, (op, s)) <- relationSymbols]) :
   [ (InfixLeft,  [(plusSymbol, "+"), (minusSymbol, "-")])    -- 6
   , (PrefixNon,  [(negateSymbol, "-")])                      -- 6+
   , (InfixLeft,  [(timesSymbol, "*"), (divideSymbol, "/")])  -- 7
   , (InfixRight, [(powerSymbol, "^")])                       -- 8
   ]

-------------------------------------------------------------
-- OpenMath symbols

negateSymbol :: OM.Symbol
negateSymbol = unaryMinusSymbol

-------------------------------------------------------------
-- Extra math symbols

signumSymbol, asinSymbol, atanSymbol, acosSymbol, asinhSymbol, atanhSymbol,
   acoshSymbol, bottomSymbol, fcompSymbol :: OM.Symbol

signumSymbol = OM.extraSymbol "signum"    
asinSymbol   = OM.extraSymbol "asin"   
atanSymbol   = OM.extraSymbol "atan"   
acosSymbol   = OM.extraSymbol "acos"     
asinhSymbol  = OM.extraSymbol "asinh"  
atanhSymbol  = OM.extraSymbol "atanh" 
acoshSymbol  = OM.extraSymbol "acosh"  
bottomSymbol = OM.extraSymbol "error"
fcompSymbol  = OM.extraSymbol "compose"

-------------------------------------------------------------
-- Some match functions

isPlus, isTimes, isMinus, isDivide, isPower :: 
   (WithFunctions a, MonadPlus m) => a -> m (a, a)
isNegate :: (WithFunctions a, MonadPlus m) => a -> m a
   
isPlus   = isAssoBinary plusSymbol
isTimes  = isAssoBinary timesSymbol  
isMinus  = isBinary     minusSymbol  
isDivide = isBinary     divideSymbol 
isNegate = isUnary      negateSymbol 
isPower  = isBinary     powerSymbol

isPowerSymbol, isRootSymbol, isLogSymbol, isDivideSymbol :: IsSymbol a => a -> Bool
isPowerSymbol  = sameSymbol powerSymbol
isRootSymbol   = sameSymbol rootSymbol
isLogSymbol    = sameSymbol logSymbol
isDivideSymbol = sameSymbol divideSymbol

infixr 8 ^

(^) :: WithFunctions a => a -> a -> a
(^) = binary powerSymbol

root :: WithFunctions a => a -> a -> a
root = binary rootSymbol

-- left-associative
isAssoBinary :: (IsSymbol s, WithFunctions a, Monad m) => s -> a -> m (a, a)
isAssoBinary s a =
   case isFunction s a of
      Just [x, y] -> return (x, y)
      Just (x:xs) | length xs > 1 -> return (x, function s xs)
      _ -> fail "isAssoBinary"