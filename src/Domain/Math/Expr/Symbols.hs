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

import Common.Rewriting hiding (Symbol)
import Control.Monad
import Domain.Math.Data.Relation (relationSymbols)
import Domain.Math.Expr.Symbolic
import Prelude hiding ((^))
import Text.OpenMath.Dictionary.Arith1
import Text.OpenMath.Dictionary.Calculus1
import Text.OpenMath.Dictionary.Fns1
import Text.OpenMath.Dictionary.List1
import Text.OpenMath.Dictionary.Nums1
import Text.OpenMath.Dictionary.Transc1
import Text.OpenMath.Symbol (extraSymbol, Symbol)

-------------------------------------------------------------
-- Operator fixities

type OperatorTable = [(Associativity, [(Symbol, String)])]

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

negateSymbol :: Symbol
negateSymbol = unaryMinusSymbol

-------------------------------------------------------------
-- Extra math symbols

signumSymbol, asinSymbol, atanSymbol, acosSymbol, asinhSymbol, atanhSymbol,
   acoshSymbol, bottomSymbol, fcompSymbol :: Symbol

signumSymbol = extraSymbol "signum"    
asinSymbol   = extraSymbol "asin"   
atanSymbol   = extraSymbol "atan"   
acosSymbol   = extraSymbol "acos"     
asinhSymbol  = extraSymbol "asinh"  
atanhSymbol  = extraSymbol "atanh" 
acoshSymbol  = extraSymbol "acosh"  
bottomSymbol = extraSymbol "error"
fcompSymbol  = extraSymbol "compose"

-------------------------------------------------------------
-- Some match functions

isPlus, isTimes, isMinus, isDivide, isPower :: 
   (Symbolic a, MonadPlus m) => a -> m (a, a)
isNegate :: (Symbolic a, MonadPlus m) => a -> m a
   
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

(^) :: Symbolic a => a -> a -> a
(^) = binary powerSymbol

root :: Symbolic a => a -> a -> a
root = binary rootSymbol