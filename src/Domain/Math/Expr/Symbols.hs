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
-----------------------------------------------------------------------------
module Domain.Math.Expr.Symbols 
   ( module Domain.Math.Expr.Symbols
     -- arith1
   , plusSymbol, timesSymbol, minusSymbol, divideSymbol
   , rootSymbol, powerSymbol
     -- logic1
   , orSymbol, trueSymbol, falseSymbol
     -- list1
   , listSymbol
     -- relation1
   , eqSymbol, ltSymbol, gtSymbol, neqSymbol
   , leqSymbol, geqSymbol, approxSymbol
     -- calculus1
   , diffSymbol
     -- nusm1
   , piSymbol
     -- fns1
   , lambdaSymbol
     -- transc1
   , sinSymbol, cosSymbol, lnSymbol
   ) where

import Control.Monad
import Domain.Math.Expr.Symbolic
import Text.OpenMath.Symbol
import Text.OpenMath.Dictionary.Arith1
import Text.OpenMath.Dictionary.Logic1
import Text.OpenMath.Dictionary.List1
import Text.OpenMath.Dictionary.Relation1
import Text.OpenMath.Dictionary.Calculus1
import Text.OpenMath.Dictionary.Nums1
import Text.OpenMath.Dictionary.Fns1
import Text.OpenMath.Dictionary.Transc1

-- Check (rationalSymbol  , oms "nums1" "rational")

-------------------------------------------------------------
-- Operator fixities

type OperatorTable = [(Associativity, [(Symbol, String)])]

data Associativity = InfixLeft | InfixRight | Prefix -- InfixNon | Postfix
   deriving (Show, Eq)

operatorTable :: OperatorTable
operatorTable =
   [ (InfixLeft,  [(plusSymbol, "+"), (minusSymbol, "-")])    -- 6
   , (Prefix,     [(negateSymbol, "-")])                      -- 6+
   , (InfixLeft,  [(timesSymbol, "*"), (divideSymbol, "/")])  -- 7
   , (InfixRight, [(powerSymbol, "^")])                       -- 8
   ]

-------------------------------------------------------------
-- Extra math symbols

-- rename
negateSymbol = unaryMinusSymbol

absSymbol    = extraSymbol "abs"   
signumSymbol = extraSymbol "signum" 
logSymbol    = extraSymbol "log"            -- in Haskell, logbase e = log
expSymbol    = extraSymbol "exp"            -- exp 1 ~= 2.718
tanSymbol    = extraSymbol "tan"       
asinSymbol   = extraSymbol "asin"   
atanSymbol   = extraSymbol "atan"   
acosSymbol   = extraSymbol "acos"   
sinhSymbol   = extraSymbol "sinh"   
tanhSymbol   = extraSymbol "tanh"   
coshSymbol   = extraSymbol "cosh"   
asinhSymbol  = extraSymbol "asinh"  
atanhSymbol  = extraSymbol "atanh" 
acoshSymbol  = extraSymbol "acosh"  
bottomSymbol = extraSymbol "error"
fcompSymbol  = extraSymbol "compose"

-------------------------------------------------------------
-- Some match functions

isPlus, isTimes, isMinus, isDivide :: 
   (Symbolic a, MonadPlus m) => a -> m (a, a)
isNegate :: (Symbolic a, MonadPlus m) => a -> m a
   
isPlus   = isAssoBinary plusSymbol
isTimes  = isAssoBinary timesSymbol  
isMinus  = isBinary     minusSymbol  
isDivide = isBinary     divideSymbol 
isNegate = isUnary      negateSymbol 

infixr 8 ^

(^) :: Symbolic a => a -> a -> a
(^) = binary powerSymbol

root :: Symbolic a => a -> a -> a
root = binary rootSymbol