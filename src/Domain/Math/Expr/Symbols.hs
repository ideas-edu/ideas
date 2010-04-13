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
-- Exports relevant OpenMath symbols, converted to the 
-- Symbol data type from @Common.Rewriting@.
--
-----------------------------------------------------------------------------
module Domain.Math.Expr.Symbols where

import Control.Monad
import Domain.Math.Expr.Symbolic
import Domain.Math.Data.Relation (relationSymbols)

-- OpenMath dictionaries
import qualified Text.OpenMath.Dictionary.Arith1    as Arith1
import qualified Text.OpenMath.Dictionary.Calculus1 as Calculus1
import qualified Text.OpenMath.Dictionary.Fns1      as Fns1
import qualified Text.OpenMath.Dictionary.List1     as List1
import qualified Text.OpenMath.Dictionary.Nums1     as Nums1
import qualified Text.OpenMath.Dictionary.Transc1   as Transc1

-------------------------------------------------------------
-- Converted OpenMath symbols

plusSymbol, timesSymbol, minusSymbol, divideSymbol,
   rootSymbol, powerSymbol, negateSymbol :: Symbol
plusSymbol       = toSymbol Arith1.plusSymbol
timesSymbol      = toSymbol Arith1.timesSymbol
minusSymbol      = toSymbol Arith1.minusSymbol 
divideSymbol     = toSymbol Arith1.divideSymbol
rootSymbol       = toSymbol Arith1.rootSymbol
powerSymbol      = toSymbol Arith1.powerSymbol
negateSymbol     = toSymbol Arith1.unaryMinusSymbol

sinSymbol, cosSymbol, lnSymbol :: Symbol
sinSymbol        = toSymbol Transc1.sinSymbol
cosSymbol        = toSymbol Transc1.cosSymbol
lnSymbol         = toSymbol Transc1.lnSymbol

diffSymbol, piSymbol, lambdaSymbol, listSymbol :: Symbol
diffSymbol       = toSymbol Calculus1.diffSymbol
piSymbol         = toSymbol Nums1.piSymbol
lambdaSymbol     = toSymbol Fns1.lambdaSymbol
listSymbol       = toSymbol List1.listSymbol

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
-- Extra math symbols

absSymbol    = toSymbol "abs"   
signumSymbol = toSymbol "signum" 
logSymbol    = toSymbol "log"            -- in Haskell, logbase e = log
expSymbol    = toSymbol "exp"            -- exp 1 ~= 2.718
tanSymbol    = toSymbol "tan"       
asinSymbol   = toSymbol "asin"   
atanSymbol   = toSymbol "atan"   
acosSymbol   = toSymbol "acos"   
sinhSymbol   = toSymbol "sinh"   
tanhSymbol   = toSymbol "tanh"   
coshSymbol   = toSymbol "cosh"   
asinhSymbol  = toSymbol "asinh"  
atanhSymbol  = toSymbol "atanh" 
acoshSymbol  = toSymbol "acosh"  
bottomSymbol = toSymbol "error"
fcompSymbol  = toSymbol "compose"

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