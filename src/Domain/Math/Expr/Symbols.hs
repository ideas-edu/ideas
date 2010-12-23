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
   ( openMathSymbol
     -- OpenMath dictionary symbols
   , plusSymbol, timesSymbol, minusSymbol, divideSymbol, rootSymbol
   , powerSymbol, negateSymbol, sinSymbol, cosSymbol, lnSymbol
   , diffSymbol, piSymbol, lambdaSymbol, listSymbol
   , absSymbol, signumSymbol, logSymbol, expSymbol, tanSymbol, asinSymbol
   , atanSymbol, acosSymbol, sinhSymbol, tanhSymbol, coshSymbol, asinhSymbol
   , atanhSymbol, acoshSymbol, bottomSymbol, fcompSymbol
     -- Matching
   , isPlus, isTimes, isMinus, isDivide, isPower, isNegate, isRoot
   , isPowerSymbol, isRootSymbol, isLogSymbol, isDivideSymbol
   , (^), root
   ) where

import Common.Id
import Common.Rewriting
import Control.Monad
import Prelude hiding ((^))
import qualified Text.OpenMath.Dictionary.Arith1    as OM
import qualified Text.OpenMath.Dictionary.Calculus1 as OM
import qualified Text.OpenMath.Dictionary.Fns1      as OM
import qualified Text.OpenMath.Dictionary.List1     as OM
import qualified Text.OpenMath.Dictionary.Nums1     as OM
import qualified Text.OpenMath.Dictionary.Transc1   as OM
import qualified Text.OpenMath.Symbol               as OM

-- | Conversion function
openMathSymbol :: OM.Symbol -> Symbol
openMathSymbol s = newSymbol (OM.dictionary s # OM.symbolName s)

-------------------------------------------------------------
-- Arith1 dictionary

plusSymbol, timesSymbol, minusSymbol, divideSymbol, rootSymbol,
   powerSymbol, negateSymbol, absSymbol :: Symbol
   
plusSymbol   = openMathSymbol OM.plusSymbol
timesSymbol  = openMathSymbol OM.timesSymbol
minusSymbol  = openMathSymbol OM.minusSymbol
divideSymbol = openMathSymbol OM.divideSymbol
rootSymbol   = openMathSymbol OM.rootSymbol
powerSymbol  = openMathSymbol OM.powerSymbol
negateSymbol = openMathSymbol OM.unaryMinusSymbol
absSymbol    = openMathSymbol OM.absSymbol

-------------------------------------------------------------
-- Transc1 dictionary

logSymbol, sinSymbol, cosSymbol, lnSymbol, expSymbol, tanSymbol,
   sinhSymbol, tanhSymbol, coshSymbol :: Symbol

logSymbol  = openMathSymbol OM.logSymbol
sinSymbol  = openMathSymbol OM.sinSymbol
cosSymbol  = openMathSymbol OM.cosSymbol
lnSymbol   = openMathSymbol OM.lnSymbol
expSymbol  = openMathSymbol OM.expSymbol 
tanSymbol  = openMathSymbol OM.tanSymbol
sinhSymbol = openMathSymbol OM.sinhSymbol
tanhSymbol = openMathSymbol OM.tanhSymbol
coshSymbol = openMathSymbol OM.coshSymbol

-------------------------------------------------------------
-- Other dictionaries

diffSymbol, lambdaSymbol, listSymbol, piSymbol :: Symbol

diffSymbol   = openMathSymbol OM.diffSymbol
lambdaSymbol = openMathSymbol OM.lambdaSymbol
listSymbol   = openMathSymbol OM.listSymbol
piSymbol     = openMathSymbol OM.piSymbol

-------------------------------------------------------------
-- Extra math symbols

signumSymbol, asinSymbol, atanSymbol, acosSymbol, asinhSymbol, atanhSymbol,
   acoshSymbol, bottomSymbol, fcompSymbol :: Symbol

signumSymbol = newSymbol "signum"    
asinSymbol   = newSymbol "asin"   
atanSymbol   = newSymbol "atan"   
acosSymbol   = newSymbol "acos"     
asinhSymbol  = newSymbol "asinh"  
atanhSymbol  = newSymbol "atanh" 
acoshSymbol  = newSymbol "acosh"  
bottomSymbol = newSymbol "error"
fcompSymbol  = newSymbol "compose"

-------------------------------------------------------------
-- Some match functions

isPlus, isTimes, isMinus, isDivide, isPower, isRoot :: 
   (WithFunctions a, MonadPlus m) => a -> m (a, a)
isNegate :: (WithFunctions a, MonadPlus m) => a -> m a
   
isPlus   = isAssoBinary plusSymbol
isTimes  = isAssoBinary timesSymbol  
isMinus  = isBinary     minusSymbol  
isDivide = isBinary     divideSymbol 
isNegate = isUnary      negateSymbol 
isPower  = isBinary     powerSymbol
isRoot   = isBinary     rootSymbol

isPowerSymbol, isRootSymbol, isLogSymbol, isDivideSymbol :: Symbol -> Bool

isPowerSymbol  = (== powerSymbol)
isRootSymbol   = (== rootSymbol)
isLogSymbol    = (== logSymbol)
isDivideSymbol = (== divideSymbol)

infixr 8 ^

(^) :: WithFunctions a => a -> a -> a
(^) = binary powerSymbol

root :: WithFunctions a => a -> a -> a
root = binary rootSymbol

-- left-associative
isAssoBinary :: (WithFunctions a, Monad m) => Symbol -> a -> m (a, a)
isAssoBinary s a =
   case isFunction s a of
      Just [x, y] -> return (x, y)
      Just (x:xs) | length xs > 1 -> return (x, function s xs)
      _ -> fail "isAssoBinary"