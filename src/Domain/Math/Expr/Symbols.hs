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
   , eqSymbol
     -- calculus1
   , diffSymbol
   ) where

import Domain.Math.Expr.Symbolic
import Text.OpenMath.Symbol
import Text.OpenMath.Dictionary.Arith1
import Text.OpenMath.Dictionary.Logic1
import Text.OpenMath.Dictionary.List1
import Text.OpenMath.Dictionary.Relation1
import Text.OpenMath.Dictionary.Calculus1

-- Check (rationalSymbol  , oms "nums1" "rational")

-------------------------------------------------------------
-- Operator fixities

data Associativity = InfixLeft | InfixRight | Prefix -- InfixNon | Postfix
   deriving (Show, Eq)

operators :: [ (Associativity, [(Symbol, String)]) ]
operators =
   [ (InfixLeft,  [(plusSymbol, "+"), (minusSymbol, "-")])    -- 6
   , (Prefix,     [(negateSymbol, "-")])                      -- 6+
   , (InfixLeft,  [(timesSymbol, "*"), (divideSymbol, "/")])  -- 7
   , (InfixRight, [(powerSymbol, "^")])                       -- 8
   ]

-------------------------------------------------------------
-- Extra math symbols

-- rename
negateSymbol = unary_minusSymbol

absSymbol    = extraSymbol "abs"   
signumSymbol = extraSymbol "signum" 
piSymbol     = makeSymbol  "nums1" "pi"
sqrtSymbol   = extraSymbol "sqrt"   
logSymbol    = extraSymbol "log"            -- in Haskell, logbase e = log
expSymbol    = extraSymbol "exp"            -- exp 1 ~= 2.718
lnSymbol     = makeSymbol  "transc1" "ln"   -- natural log
sinSymbol    = makeSymbol  "transc1" "sin"    
tanSymbol    = extraSymbol "tan"    
cosSymbol    = makeSymbol  "transc1" "cos"    
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
lambdaSymbol = makeSymbol  "fns1" "lambda"
fcompSymbol  = extraSymbol "compose"

-------------------------------------------------------------
-- Some match functions

isPlus   a = isAssoBinary plusSymbol   a
isTimes  a = isAssoBinary timesSymbol  a
isMinus  a = isBinary     minusSymbol  a
isDivide a = isBinary     divideSymbol a
isNegate a = isUnary      negateSymbol a
isSqrt   a = isUnary      sqrtSymbol   a

infixr 8 ^

(^) :: Symbolic a => a -> a -> a
(^) = binary powerSymbol

root :: Symbolic a => a -> a -> a
root = binary rootSymbol