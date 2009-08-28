module Domain.Math.Expr.Symbols where

import Domain.Math.Expr.Symbolic

-------------------------------------------------------------
-- Operator fixities

data Associativity = InfixLeft | InfixRight | InfixNon | Prefix | Postfix 
   deriving (Show, Eq)

operators :: [ (Associativity, [Symbol]) ]
operators =
   [ (InfixLeft, [plusSymbol, minusSymbol]) -- 6
   , (Prefix,    [negateSymbol])            -- 6+
   , (InfixLeft, [timesSymbol, divSymbol])  -- 7
   , (InfixRight, [powerSymbol])            -- 8
   ]

-------------------------------------------------------------
-- Math symbols

-- Num type class
plusSymbol    = makeSymbol "+"      2
timesSymbol   = makeSymbol "*"      2
minusSymbol   = makeSymbol "-"      2
absSymbol     = makeSymbol "abs"    1
signumSymbol  = makeSymbol "signum" 1
negateSymbol  = (makeSymbol "negate" 1)  { extraNames = ["-"] }

-- Fractional type class
divSymbol     = makeSymbol "/"      2

-- Floating type class
piSymbol      = makeConst  "pi"
sqrtSymbol    = makeSymbol "sqrt"   1
powerSymbol   = makeSymbol "^"      2   -- same as "**"
logSymbol     = makeSymbol "log"    2   -- in Haskell, logbase e = log
expSymbol     = makeSymbol "exp"    1   -- exp 1 ~= 2.718
lnSymbol      = makeSymbol "ln"     1   -- natural log
sinSymbol     = makeSymbol "sin"    1
tanSymbol     = makeSymbol "tan"    1
cosSymbol     = makeSymbol "cos"    1
asinSymbol    = makeSymbol "asin"   1
atanSymbol    = makeSymbol "atan"   1
acosSymbol    = makeSymbol "acos"   1
sinhSymbol    = makeSymbol "sinh"   1
tanhSymbol    = makeSymbol "tanh"   1
coshSymbol    = makeSymbol "cosh"   1
asinhSymbol   = makeSymbol "asinh"  1
atanhSymbol   = makeSymbol "atanh"  1
acoshSymbol   = makeSymbol "acosh"  1

-- Extra symbols
rootSymbol    = makeSymbol  "root" 2  
bottomSymbol  = makeSymbolN "error"
lambdaSymbol  = makeSymbolN "lambda"
diffSymbol    = makeSymbolN "diff"
fcompSymbol   = makeSymbolN "compose"

-------------------------------------------------------------
-- Some match functions

isPlus   a = isBinary plusSymbol   a
isTimes  a = isBinary timesSymbol  a
isMinus  a = isBinary minusSymbol  a
isDiv    a = isBinary divSymbol    a
isNegate a = isUnary  negateSymbol a

infixr 8 ^

(^) :: Symbolic a => a -> a -> a
(^) = binary powerSymbol

root :: Symbolic a => a -> a -> a
root = binary rootSymbol