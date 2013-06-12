-- Automatically generated from content dictionary arith1.ocd.  Do not change.
module Ideas.Text.OpenMath.Dictionary.Arith1 where

import Ideas.Text.OpenMath.Symbol

-- | List of symbols defined in arith1 dictionary
arith1List :: [Symbol]
arith1List = [lcmSymbol, gcdSymbol, plusSymbol, unaryMinusSymbol, minusSymbol, timesSymbol, divideSymbol, powerSymbol, absSymbol, rootSymbol, sumSymbol, productSymbol]

{-| The symbol to represent the n-ary function to return the least common
multiple of its arguments. -}
lcmSymbol :: Symbol
lcmSymbol = makeSymbol "arith1" "lcm"

{-| The symbol to represent the n-ary function to return the gcd (greatest
common divisor) of its arguments. -}
gcdSymbol :: Symbol
gcdSymbol = makeSymbol "arith1" "gcd"

{-| The symbol representing an n-ary commutative function plus. -}
plusSymbol :: Symbol
plusSymbol = makeSymbol "arith1" "plus"

{-| This symbol denotes unary minus, i.e. the additive inverse. -}
unaryMinusSymbol :: Symbol
unaryMinusSymbol = makeSymbol "arith1" "unary_minus"

{-| The symbol representing a binary minus function. This is equivalent to
adding the additive inverse. -}
minusSymbol :: Symbol
minusSymbol = makeSymbol "arith1" "minus"

{-| The symbol representing an n-ary multiplication function. -}
timesSymbol :: Symbol
timesSymbol = makeSymbol "arith1" "times"

{-| This symbol represents a (binary) division function denoting the first
argument right-divided by the second, i.e. divide(a,b)=a*inverse(b). It is the
inverse of the multiplication function defined by the symbol times in this CD.
-}
divideSymbol :: Symbol
divideSymbol = makeSymbol "arith1" "divide"

{-| This symbol represents a power function. The first argument is raised to
the power of the second argument. When the second argument is not an integer,
powering is defined in terms of exponentials and logarithms for the complex
and real numbers. This operator can represent general powering. -}
powerSymbol :: Symbol
powerSymbol = makeSymbol "arith1" "power"

{-| A unary operator which represents the absolute value of its argument. The
argument should be numerically valued. In the complex case this is often
referred to as the modulus. -}
absSymbol :: Symbol
absSymbol = makeSymbol "arith1" "abs"

{-| A binary operator which represents its first argument "lowered" to its
n'th root where n is the second argument. This is the inverse of the operation
represented by the power symbol defined in this CD. Care should be taken as to
the precise meaning of this operator, in particular which root is represented,
however it is here to represent the general notion of taking n'th roots. As
inferred by the signature relevant to this symbol, the function represented by
this symbol is the single valued function, the specific root returned is the
one indicated by the first CMP. Note also that the converse of the second CMP
is not valid in general. -}
rootSymbol :: Symbol
rootSymbol = makeSymbol "arith1" "root"

{-| An operator taking two arguments, the first being the range of summation,
e.g. an integral interval, the second being the function to be summed. Note
that the sum may be over an infinite interval. -}
sumSymbol :: Symbol
sumSymbol = makeSymbol "arith1" "sum"

{-| An operator taking two arguments, the first being the range of
multiplication e.g. an integral interval, the second being the function to be
multiplied. Note that the product may be over an infinite interval. -}
productSymbol :: Symbol
productSymbol = makeSymbol "arith1" "product"