-- Automatically generated from content dictionary nums1.ocd.  Do not change.
module Ideas.Text.OpenMath.Dictionary.Nums1 where

import Ideas.Text.OpenMath.Symbol

-- | List of symbols defined in nums1 dictionary
nums1List :: [Symbol]
nums1List = [basedIntegerSymbol, rationalSymbol, infinitySymbol, eSymbol, iSymbol, piSymbol, gammaSymbol, naNSymbol]

{-| This symbol represents the constructor function for integers, specifying
the base. It takes two arguments, the first is a positive integer to denote
the base to which the number is represented, the second argument is a string
which contains an optional sign and the digits of the integer, using 0-9a-z
(as a consequence of this no radix greater than 35 is supported). Base 16 and
base 10 are already covered in the encodings of integers. -}
basedIntegerSymbol :: Symbol
basedIntegerSymbol = makeSymbol "nums1" "based_integer"

{-| This symbol represents the constructor function for rational numbers. It
takes two arguments, the first is an integer p to denote the numerator and the
second a nonzero integer q to denote the denominator of the rational p/q. -}
rationalSymbol :: Symbol
rationalSymbol = makeSymbol "nums1" "rational"

{-| A symbol to represent the notion of infinity. -}
infinitySymbol :: Symbol
infinitySymbol = makeSymbol "nums1" "infinity"

{-| This symbol represents the base of the natural logarithm, approximately
2.718. See Abramowitz and Stegun, Handbook of Mathematical Functions, section
4.1. -}
eSymbol :: Symbol
eSymbol = makeSymbol "nums1" "e"

{-| This symbol represents the square root of -1. -}
iSymbol :: Symbol
iSymbol = makeSymbol "nums1" "i"

{-| A symbol to convey the notion of pi, approximately 3.142. The ratio of the
circumference of a circle to its diameter. -}
piSymbol :: Symbol
piSymbol = makeSymbol "nums1" "pi"

{-| A symbol to convey the notion of the gamma constant as defined in
Abramowitz and Stegun, Handbook of Mathematical Functions, section 6.1.3. It
is the limit of 1 + 1/2 + 1/3 + ... + 1/m - ln m as m tends to infinity, this
is approximately 0.5772 15664. -}
gammaSymbol :: Symbol
gammaSymbol = makeSymbol "nums1" "gamma"

{-| A symbol to convey the notion of not-a-number. The result of an ill-posed
floating computation. See IEEE standard for floating point representations. -}
naNSymbol :: Symbol
naNSymbol = makeSymbol "nums1" "NaN"