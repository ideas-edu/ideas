-- Automatically generated from content dictionary calculus1.ocd.  Do not change.
module Ideas.Text.OpenMath.Dictionary.Calculus1 where

import Ideas.Text.OpenMath.Symbol

-- | List of symbols defined in calculus1 dictionary
calculus1List :: [Symbol]
calculus1List = [diffSymbol, nthdiffSymbol, partialdiffSymbol, intSymbol, defintSymbol]

{-| This symbol is used to express ordinary differentiation of a unary
function. The single argument is the unary function. -}
diffSymbol :: Symbol
diffSymbol = makeSymbol "calculus1" "diff"

{-| This symbol is used to express the nth-iterated ordinary differentiation
of a unary function. The first argument is n, and the second the unary
function. -}
nthdiffSymbol :: Symbol
nthdiffSymbol = makeSymbol "calculus1" "nthdiff"

{-| This symbol is used to express partial differentiation of a function of
more than one variable. It has two arguments, the first is a list of integers
which index the variables of the function, the second is the function. -}
partialdiffSymbol :: Symbol
partialdiffSymbol = makeSymbol "calculus1" "partialdiff"

{-| This symbol is used to represent indefinite integration of unary
functions. The argument is the unary function. -}
intSymbol :: Symbol
intSymbol = makeSymbol "calculus1" "int"

{-| This symbol is used to represent definite integration of unary functions.
It takes two arguments; the first being the range (e.g. a set) of integration,
and the second the function. -}
defintSymbol :: Symbol
defintSymbol = makeSymbol "calculus1" "defint"