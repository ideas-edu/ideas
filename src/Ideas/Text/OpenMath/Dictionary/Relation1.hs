-- Automatically generated from content dictionary relation1.ocd.  Do not change.
module Ideas.Text.OpenMath.Dictionary.Relation1 where

import Ideas.Text.OpenMath.Symbol

-- | List of symbols defined in relation1 dictionary
relation1List :: [Symbol]
relation1List = [eqSymbol, ltSymbol, gtSymbol, neqSymbol, leqSymbol, geqSymbol, approxSymbol]

{-| This symbol represents the binary equality function. -}
eqSymbol :: Symbol
eqSymbol = makeSymbol "relation1" "eq"

{-| This symbol represents the binary less than function which returns true if
the first argument is less than the second, it returns false otherwise. -}
ltSymbol :: Symbol
ltSymbol = makeSymbol "relation1" "lt"

{-| This symbol represents the binary greater than function which returns true
if the first argument is greater than the second, it returns false otherwise.
-}
gtSymbol :: Symbol
gtSymbol = makeSymbol "relation1" "gt"

{-| This symbol represents the binary inequality function. -}
neqSymbol :: Symbol
neqSymbol = makeSymbol "relation1" "neq"

{-| This symbol represents the binary less than or equal to function which
returns true if the first argument is less than or equal to the second, it
returns false otherwise. -}
leqSymbol :: Symbol
leqSymbol = makeSymbol "relation1" "leq"

{-| This symbol represents the binary greater than or equal to function which
returns true if the first argument is greater than or equal to the second, it
returns false otherwise. -}
geqSymbol :: Symbol
geqSymbol = makeSymbol "relation1" "geq"

{-| This symbol is used to denote the approximate equality of its two
arguments. -}
approxSymbol :: Symbol
approxSymbol = makeSymbol "relation1" "approx"