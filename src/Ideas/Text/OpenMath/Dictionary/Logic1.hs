-- Automatically generated from content dictionary logic1.ocd.  Do not change.
module Ideas.Text.OpenMath.Dictionary.Logic1 where

import Ideas.Text.OpenMath.Symbol

-- | List of symbols defined in logic1 dictionary
logic1List :: [Symbol]
logic1List = [equivalentSymbol, notSymbol, andSymbol, xorSymbol, orSymbol, impliesSymbol, trueSymbol, falseSymbol]

{-| This symbol is used to show that two boolean expressions are logically
equivalent, that is have the same boolean value for any inputs. -}
equivalentSymbol :: Symbol
equivalentSymbol = makeSymbol "logic1" "equivalent"

{-| This symbol represents the logical not function which takes one boolean
argument, and returns the opposite boolean value. -}
notSymbol :: Symbol
notSymbol = makeSymbol "logic1" "not"

{-| This symbol represents the logical and function which is an n-ary function
taking boolean arguments and returning a boolean value. It is true if all
arguments are true or false otherwise. -}
andSymbol :: Symbol
andSymbol = makeSymbol "logic1" "and"

{-| This symbol represents the logical xor function which is an n-ary function
taking boolean arguments and returning a boolean value. It is true if there
are an odd number of true arguments or false otherwise. -}
xorSymbol :: Symbol
xorSymbol = makeSymbol "logic1" "xor"

{-| This symbol represents the logical or function which is an n-ary function
taking boolean arguments and returning a boolean value. It is true if any of
the arguments are true or false otherwise. -}
orSymbol :: Symbol
orSymbol = makeSymbol "logic1" "or"

{-| This symbol represents the logical implies function which takes two
boolean expressions as arguments. It evaluates to false if the first argument
is true and the second argument is false, otherwise it evaluates to true. -}
impliesSymbol :: Symbol
impliesSymbol = makeSymbol "logic1" "implies"

{-| This symbol represents the boolean value true. -}
trueSymbol :: Symbol
trueSymbol = makeSymbol "logic1" "true"

{-| This symbol represents the boolean value false. -}
falseSymbol :: Symbol
falseSymbol = makeSymbol "logic1" "false"