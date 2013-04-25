-- Automatically generated from content dictionary list1.ocd.  Do not change.
module Ideas.Text.OpenMath.Dictionary.List1 where

import Ideas.Text.OpenMath.Symbol

-- | List of symbols defined in list1 dictionary
list1List :: [Symbol]
list1List = [mapSymbol, suchthatSymbol, listSymbol]

{-| This symbol represents a mapping function which may be used to construct
lists, it takes as arguments a function from X to Y and a list over X in that
order. The value that is returned is a list of values in Y. The argument list
may be a set or an integer_interval. -}
mapSymbol :: Symbol
mapSymbol = makeSymbol "list1" "map"

{-| This symbol represents the suchthat function which may be used to
construct lists, it takes two arguments. The first argument should be the set
which contains the elements of the list, the second argument should be a
predicate, that is a function from the set to the booleans which describes if
an element is to be in the list returned. -}
suchthatSymbol :: Symbol
suchthatSymbol = makeSymbol "list1" "suchthat"

{-| This symbol denotes the list construct which is an n-ary function. The
list entries must be given explicitly. -}
listSymbol :: Symbol
listSymbol = makeSymbol "list1" "list"