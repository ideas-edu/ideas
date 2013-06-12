-- Automatically generated from content dictionary fns1.ocd.  Do not change.
module Ideas.Text.OpenMath.Dictionary.Fns1 where

import Ideas.Text.OpenMath.Symbol

-- | List of symbols defined in fns1 dictionary
fns1List :: [Symbol]
fns1List = [domainofapplicationSymbol, domainSymbol, rangeSymbol, imageSymbol, identitySymbol, leftInverseSymbol, rightInverseSymbol, inverseSymbol, leftComposeSymbol, lambdaSymbol]

{-| The domainofapplication element denotes the domain over which a given
function is being applied. It is intended in MathML to be a more general
alternative to specification of this domain using such quantifier elements as
bvar, lowlimit or condition. -}
domainofapplicationSymbol :: Symbol
domainofapplicationSymbol = makeSymbol "fns1" "domainofapplication"

{-| This symbol denotes the domain of a given function, which is the set of
values it is defined over. -}
domainSymbol :: Symbol
domainSymbol = makeSymbol "fns1" "domain"

{-| This symbol denotes the range of a function, that is a set that the
function will map to. The single argument should be the function whos range is
being queried. It should be noted that this is not necessarily equal to the
image, it is merely required to contain the image. -}
rangeSymbol :: Symbol
rangeSymbol = makeSymbol "fns1" "range"

{-| This symbol denotes the image of a given function, which is the set of
values the domain of the given function maps to. -}
imageSymbol :: Symbol
imageSymbol = makeSymbol "fns1" "image"

{-| The identity function, it takes one argument and returns the same value.
-}
identitySymbol :: Symbol
identitySymbol = makeSymbol "fns1" "identity"

{-| This symbol is used to describe the left inverse of its argument (a
function). This inverse may only be partially defined because the function may
not have been surjective. If the function is not surjective the left inverse
function is ill-defined without further stipulations. No other assumptions are
made on the semantics of this left inverse. -}
leftInverseSymbol :: Symbol
leftInverseSymbol = makeSymbol "fns1" "left_inverse"

{-| This symbol is used to describe the right inverse of its argument (a
function). This inverse may only be partially defined because the function may
not have been surjective. If the function is not surjective the right inverse
function is ill-defined without further stipulations. No other assumptions are
made on the semantics of this right inverse. -}
rightInverseSymbol :: Symbol
rightInverseSymbol = makeSymbol "fns1" "right_inverse"

{-| This symbol is used to describe the inverse of its argument (a function).
This inverse may only be partially defined because the function may not have
been surjective. If the function is not surjective the inverse function is
ill-defined without further stipulations. No assumptions are made on the
semantics of this inverse. -}
inverseSymbol :: Symbol
inverseSymbol = makeSymbol "fns1" "inverse"

{-| This symbol represents the function which forms the left-composition of
its two (function) arguments. -}
leftComposeSymbol :: Symbol
leftComposeSymbol = makeSymbol "fns1" "left_compose"

{-| This symbol is used to represent anonymous functions as lambda expansions.
It is used in a binder that takes two further arguments, the first of which is
a list of variables, and the second of which is an expression, and it forms
the function which is the lambda extraction of the expression -}
lambdaSymbol :: Symbol
lambdaSymbol = makeSymbol "fns1" "lambda"