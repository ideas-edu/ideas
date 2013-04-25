-- Automatically generated from content dictionary transc1.ocd.  Do not change.
module Ideas.Text.OpenMath.Dictionary.Transc1 where

import Ideas.Text.OpenMath.Symbol

-- | List of symbols defined in transc1 dictionary
transc1List :: [Symbol]
transc1List = [logSymbol, lnSymbol, expSymbol, sinSymbol, cosSymbol, tanSymbol, secSymbol, cscSymbol, cotSymbol, sinhSymbol, coshSymbol, tanhSymbol, sechSymbol, cschSymbol, cothSymbol, arcsinSymbol, arccosSymbol, arctanSymbol, arcsecSymbol, arccscSymbol, arccotSymbol, arcsinhSymbol, arccoshSymbol, arctanhSymbol, arcsechSymbol, arccschSymbol, arccothSymbol]

{-| This symbol represents a binary log function; the first argument is the
base, to which the second argument is log'ed. It is defined in Abramowitz and
Stegun, Handbook of Mathematical Functions, section 4.1 -}
logSymbol :: Symbol
logSymbol = makeSymbol "transc1" "log"

{-| This symbol represents the ln function (natural logarithm) as described in
Abramowitz and Stegun, section 4.1. It takes one argument. Note the
description in the CMP/FMP of the branch cut. If signed zeros are in use, the
inequality needs to be non-strict. -}
lnSymbol :: Symbol
lnSymbol = makeSymbol "transc1" "ln"

{-| This symbol represents the exponentiation function as described in
Abramowitz and Stegun, section 4.2. It takes one argument. -}
expSymbol :: Symbol
expSymbol = makeSymbol "transc1" "exp"

{-| This symbol represents the sin function as described in Abramowitz and
Stegun, section 4.3. It takes one argument. -}
sinSymbol :: Symbol
sinSymbol = makeSymbol "transc1" "sin"

{-| This symbol represents the cos function as described in Abramowitz and
Stegun, section 4.3. It takes one argument. -}
cosSymbol :: Symbol
cosSymbol = makeSymbol "transc1" "cos"

{-| This symbol represents the tan function as described in Abramowitz and
Stegun, section 4.3. It takes one argument. -}
tanSymbol :: Symbol
tanSymbol = makeSymbol "transc1" "tan"

{-| This symbol represents the sec function as described in Abramowitz and
Stegun, section 4.3. It takes one argument. -}
secSymbol :: Symbol
secSymbol = makeSymbol "transc1" "sec"

{-| This symbol represents the csc function as described in Abramowitz and
Stegun, section 4.3. It takes one argument. -}
cscSymbol :: Symbol
cscSymbol = makeSymbol "transc1" "csc"

{-| This symbol represents the cot function as described in Abramowitz and
Stegun, section 4.3. It takes one argument. -}
cotSymbol :: Symbol
cotSymbol = makeSymbol "transc1" "cot"

{-| This symbol represents the sinh function as described in Abramowitz and
Stegun, section 4.5. It takes one argument. -}
sinhSymbol :: Symbol
sinhSymbol = makeSymbol "transc1" "sinh"

{-| This symbol represents the cosh function as described in Abramowitz and
Stegun, section 4.5. It takes one argument. -}
coshSymbol :: Symbol
coshSymbol = makeSymbol "transc1" "cosh"

{-| This symbol represents the tanh function as described in Abramowitz and
Stegun, section 4.5. It takes one argument. -}
tanhSymbol :: Symbol
tanhSymbol = makeSymbol "transc1" "tanh"

{-| This symbol represents the sech function as described in Abramowitz and
Stegun, section 4.5. It takes one argument. -}
sechSymbol :: Symbol
sechSymbol = makeSymbol "transc1" "sech"

{-| This symbol represents the csch function as described in Abramowitz and
Stegun, section 4.5. It takes one argument. -}
cschSymbol :: Symbol
cschSymbol = makeSymbol "transc1" "csch"

{-| This symbol represents the coth function as described in Abramowitz and
Stegun, section 4.5. It takes one argument. -}
cothSymbol :: Symbol
cothSymbol = makeSymbol "transc1" "coth"

{-| This symbol represents the arcsin function. This is the inverse of the sin
function as described in Abramowitz and Stegun, section 4.4. It takes one
argument. -}
arcsinSymbol :: Symbol
arcsinSymbol = makeSymbol "transc1" "arcsin"

{-| This symbol represents the arccos function. This is the inverse of the cos
function as described in Abramowitz and Stegun, section 4.4. It takes one
argument. -}
arccosSymbol :: Symbol
arccosSymbol = makeSymbol "transc1" "arccos"

{-| This symbol represents the arctan function. This is the inverse of the tan
function as described in Abramowitz and Stegun, section 4.4. It takes one
argument. -}
arctanSymbol :: Symbol
arctanSymbol = makeSymbol "transc1" "arctan"

{-| This symbol represents the arcsec function as described in Abramowitz and
Stegun, section 4.4. -}
arcsecSymbol :: Symbol
arcsecSymbol = makeSymbol "transc1" "arcsec"

{-| This symbol represents the arccsc function as described in Abramowitz and
Stegun, section 4.4. -}
arccscSymbol :: Symbol
arccscSymbol = makeSymbol "transc1" "arccsc"

{-| This symbol represents the arccot function as described in Abramowitz and
Stegun, section 4.4. -}
arccotSymbol :: Symbol
arccotSymbol = makeSymbol "transc1" "arccot"

{-| This symbol represents the arcsinh function as described in Abramowitz and
Stegun, section 4.6. -}
arcsinhSymbol :: Symbol
arcsinhSymbol = makeSymbol "transc1" "arcsinh"

{-| This symbol represents the arccosh function as described in Abramowitz and
Stegun, section 4.6. -}
arccoshSymbol :: Symbol
arccoshSymbol = makeSymbol "transc1" "arccosh"

{-| This symbol represents the arctanh function as described in Abramowitz and
Stegun, section 4.6. -}
arctanhSymbol :: Symbol
arctanhSymbol = makeSymbol "transc1" "arctanh"

{-| This symbol represents the arcsech function as described in Abramowitz and
Stegun, section 4.6. -}
arcsechSymbol :: Symbol
arcsechSymbol = makeSymbol "transc1" "arcsech"

{-| This symbol represents the arccsch function as described in Abramowitz and
Stegun, section 4.6. -}
arccschSymbol :: Symbol
arccschSymbol = makeSymbol "transc1" "arccsch"

{-| This symbol represents the arccoth function as described in Abramowitz and
Stegun, section 4.6. -}
arccothSymbol :: Symbol
arccothSymbol = makeSymbol "transc1" "arccoth"