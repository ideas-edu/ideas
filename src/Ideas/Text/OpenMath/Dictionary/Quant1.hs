-- Automatically generated from content dictionary quant1.ocd.  Do not change.
module Ideas.Text.OpenMath.Dictionary.Quant1 where

import Ideas.Text.OpenMath.Symbol

-- | List of symbols defined in quant1 dictionary
quant1List :: [Symbol]
quant1List = [forallSymbol, existsSymbol]

{-| This symbol represents the universal ("for all") quantifier which takes
two arguments. It must be placed within an OMBIND element. The first argument
is the bound variables (placed within an OMBVAR element), and the second is an
expression. -}
forallSymbol :: Symbol
forallSymbol = makeSymbol "quant1" "forall"

{-| This symbol represents the existential ("there exists") quantifier which
takes two arguments. It must be placed within an OMBIND element. The first
argument is the bound variables (placed within an OMBVAR element), and the
second is an expression. -}
existsSymbol :: Symbol
existsSymbol = makeSymbol "quant1" "exists"