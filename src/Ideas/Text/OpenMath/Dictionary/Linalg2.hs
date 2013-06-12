-- Automatically generated from content dictionary linalg2.ocd.  Do not change.
module Ideas.Text.OpenMath.Dictionary.Linalg2 where

import Ideas.Text.OpenMath.Symbol

-- | List of symbols defined in linalg2 dictionary
linalg2List :: [Symbol]
linalg2List = [vectorSymbol, matrixrowSymbol, matrixSymbol]

{-| This symbol represents an n-ary function used to construct (or describe)
vectors. Vectors in this CD are considered to be row vectors and must
therefore be transposed to be considered as column vectors. -}
vectorSymbol :: Symbol
vectorSymbol = makeSymbol "linalg2" "vector"

{-| This symbol is an n-ary constructor used to represent rows of matrices.
Its arguments should be members of a ring. -}
matrixrowSymbol :: Symbol
matrixrowSymbol = makeSymbol "linalg2" "matrixrow"

{-| This symbol is an n-ary matrix constructor which requires matrixrow's as
arguments. It is used to represent matrices. -}
matrixSymbol :: Symbol
matrixSymbol = makeSymbol "linalg2" "matrix"