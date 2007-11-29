{---------------------------------------------------------------
 Copyright (c)        2006 - 2007 
 Johan Jeuring and Harrie Passier
----------------------------------------------------------------}
module Logic.Solver.LogicIndicator where

import Logic

indicators :: Logic -> (Int, Int, Int)
indicators p = 
   (countImplications p, countEquivalences p, countDoubleNegations p)

-- Count the number of implicationsations :: Logic -> Int
countImplications = foldLogic (const 0, \x y -> x+y+1, (+), (+), (+), id, 0, 0)
 
-- Count the number of equivalences
countEquivalences :: Logic -> Int
countEquivalences = foldLogic (const 0, (+), \x y -> x+y+1, (+), (+), id, 0, 0)

-- Count the number of binary operators
countBinaryOperators :: Logic -> Int
countBinaryOperators = foldLogic (const 0, binop, binop, binop, binop, id, 0, 0)
 where binop x y = x + y + 1

-- Count the number of double negations 
countDoubleNegations :: Logic -> Int
countDoubleNegations = fst . foldLogic (const zero, bin, bin, bin, bin, notf, zero, zero)
 where
   zero = (0, False)
   bin (n, _) (m, _) = (n+m, False)
   notf (n, b) = if b then (n+1, False) else (n, True)