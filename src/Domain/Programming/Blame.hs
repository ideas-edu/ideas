module Domain.Programming.Blame 
    ( module Domain.Programming.Loc
    , Locs, blame, makeloc, (+>)
    ) where

import Domain.Programming.Loc

interleave sep [] = []
interleave sep [a] = [a]
interleave sep (a1 : a2 : as) = a1 : sep : interleave sep (a2 : as)

cause       ::  [Loc] -> String
cause []    =   ""
cause locs  =   " (the violation was caused by the expression(s) " 
            ++  concat (interleave ", " (map show locs)) ++ ")"

data Locs = NegPos { neg :: [Loc], pos :: [Loc] }

blame       ::  Locs -> String
blame locs  =   "the expression " ++ show (head (pos locs)) ++ " is to blame" 
            ++  (  case tail (pos locs) of
                   []     ->  "."
                   locs'  ->  " (the violation was caused by the expression(s) " ++
                              concat (interleave ", " (map show locs')) ++ ")." )

makeloc      ::  Loc -> Locs
makeloc loc  =   NegPos [] [loc]

(+>) :: Locs -> Locs -> Locs
NegPos ns ps +> NegPos ns' ps'  =   NegPos (ps ++ ns') (ns ++ ps')
