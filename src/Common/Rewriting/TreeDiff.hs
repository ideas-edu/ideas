module Common.Rewriting.TreeDiff 
   ( treeDiff, TreeDiff(..)
   ) where

import Common.Rewriting.Unification (ShallowEq(..))
import Common.Uniplate

type Loc = [Int]

data TreeDiff = Equal | Top | Different | Inside deriving (Show, Eq, Ord)

-- Returns locations for the second term
treeDiff :: (Uniplate a, ShallowEq a) => a -> a -> [(Loc, TreeDiff)]
treeDiff x y = fst (rec x y)
 where
   rec x y
      | not b     = ([(l, if null l then Different else Inside) | l <- allLocations y], False)
      | and bs    = ([(l, Equal) | l <- allLocations y] , True)
      | otherwise = (([], Top):concat lists, False)
    where
      xs = children x
      ys = children y
      b  = shallowEq x y && length xs == length ys
      (lists, bs) = unzip (zipWith3 f [0..] xs ys)
      f i x y = 
         let (zs, b) = rec x y
         in (map (\(loc,td) -> (i:loc, td)) zs, b)
                
      
allLocations :: Uniplate a => a -> [Loc]
allLocations = ([]:) . concat . zipWith f [0..] . children
 where 
   f i = map (i:) . allLocations