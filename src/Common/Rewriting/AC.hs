module Common.Rewriting.AC where

import Common.Uniplate
import Common.Utils
import Data.List
import Data.Maybe

-----------------------------------------------------------
-- AC theories

type OperatorAC a = (a -> Maybe (a, a), a -> a -> a)

collectAC :: OperatorAC a -> a -> [a]
collectAC (f, _) a = rec a []
 where
   rec a = case f a of
              Just (x, y) -> rec x . rec y
              Nothing     -> (a:)

buildAC :: OperatorAC a -> [a] -> a
buildAC (_, f) = foldr1 f

testAC :: OperatorAC a -> a -> Bool
testAC (f, _) = isJust . f

findOperatorAC :: [OperatorAC a] -> a -> Maybe (OperatorAC a)
findOperatorAC acs a = safeHead $ filter (`testAC` a) acs 

normalizeACs :: (Uniplate a, Ord a) => [OperatorAC a] -> a -> a
normalizeACs acs = rec
 where
   rec a = 
      case findOperatorAC acs a of
         Just ac -> 
            buildAC ac $ sort $ map rec $ collectAC ac a
         Nothing -> 
            let (cs, f) = uniplate a
            in f (map rec cs)

normalizeAC :: (Uniplate a, Ord a) => OperatorAC a -> a -> a
normalizeAC ac = normalizeACs [ac]

equalACs :: (Uniplate a, Ord a) => [OperatorAC a] -> a -> a -> Bool
equalACs acs x y = normalizeACs acs x == normalizeACs acs y

equalAC :: (Uniplate a, Ord a) => OperatorAC a -> a -> a -> Bool
equalAC ac = equalACs [ac]