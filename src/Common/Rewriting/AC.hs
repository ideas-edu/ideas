-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Common.Rewriting.AC 
   ( Operator, Operators, constructor, destructor
   , newOperator, associativeOperator, commutativeOperator, acOperator
   , makeAssociative, makeCommutative, isAssociative, isCommutative
   , collectWithOperator, buildWithOperator
   , isOperator, findOperator
   , normalizeWith, equalWith
   , pairings, pairingsMatch
   , pairingsA2, onBoth
   ) where

import Common.Uniplate
import Common.Utils
import Data.List
import Data.Maybe

-----------------------------------------------------------
-- AC theories

type Operators a = [Operator a]

data Operator a = O 
   { constructor   :: a -> a -> a
   , destructor    :: a -> Maybe (a, a)
   , isAssociative :: Bool
   , isCommutative :: Bool
   }
   
newOperator :: (a -> a -> a) ->  (a -> Maybe (a, a)) -> Operator a
newOperator f g = O f g False False

associativeOperator, commutativeOperator, acOperator :: (a -> a -> a) ->  (a -> Maybe (a, a)) -> Operator a
associativeOperator f = makeAssociative . newOperator f
commutativeOperator f = makeCommutative . newOperator f
acOperator          f = makeAssociative . commutativeOperator f

makeCommutative, makeAssociative :: Operator a -> Operator a
makeCommutative op = op { isCommutative = True }
makeAssociative op = op { isAssociative = True  }

collectWithOperator :: Operator a -> a -> [a]
collectWithOperator op a
   | isAssociative op = rec a []
   | otherwise        = maybe [a] (\(x, y) -> [x, y]) (destructor op a)
 where
   rec a = case destructor op a of
              Just (x, y) -> rec x . rec y
              Nothing     -> (a:)

buildWithOperator :: Operator a -> [a] -> a
buildWithOperator op xs 
   | null xs = 
        error "Rewriting.buildWithOperator: empty list"
   | not (isAssociative op) && length xs > 2 =
        error "Rewriting.buildWithOperator: non-associative operator"
   | otherwise = 
        foldr1 (constructor op) xs
   
isOperator :: Operator a -> a -> Bool
isOperator op = isJust . destructor op

findOperator :: Operators a -> a -> Maybe (Operator a)
findOperator ops a = safeHead $ filter (`isOperator` a) ops

normalizeWith :: (Uniplate a, Ord a) => Operators a -> a -> a
normalizeWith ops = rec
 where
   rec a = 
      case findOperator ops a of
         Just op -> 
            buildWithOperator op $ (if isCommutative op then sort else id) $ map rec $ collectWithOperator op a
         Nothing -> 
            let (cs, f) = uniplate a
            in f (map rec cs)

equalWith :: (Uniplate a, Ord a) => Operators a -> a -> a -> Bool
equalWith ops x y = normalizeWith ops x == normalizeWith ops y

-----------------------------------------------------------
-- Pairing terms with an AC theory
-- matchMode: the left-hand sides cannot have the operator at top-level 

pairings, pairingsMatch :: Operator a -> a -> a -> [[(a, a)]]
pairings      = pairingsMode False
pairingsMatch = pairingsMode True

pairingsMode :: Bool -> Operator a -> a -> a -> [[(a, a)]]
pairingsMode matchMode op =
   case (isAssociative op, isCommutative op) of
      (True , True ) -> pairingsAC matchMode op
      (True , False) -> pairingsA  matchMode op
      (False, True ) -> pairingsC op
      (False, False) -> pairingsNone op

-- non-associative, non-commutative pairings
pairingsNone :: Operator a -> a -> a -> [[(a, a)]]
pairingsNone op a b =
   case (destructor op a, destructor op b) of
      (Just (a1, a2), Just (b1, b2)) -> [[(a1, b1), (a2, b2)]]
      _ -> []
      
-- commutative pairings
pairingsC :: Operator a -> a -> a -> [[(a, a)]]
pairingsC op a b = 
   case (destructor op a, destructor op b) of
      (Just (a1, a2), Just (b1, b2)) -> [[(a1, b1), (a2, b2)], [(a1, b2), (a2, b1)]]
      _ -> []

-- associative pairings
pairingsA :: Bool -> Operator a -> a -> a -> [[(a, a)]]
pairingsA matchMode op a b = map (map make) result
 where 
   (as, bs) = onBoth (collectWithOperator op) (a, b)
   result   = pairingsA2 matchMode as bs
   make     = onBoth (buildWithOperator op)

pairingsA2 :: Bool -> [a] -> [a] -> [[([a], [a])]]
pairingsA2 matchMode = rec
 where
   rec [] [] = [[]]
   rec as bs = 
      [ (a1, b1):ps
      | i <- if matchMode && not (null as) then [1] else [1 .. length as]
      , j <- [1 .. length bs]
      , i==1 || j==1
      , let (as1, as2) = splitAt i as
      , let (bs1, bs2) = splitAt j bs
      , let a1 = as1
      , let b1 = bs1
      , ps <- rec as2 bs2
      ]

-- associative/commutative pairings
pairingsAC :: Bool -> Operator a -> a -> a -> [[(a, a)]]  
pairingsAC matchMode op a b = rec (collectWithOperator op a) (collectWithOperator op b)
 where
   rec [] [] = [[]]
   rec [] _  = []
   rec (a:as) bs = 
      [ (a1, b1):ps
      | (asr, as2) <- if matchMode then [([], as)] else splits as
      , let as1 = a:asr
      , (bs1, bs2) <- splits bs
      , not (null bs1)
      , length as1==1 || length bs1==1
      , let a1 = buildWithOperator op as1
      , let b1 = buildWithOperator op bs1
      , ps <- rec as2 bs2
      ]

{-
data Tree = Leaf String | Bin Tree Tree deriving (Show, Eq, Ord)

opBin :: Operator Tree
opBin = Operator isBin Bin
 where
   isBin (Bin a b) = Just (a, b)
   isBin _ = Nothing
   
tree1 = Bin (Bin (Leaf "1") (Leaf "2")) (Bin (Leaf "3") (Leaf "4")) -- Bin (Bin (Leaf "a") (Leaf "b")) (Bin (Leaf "c") (Leaf "d"))
tree2 = Bin (Bin (Leaf "a") (Leaf "b")) (Bin (Leaf "c") (Leaf "d")) --Bin (Bin (Leaf "w") (Leaf "x")) (Bin (Leaf "y") (Leaf "z"))

ex1 = pairingsC opBin tree1 tree2
ex2 = pairingsA  False opBin tree1 tree2
ex3 = pairingsA  True  opBin tree1 tree2
ex4 = pairingsAC False opBin tree1 tree2
ex5 = pairingsAC True opBin tree1 tree2 -}

splits :: [a] -> [([a], [a])]
splits = foldr insert [([], [])]
 where
   insert a ps = 
      let toLeft  (xs, ys) = (a:xs,   ys)
          toRight (xs, ys) = (  xs, a:ys)
      in map toLeft ps ++ map toRight ps

onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth f (x, y) = (f x, f y)

{-
permutations :: [a] -> [[a]]
permutations = foldr (concatMap . insert) [[]]
 where
   insert a []     = [[a]]
   insert a (x:xs) = (a:x:xs) : map (x:) (insert a xs)
-}