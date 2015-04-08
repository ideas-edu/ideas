{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Testing strategy combinator properties
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Tests (main) where

import Control.Monad
import Data.Function
import Ideas.Common.Library hiding (ready)
import Ideas.Common.Strategy.Sequence (Firsts(..), firstsOrdered)
import Ideas.Common.Strategy.Core (Core, GCore(..))
import Ideas.Common.Strategy.Abstract (toCore, fromCore)
import Ideas.Common.Utils.Uniplate
import Test.QuickCheck

-- s |> t = s <|> (notS s <*> t)

(~>) :: IsStrategy f => Rule a -> f a -> Strategy a
a ~> s = toStrategy a <*> s

infix 0 ===

(===) :: Strategy Int -> Strategy Int -> Property
s === t = forAll arbitrary $ \i -> 
   eqPrefix (emptyPrefix s i) (emptyPrefix t i)

eqPrefix :: Eq a => Prefix a -> Prefix a -> Bool
eqPrefix a b = rec 1000 [] [(majorPrefix a, majorPrefix b)]
 where
   rec :: Eq a => Int -> [(Prefix a, Prefix a)] ->  [(Prefix a, Prefix a)] -> Bool
   rec 0 _   _  = True
   rec _ []  [] = True
   rec n acc [] = rec n [] (reverse acc)
   rec n acc ((p, q):rest) = 
      let (xs1, xs2) = unzip $ merge $ firstsOrdered cmp p
          (ys1, ys2) = unzip $ merge $ firstsOrdered cmp q
      in ready (majorPrefix p) == ready (majorPrefix q) 
      && xs1 == ys1
      && rec (n-1) (zip xs2 ys2 ++ acc) rest

   cmp :: (Step a, a) -> (Step a, a) -> Ordering
   cmp = compareId `on` fst
      
   merge :: Eq a => [((Step a, a), Prefix a)] -> [((Step a, a), Prefix a)]
   merge ((p, x):(q, y):zs) | p == q = merge $ (p, x <> y) : zs
   merge (x:xs) = x:merge xs
   merge [] = []

main :: IO ()
main = mapM_ runLaw
   [ associative (<|>)
   , commutative (<|>) 
   , idempotent  (<|>)
   , leftUnit failS  (<|>)
   , rightUnit failS (<|>)
   , useGen arbRule $ \a -> law2 "merge" $ \x y -> ((a ~> x) <|> (a ~> y), a ~> (x <|> y)) 
   , leftUnit succeed (<*>)
   , rightUnit succeed (<*>)
   , leftZero failS (<*>)
   , rightDistributive (<*>) (<|>)
   , useGen arbRule $ \a -> law2 "prefix" $ \x y -> ((a ~> x) <*> y, a ~> (x <*> y)) 
   , associative (<*>)
   -- , rightZero failS (<*>)
   , leftDistributive (<*>) (<|>)
   
   -----------------------------------------------
   , associative (|>)
   , idempotent  (|>)
   , leftUnit failS  (|>)
   , rightUnit failS (|>)
   , leftZero succeed (|>)
   
   , rightDistributive (<*>) (|>) -- ???
   
   {-
   
   , leftDistributive (<*>) (|>) -- ???
   
   , leftDistributive (|>) (<|>)
   , law2 "abs1" $ \x y -> ((x <|> y) |> x, x <|> y) 
   , law2 "abs2" $ \x y -> (x |> (x <|> y), x |> y) 
   , law2 "abs3" $ \x y -> ((x |> y) <|> x, x |> y) 
   , law2 "abs4" $ \x y -> (x <|> (x |> y), x |> y) 
   -}
   ]

-----------------

forAllShrink2 :: (Show a, Testable prop) => Gen a -> (a -> [a]) -> (a -> a -> prop) -> Property
forAllShrink2 gen sh f = forAllShrink gen2 sh2 f2
 where
  gen2 = liftM2 (,) gen gen
  sh2 (x, y) = [ (a, y) | a <- sh x ] ++ [ (x, a) | a <- sh y ]
  f2  (x, y) = f x y


forAllShrink3 :: (Show a, Testable prop) => Gen a -> (a -> [a]) -> (a -> a -> a -> prop) -> Property
forAllShrink3 gen sh f = forAllShrink gen3 sh3 f3
 where
  gen3 = liftM3 (,,) gen gen gen
  sh3 (x, y, z) = [ (a, y, z) | a <- sh x ] ++ 
                  [ (x, a, z) | a <- sh y ] ++
                  [ (x, y, a) | a <- sh z ]
  f3  (x, y, z) = f x y z

runLaw :: Law (Strategy Int) -> IO ()
runLaw (Law s f) = do
   putStr $ take 30 $ "  "  ++ s ++ repeat ' '
   quickCheck $ f arbitrary shrink (===) 

data Law a = Law 
   { lawName :: String
   , lawProp ::  Gen a -> (a -> [a]) -> (a -> a -> Property) -> Property
   }

useGen :: Show a => Gen a -> (a -> Law b) -> Law b
useGen ga f = Law (lawName $ f undefined) $ \gen sh eq -> 
   forAll ga $ \a -> lawProp (f a) gen sh eq

law :: Show a => String -> (a -> (a, a)) -> Law a
law s f = Law s $ \gen sh eq -> forAllShrink gen sh $ \x ->
   uncurry eq (f x)

law2 :: Show a => String -> (a -> a -> (a, a)) -> Law a
law2 s f = Law s $ \gen sh eq -> forAllShrink2 gen sh $ \x y -> uncurry eq (f x y)
   
law3 :: Show a => String -> (a -> a -> a -> (a, a)) -> Law a
law3 s f = Law s $ \gen sh eq -> forAllShrink3 gen sh $ \x y z -> 
   uncurry eq (f x y z)

associative :: Show a => (a -> a -> a) -> Law a
associative op = law3 "associative" $ \x y z -> (op x (op y z),  op (op x y) z)

commutative :: Show a => (a -> a -> a) -> Law a
commutative op = law2 "commutative" $ \x y -> (op x y, op y x)

idempotent :: Show a => (a -> a -> a) -> Law a
idempotent op = law "idempotent" $ \x -> (op x x, x)

leftUnit :: Show a => a -> (a -> a -> a) -> Law a
leftUnit e op = law (show e ++ " left-unit") $ \x -> (op e x, x)

rightUnit :: Show a => a -> (a -> a -> a) -> Law a
rightUnit e op = law (show e ++ " right-unit") $ \x -> (op x e, x)

leftZero :: Show a => a -> (a -> a -> a) -> Law a
leftZero z op = law (show z ++ " left-zero") $ \x -> (op z x, z)

rightZero :: Show a => a -> (a -> a -> a) -> Law a
rightZero z op = law (show z ++ " right-zero") $ \x -> (op x z, z)

leftDistributive :: Show a => (a -> a -> a) -> (a -> a -> a) -> Law a
leftDistributive f op = law3 "left distributive" $ \x y z -> 
   (f x (op y z), op (f x y) (f x z))

rightDistributive :: Show a => (a -> a -> a) -> (a -> a -> a) -> Law a
rightDistributive f op = law3 "right distributive" $ \x y z -> 
   (f (op x y) z, op (f x z) (f y z))

-------------------

instance Arbitrary (Strategy Int) where
   arbitrary = arbRules >>= arbWith
   shrink = map fromCore . shrinkCore . toCore

shrinkCore :: Core a -> [Core a]
shrinkCore Fail    = []
shrinkCore Succeed = [Fail]
shrinkCore core = do
   (a, f) <- holes core
   Fail : Succeed : a : map f (shrinkCore a)
      
arbWith :: [Rule Int] -> Gen (Strategy Int)
arbWith rs = sized f
 where
   f n 
      | n == 0    = elements (failS : succeed : map toStrategy rs)
      | otherwise = oneof
           [ f 0
           , liftM2 (<|>) rec rec
           , liftM2 (<*>) rec rec
           -- , liftM2 (|>) rec rec
           , liftM2 (~>) (elements rs) rec
           ]
    where
      rec = f (n `div` 2)
      
arbRule :: Gen (Rule Int)
arbRule = elements [ra, rb, rc]
      
arbRules :: Gen [Rule Int]
arbRules = return [ra, rb, rc] {- do
   fs <- vector 3
   return $ zipWith (\c f -> makeRule [c] (f :: Int -> Maybe Int)) ['A' .. ] fs
      -}
ra, rb, rc :: Rule Int
ra = makeRule "a" Just
rb = makeRule "b" $ \i -> if even i then Just (i `div` 2) else Nothing
rc = makeRule "c" $ \i -> if i `mod` 3 == 0 then Just (i-1) else Nothing