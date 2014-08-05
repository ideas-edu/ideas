{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
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

module Ideas.Common.Strategy.Tests (tests) where

import Control.Monad
import Data.Function
import Data.List
import Ideas.Common.Id
import Ideas.Common.Strategy hiding (repeat)
import Ideas.Common.Library (failS, makeRule, Rule)
import Ideas.Common.Strategy.Sequence (Firsts(..), MenuItem(..), firstsOrdered)
import Test.QuickCheck

infix 0 ===

(===) :: Strategy Int -> Strategy Int -> Property
s === t = forAll arbitrary $ \i -> 
   eqPrefix (emptyPrefix s i) (emptyPrefix t i)

eqPrefix :: Eq a => Prefix a -> Prefix a -> Bool
eqPrefix p q =
   ready p == ready q &&
   f (merge $ firstsOrdered cmp p) (merge $ firstsOrdered cmp q)
 where
   cmp :: (Step a, a) -> (Step a, a) -> Ordering
   cmp = compareId `on` fst

   f :: Eq a => [(Elem (Prefix a), Prefix a)] -> [(Elem (Prefix a), Prefix a)] -> Bool
   f (x:xs) (y:ys) = fst x == fst y && eqPrefix (snd x) (snd y) && f xs ys
   f [] [] = True
   f _ _ = False
   
   merge :: Eq a => [(Elem (Prefix a), Prefix a)] -> [(Elem (Prefix a), Prefix a)]
   merge ((a, x):(b, y):zs) | a == b = merge $ (a, x <> y) : zs
   merge (x:xs) = x:merge xs
   merge [] = []

prop1 :: Strategy Int -> Strategy Int -> Strategy Int -> Property
prop1 p q r = p <|> (q <|> r) === (p <|> q) <|> r

prop2 :: Strategy Int -> Strategy Int -> Property
prop2 p q = p <|> q === q <|> p

prop3 :: Strategy Int -> Property
prop3 p = p <|> p === p

prop4 :: Strategy Int -> Property
prop4 p = failS <|> p === p

prop5 :: Strategy Int -> Property
prop5 p = p <|> failS === p

prop6 :: Strategy Int -> Strategy Int -> Strategy Int -> Property
prop6 p q r = p <*> (q <*> r) === (p <*> q) <*> r

prop7 :: Strategy Int -> Property
prop7 p = succeed <*> p === p

prop8 :: Strategy Int -> Property
prop8 p = p <*> succeed === p

prop9 :: Strategy Int -> Property
prop9 p = failS <*> p === failS

prop10 :: Strategy Int -> Property
prop10 p = p <*> failS === failS

prop11 :: Strategy Int -> Strategy Int -> Strategy Int -> Property
prop11 p q r = p <*> (q <|> r) === (p <*> q) <|> (p <*> r)

prop12 :: Strategy Int -> Strategy Int -> Strategy Int -> Property
prop12 p q r = (p <|> q) <*> r === (p <*> r) <|> (q <*> r)

prop13 :: Strategy Int -> Strategy Int -> Strategy Int -> Property
prop13 p q r = p |> (q |> r) === (p |> q) |> r

prop14 :: Strategy Int -> Strategy Int -> Property
prop14 p q = p |> q === q |> p

prop15 :: Strategy Int -> Property
prop15 p = p |> p === p

prop16 :: Strategy Int -> Property
prop16 p = failS |> p === p

prop17 :: Strategy Int -> Property
prop17 p = p |> failS === p

prop18 :: Strategy Int -> Property
prop18 p = succeed |> p=== succeed

main = do
   cat "choice"
   runSSS "1. associative" prop1
   runSS  "2. commutative" prop2
   runS   "3. idempotent"  prop3
   runS   "4. fail left-unit" prop4
   runS   "5. fail right-unit" prop5
   cat "sequence"
   runSSS "6. associative" prop6
   runS   "7. succeed left-unit" prop7
   runS   "8. succeed right-unit" prop8
   runS   "9. fail left-zero" prop9
   runS   "10. fail right-zero" prop10
   cat "choice/sequence"
   runSSS "11. left distributive" prop11
   runSSS "12. right distributive" prop12
   cat "left-biased choice"
   runSSS "13. associative" prop13
   runSS  "14. commutative" prop14
   runS   "15. idempotent"  prop15
   runS   "16. fail left-unit" prop16
   runS   "17. fail right-unit" prop17
   runS   "18. succeed left-zero" prop18
 where 
   cat :: String -> IO ()
   cat s = putStrLn $ take 70 $ "--- " ++ s ++ " " ++ repeat '-' 
   
   run :: Testable a => String -> a -> IO ()
   run s p = do
      putStr $ take 30 $ "   " ++ s ++ repeat ' '
      quickCheck p
      
   runS   s p = run s (forAll arbS p)
   runSS  s p = run s (forAll arbS2 $ \(x, y) -> p x y)
   runSSS s p = run s (forAll arbS3 $ \(x, y, z) -> p x y z)

arbS :: Gen (Strategy Int)
arbS = arbRules >>= arbWith

arbS2 :: Gen (Strategy Int, Strategy Int)
arbS2 = arbRules >>= \rs -> 
   liftM2 (,) (arbWith rs) (arbWith rs)

arbS3 :: Gen (Strategy Int, Strategy Int, Strategy Int)
arbS3 = arbRules >>= \rs -> 
   liftM3 (,,) (arbWith rs) (arbWith rs) (arbWith rs)

data S a = S a :|: S a | S a :*: S a | Rule a | Succeed | Fail
   deriving Show

arbWith :: [Rule Int] -> Gen (Strategy Int)
arbWith rs = sized f
 where
   f n 
      | n == 0    = elements (failS : succeed : map toStrategy rs)
      | otherwise = oneof
           [ f 0
           , liftM2 (<|>) rec rec
           , liftM2 (<*>) rec rec
           , liftM2 (|>) rec rec
           ]
    where
      rec = f (n `div` 2)
      
arbRules :: Gen [Rule Int]
arbRules = do
   fs <- vector 3
   return $ zipWith (\c f -> makeRule [c] (f :: Int -> Maybe Int)) ['A' .. ] fs
      
ra, rb, rc :: Rule Int
ra = makeRule "a" Just
rb = makeRule "b" $ \i -> if even i then Just (i `div` 2) else Nothing
rc = makeRule "c" $ \i -> if i `mod` 3 == 0 then Just (i-1) else Nothing
      
tests = []
{-
import Data.Function
import Data.List
import Data.Ord
import Ideas.Common.Algebra.Group
import Ideas.Common.Algebra.GroupLaws
import Ideas.Common.Algebra.Law
import Ideas.Common.Classes
import Ideas.Common.Strategy
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Parsing hiding (firsts)
import Ideas.Common.Utils.QuickCheck hiding (label, Result)
import Ideas.Common.Utils.TestSuite
import Prelude hiding (fail)
import qualified Ideas.Common.Algebra.Field as F
import qualified Ideas.Common.Algebra.FieldLaws as F

---------------------------------------------------------
-- Properties

tests :: TestSuite
tests = suite "Strategy combinator properties"
   [ -- monoids and semi-rings
     fs (commutative : idempotent : monoidLaws :: [Law Choice])
   , fs (monoidZeroLaws :: [Law Sequence])
   , fs (commutative : monoidZeroLaws :: [Law Interleave])
   , fs (F.distributiveLaws :: [Law Sequence])
   , fs (F.distributiveLaws :: [Law Interleave])

   -- properties of atomic
   , useProperty "atomic-twice" $ \a ->
        atomic (atomic a) === atomic (idS a)
   , assertTrue  "atomic-succeed" $
        atomic succeed === succeed
   , assertTrue  "atomic-fail" $
        atomic fail === fail
   , useProperty "atomic-choice" $ \a b ->
        atomic (idS a <|> idS b) === atomic a <|> atomic b

   -- splits theorm parallel/atomic
   , useProperty "atomic-split"  $ \x y a b ->
        (atomic x <*> a) <%> (atomic y <*> b)
        ===
        (idS x <*> (a <%> (atomic y <*> b)))
          <|>
        (idS y <*> ((atomic x <*> idS a) <%> idS b))
   ]
 where
   fs :: (Arbitrary a, Show a, Eq a) => [Law a] -> TestSuite
   fs ps = mconcat [ useProperty (show p) p | p <- ps ]

---------------------------------------------------------
-- Algebraic instances

newtype Choice     = Choice     (Strategy Int) deriving (Show, Arbitrary)
newtype Sequence   = Sequence   (Strategy Int) deriving (Show, Arbitrary)
newtype Interleave = Interleave (Strategy Int) deriving (Show, Arbitrary)

instance Eq Choice     where     Choice a == Choice b     = a === b
instance Eq Sequence   where   Sequence a == Sequence b   = a === b
instance Eq Interleave where Interleave a == Interleave b = a === b

instance Monoid Choice where
   mempty = Choice fail
   mappend (Choice a) (Choice b) = Choice (a <|> b)

instance Monoid Sequence where
   mempty = Sequence succeed
   mappend (Sequence a) (Sequence b) = Sequence (a <*> b)

instance MonoidZero Sequence where
   mzero = Sequence fail

instance Monoid Interleave where
   mempty = Interleave succeed
   mappend (Interleave a) (Interleave b) = Interleave (a <%> b)

instance MonoidZero Interleave where
   mzero = Interleave fail

instance F.SemiRing Sequence where
   Sequence a <+> Sequence b = Sequence (a <|> b)
   zero  = Sequence fail
   (<*>) = mappend
   one   = mempty

instance F.SemiRing Interleave where
   Interleave a <+> Interleave b = Interleave (a <|> b)
   zero  = Interleave fail
   (<*>) = mappend
   one   = mempty

---------------------------------------------------------
-- Helper functions for equality

idS :: Strategy Int -> Strategy Int
idS = id

infix 1 ===

(===) :: Strategy Int -> Strategy Int -> Bool
s1 === s2 = rec 100 [(start s1, start s2)]
 where
   start = return . replayCore emptyPath 0 . toCore

   rec :: Int -> [([Prefix Int], [Prefix Int])] -> Bool
   rec _ [] = True
   rec n (pair:rest)
      | n == 0    = True
      | otherwise = testReady xs ys
                 && testFirsts gxs gys
                 && rec (n-1) (rest ++ new)

    where
      p@(xs, ys)    = mapBoth (concatMap myFirsts) pair
      gp@(gxs, gys) = mapBoth f p
      new           = uncurry zip (mapBoth (map snd) gp)

      testReady  = (==) `on` any (isReady . fst)
      testFirsts = (==) `on` map fst

      f          = map merge . groupBy eqFst . sortBy cmpFst . results
      merge   as = (fst (head as), map snd as)
      results as = [ (a, b) | (a, b) <- as ]

      cmpFst = comparing (show . fst)
      eqFst  = (==) `on` fst

firsts :: Bool -> Prefix a -> [(Step a, Prefix a)]
firsts = undefined -- fix me

isReady :: Step a -> Bool
isReady = undefined -- fix me

myFirsts :: Prefix a -> [(Step a, Prefix a)]
myFirsts = concatMap f . firsts False
 where
   f pair@(result, a) =
      case result of
         Enter _ -> myFirsts a
         Exit _  -> myFirsts a
         _       -> [pair]

{-
debug :: Show a => Strategy a -> a -> IO ()
debug s = rec . makeState (toCore s)
 where
   rec st = do
      print st
      putStrLn $ "\nReady: " ++ show (any (isReady . fst) xs)
      putStrLn $ unlines $
         zipWith (\i y -> show i ++ ". " ++ show (fst y)) [1::Int ..] ys
      if (null xs) then print "(no choices)" else do
      n <- ask
      rec (snd (ys !! n))
    where
      xs = firsts st
      ys = [ (a, b) | (Result a, b) <- xs ]

      ask = do
         putStr "? "
         input <- getLine
         case readInt input of
            Just n | n > 0 && n <= length ys ->
               return (n-1)
            _ -> if input == "q" then error "QUIT" else ask -} -}