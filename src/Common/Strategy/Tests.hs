{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
module Common.Strategy.Tests (tests) where

import Common.Algebra.Group
import Common.Algebra.Law
import Common.Classes
import Common.Id
import Common.Strategy
import Common.Strategy.Abstract
import Common.Strategy.Parsing
import Common.Utils.QuickCheck hiding (label, Result)
import Common.Utils.TestSuite
import Data.Function
import Data.List
import Data.Ord
import Prelude hiding (fail)
import qualified Common.Algebra.Field as F

---------------------------------------------------------
-- Properties

tests :: TestSuite
tests = suite "Strategy combinator properties" $ do
   -- monoids and semi-rings
   fs (commutative : idempotent : monoidLaws :: [Law Choice])
   fs (monoidZeroLaws :: [Law Sequence])
   fs (commutative : monoidZeroLaws :: [Law Interleave])
   fs (F.distributiveLaws :: [Law Sequence])
   fs (F.distributiveLaws :: [Law Interleave])

   -- properties of atomic
   addProperty "atomic-twice" $ \a ->
      atomic (atomic a) === atomic (idS a)
   assertTrue  "atomic-succeed" $
      atomic succeed === succeed
   assertTrue  "atomic-fail" $
      atomic fail === fail
   addProperty "atomic-choice" $ \a b ->
      atomic (idS a <|> idS b) === atomic a <|> atomic b

   -- splits theorm parallel/atomic
   addProperty "atomic-split"  $ \x y a b ->
      (atomic x <*> a) <%> (atomic y <*> b)
      ===
      (idS x <*> (a <%> (atomic y <*> b)))
        <|>
      (idS y <*> ((atomic x <*> idS a) <%> idS b))
 where
   fs :: (Arbitrary a, Show a, Eq a) => [Law a] -> TestSuite
   fs = mapM_ (\p -> addProperty (show p) p)

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
   start = return . flip makeState 0 . toCore

   rec :: Int -> [([State LabelInfo Int], [State LabelInfo Int])] -> Bool
   rec _ [] = True
   rec n (pair:rest)
      | n == 0    = True
      | otherwise = testReady xs ys
                 && testValue xs ys
                 && testFirsts gxs gys
                 && rec (n-1) (rest ++ new)

    where
      p@(xs, ys)    = mapBoth (concatMap myFirsts) pair
      gp@(gxs, gys) = mapBoth f p
      new           = uncurry zip (mapBoth (map snd) gp)

      testReady  = (==) `on` any (isReady . fst)
      testValue  = (==) `on` (nub . sort . map (value . snd))
      testFirsts = (==) `on` map fst

      f          = map merge . groupBy eqFst . sortBy cmpFst . results
      merge   as = (fst (head as), map snd as)
      results as = [ (a, b) | (Result a, b) <- as ]

      cmpFst = comparing (show . fst)
      eqFst  = (==) `on` fst

myFirsts :: State l a -> [(Result (Step l a), State l a)]
myFirsts = concatMap f . firsts
 where
   f pair@(result, a) =
      case result of
         Result (Enter _) -> myFirsts a
         Result (Exit _)  -> myFirsts a
         _                -> [pair]

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
            _ -> if input == "q" then error "QUIT" else ask -}