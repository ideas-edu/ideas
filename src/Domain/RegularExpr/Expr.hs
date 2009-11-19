{-# OPTIONS -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.RegularExpr.Expr where

import Common.Rewriting
import Common.Traversable
import Common.Uniplate
import Control.Monad
import Test.QuickCheck

--------------------------------------------------------------------
-- Data type declaration

infixl 4 :|:
infixl 5 :*:

type RegExp = RE String

data RE a = EmptySet | Epsilon | Atom a | Option (RE a) | Star (RE a)
          | Plus (RE a) | RE a :*: RE a | RE a :|: RE a
   deriving (Show, Eq, Ord)

--------------------------------------------------------------------
-- Fold

foldRE (es, eps, at, opt, st, pl, sq, ch) = rec 
 where
   rec regexp = 
      case regexp of
         EmptySet -> es
         Epsilon  -> eps
         Atom a   -> at a
         Option r -> opt (rec r)
         Star r   -> st (rec r)
         Plus r   -> pl (rec r)
         r :*: s  -> sq (rec r) (rec s)
         r :|: s  -> ch (rec r) (rec s)

--------------------------------------------------------------------
-- General instances

instance Functor RE where
   fmap f = foldRE (EmptySet, Epsilon, Atom . f, Option, Star, Plus, (:*:), (:|:))

instance Crush RE where
   crush (Atom a) = [a]
   crush regexp   = concatMap crush (children regexp)

instance Arbitrary RegExp where
   arbitrary = sized (arbRE $ oneof $ map return ["a", "b", "c", "d"])
   coarbitrary = foldRE 
      (         variant 0
      ,         variant 1
      , \a ->   variant 2 . coarbitrary a
      , \a ->   variant 3 . a
      , \a ->   variant 4 . a
      , \a ->   variant 5 . a
      , \a b -> variant 6 . a . b
      , \a b -> variant 7 . a . b
      )

arbRE :: Gen a -> Int -> Gen (RE a)
arbRE g n 
   | n == 0 = frequency 
        [ (6, liftM Atom g)
        , (3, return Epsilon)
        , (1, return EmptySet)
        ]
   | otherwise = frequency 
        [ (3, arbRE g 0)
        , (2, unop Star) -- (1, unop Option), (1, unop Plus)
        , (3, binop (:*:)), (3, binop (:|:))
        ]
 where
   rec     = arbRE g (n `div` 2)
   unop  f = liftM  f rec
   binop f = liftM2 f rec rec

--------------------------------------------------------------------
-- Pretty-printer

ppRegExp :: RegExp -> String
ppRegExp = ppWith (const id)

ppWith :: (Int -> a -> String) -> RE a -> String
ppWith f = ($ 0) . foldRE 
   (const "F", const "T", flip f, unop "?", unop "*", unop "+", binop 5 "", binop 4 "|")
 where 
   unop s a _ = parIf False (a 6 ++ s)
   binop i s a b n = parIf (n > i) (a i ++ s ++ b i)
   parIf b s = if b then "(" ++ s ++ ")" else s

--testje = ppWith (const id) (Star (Plus (Atom "P")) :*: (Option (Atom "Q" :*: Option (Atom "S")) :|: Atom "R"))

--------------------------------------------------------------------
-- Function for associative operators

concatOp :: Operator (RE a)
concatOp = associativeOperator (:*:) isConcat
 where
   isConcat (r :*: s) = Just (r, s)
   isConcat _         = Nothing

choiceOp :: Operator (RE a)
choiceOp = associativeOperator (:|:) isChoice
 where
   isChoice (r :|: s) = Just (r, s)
   isChoice _         = Nothing

--------------------------------------------------------------------
-- Instances for rewriting

instance Uniplate (RE a) where
   uniplate regexp = 
      case regexp of
         EmptySet -> ([],     \[] -> EmptySet)
         Epsilon  -> ([],     \[] -> Epsilon)
         Atom a   -> ([],     \[] -> Atom a)
         Option r -> ([r],    \[a] -> Option a)
         Star r   -> ([r],    \[a] -> Star a)
         Plus r   -> ([r],    \[a] -> Plus a)
         r :*: s  -> ([r, s], \[a, b] -> a :*: b)
         r :|: s  -> ([r, s], \[a, b] -> a :|: b)

instance MetaVar a => MetaVar (RE a) where
   isMetaVar (Atom a) = isMetaVar a
   isMetaVar _        = Nothing
   metaVar            = Atom . metaVar
   
instance Eq a => ShallowEq (RE a) where
   shallowEq re1 re2 = 
      case (re1, re2) of
         (EmptySet, EmptySet) -> True
         (Epsilon,  Epsilon ) -> True
         (Atom a,   Atom b  ) -> a==b
         (Option _, Option _) -> True
         (Star _,   Star _  ) -> True
         (Plus _,   Plus _  ) -> True
         (_ :*: _,  _ :*: _ ) -> True
         (_ :|: _,  _ :|: _ ) -> True
         _                    -> False

instance Rewrite RegExp where
   operators = [concatOp, choiceOp]