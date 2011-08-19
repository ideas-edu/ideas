{-# LANGUAGE TypeSynonymInstances #-}
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
module Domain.RegularExpr.Expr where

import Common.Rewriting
import Common.Utils.Uniplate
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

foldRE :: (b, b, a -> b, b -> b, b -> b, b -> b, b -> b -> b, b -> b -> b) -> RE a -> b
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

instance Arbitrary RegExp where
   arbitrary = sized $ arbRE $ elements ["a", "b", "c", "d"]

instance CoArbitrary RegExp where
   coarbitrary = foldRE 
      (         variant (0 :: Int)
      ,         variant (1 :: Int)
      , \a ->   variant (2 :: Int). coarbitrary a
      , \a ->   variant (3 :: Int) . a
      , \a ->   variant (4 :: Int) . a
      , \a ->   variant (5 :: Int) . a
      , \a b -> variant (6 :: Int) . a . b
      , \a b -> variant (7 :: Int) . a . b
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
-- Instances for rewriting

instance Uniplate (RE a) where
   uniplate regexp = 
      case regexp of
         Option r -> plate Option |* r
         Star r   -> plate Star   |* r
         Plus r   -> plate Plus   |* r
         r :*: s  -> plate (:*:)  |* r |* s
         r :|: s  -> plate (:|:)  |* r |* s
         _        -> plate regexp

instance Different (RE a) where
   different = (EmptySet, Epsilon)

instance IsTerm RegExp where 
   toTerm = foldRE 
      ( symbol emptySetSymbol, symbol epsilonSymbol, variable
      , unary optionSymbol, unary starSymbol, unary plusSymbol
      , binary sequenceSymbol, binary choiceSymbol
      ) 

   fromTerm a = fromTermWith f a `mplus` liftM Atom (getVariable a)
    where
      f s []     
         | s == emptySetSymbol = return EmptySet
         | s == epsilonSymbol  = return Epsilon
      f s [x]    
         | s == optionSymbol   = return (Option x)
         | s == starSymbol     = return (Star x)
         | s == plusSymbol     = return (Plus x)
      f s [x, y] 
         | s == sequenceSymbol = return (x :*: y)
         | s == choiceSymbol   = return (x :|: y)
      f _ _ = fail "fromExpr"

emptySetSymbol, epsilonSymbol, optionSymbol, starSymbol,
   plusSymbol, sequenceSymbol, choiceSymbol :: Symbol
emptySetSymbol = regexpSymbol "EmptySet"
epsilonSymbol  = regexpSymbol "Epsilon"
optionSymbol   = regexpSymbol "Option"
starSymbol     = regexpSymbol "Star"
plusSymbol     = regexpSymbol "Plus"
sequenceSymbol = makeAssociative $ regexpSymbol "Sequence"
choiceSymbol   = makeAssociative $ regexpSymbol "Choice"

regexpSymbol :: String -> Symbol
regexpSymbol a = newSymbol ["regexp", a]