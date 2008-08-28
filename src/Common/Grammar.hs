-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- This module defines a set of combinators for context-free grammars. These
-- grammars are the basis of the strategies. The fix-point combinator 'fix' 
-- makes it context-free.
--
-----------------------------------------------------------------------------
module Common.Grammar
   ( -- * Abstract data type
     Grammar
     -- * Smart constructor functions
   , (<*>), (<|>), fix, star, succeed, fail, symbol
     -- * Grammar operations
   , acceptsEmpty, nonEmpty, firsts, member, language
   , collectSymbols, join, withIndex
     -- * QuickCheck properties
   , checks
   ) where

import Prelude hiding (fail)
import Common.Utils ()
import Control.Monad (liftM, liftM2)
import Control.Arrow (second)
import Data.List (nub)
import Test.QuickCheck

-----------------------------------------------------------
-- Abstract data type

data Grammar a
   = Grammar a :*: Grammar a
   | Grammar a :|: Grammar a
   | Fix Int (Grammar a)
   | Var Int
   | Symbol a
   | Succeed 
   | Fail
 deriving Show

instance Functor Grammar where
   fmap f grammar =
      case grammar of
         p :*: q  -> fmap f p :*: fmap f q
         p :|: q  -> fmap f p :|: fmap f q
         Fix i p  -> Fix i (fmap f p) 
         Var i    -> Var i
         Symbol a -> Symbol (f a)
         Succeed  -> Succeed
         Fail     -> Fail

instance Monad Grammar where
   return  = symbol
   m >>= f = join (fmap f m) 

-----------------------------------------------------------
-- Smart constructor functions

infixr 5 <*>
infixr 4 <|>

-- | Smart constructor for sequences: removes fails and succeeds in the
-- operands
(<*>) :: Grammar a -> Grammar a -> Grammar a
p <*> q = 
   case (p, q) of
      (Fail, _)    -> Fail
      (_, Fail)    -> Fail
      (Succeed, _) -> q
      (_, Succeed) -> p
      _            -> p :*: q

-- | Smart constructor for alternatives: removes fails in the operands, and 
-- merges succeeds if present in both arguments
(<|>) :: Grammar a -> Grammar a -> Grammar a
p <|> q = 
   case (p, q) of
      (Fail, _) -> q
      (_, Fail) -> p
      _ | acceptsEmpty p || acceptsEmpty q -> 
             case nonEmpty p <|> nonEmpty q of 
                Fail -> Succeed
                new  -> Succeed :|: new
        | otherwise -> p :|: q

-- | Fix-point combinator to model recursion. Be careful: this combinator is 
-- VERY powerfull, and it is your own responsibility that the result
-- is a valid, non-left-recursive grammar 
fix :: (Grammar a -> Grammar a) -> Grammar a
fix f = 
   let p = f (Var i)
       i = maximum (-1 : getFixVars p) + 1
   in Fix i p

-- | Zero or more occurrences (defined with the fix-point combinator)
star :: Grammar a -> Grammar a
star p = fix $ \this -> succeed <|> (nonEmpty p <*> this)

-- | One symbol
symbol :: a -> Grammar a
symbol = Symbol
         
-- | The empty string (singleton result)
succeed :: Grammar a
succeed = Succeed

-- | The empty grammar
fail :: Grammar a
fail = Fail

-----------------------------------------------------------
-- Grammar operations

-- | Tests whether the grammar accepts the empty string
acceptsEmpty :: Grammar a -> Bool
acceptsEmpty grammar =
   case grammar of
      p :*: q -> acceptsEmpty p && acceptsEmpty q
      p :|: q -> acceptsEmpty p || acceptsEmpty q
      Fix _ p -> acceptsEmpty p
      Succeed -> True
      _       -> False

-- | Returns the grammar without the empty string alternative
nonEmpty :: Grammar a -> Grammar a
nonEmpty grammar =
   case grammar of
      p :*: q |  acceptsEmpty p && acceptsEmpty q 
              -> let nsp = nonEmpty p
                     nsq = nonEmpty q
                 in nsp <|> nsq <|> (nsp <*> nsq)
      p :|: q -> nonEmpty p <|> nonEmpty q
      Fix i p -> nonEmpty (unfoldFix i p)
      Succeed -> fail
      _       -> grammar

-- | Returns the firsts set of the grammar, where each symbol is
-- paired with the remaining grammar
firsts :: Grammar a -> [(a, Grammar a)]
firsts grammar =
   case grammar of
      Symbol r -> [(r, succeed)]
      p :|: q  -> firsts p ++ firsts q
      p :*: q  -> map (second (<*> q)) (firsts p) ++ 
                  if acceptsEmpty p then firsts q else []
      Fix i p  -> firsts (unfoldFix i p)
      _        -> []

-- | Checks whether a string is member of the grammar's language
member :: Eq a => [a] -> Grammar a -> Bool
member xs grammar = not $ null $ filter null $ rec grammar xs
 where
   rec grammar xs =
      case grammar of
         p :*: q  -> [ zs | ys <- rec p xs, zs <- rec q ys ]
         p :|: q  -> rec p xs ++ rec q xs
         Fix i p  -> rec (unfoldFix i p) xs
         Symbol a -> case xs of
                        y:ys | y==a -> [ys]
                        _           -> []
         Succeed  -> [xs]
         Fail     -> []   
         Var _    -> error "member"

-- | Generates the language of the grammar (list can be infinite)  
language :: Grammar a -> [[a]]
language grammar = 
   case grammar of
      p :*: q  -> [ xs ++ ys | xs <- language p, ys <- language q ]
      p :|: q  -> language p ++ language q
      Fix i p  -> language (unfoldFix i p)
      Symbol a -> [[a]]
      Succeed  -> [[]]
      Fail     -> []
      Var _    -> error "language"

-- | Collect all the symbols of the grammar
collectSymbols :: Grammar a -> [a]
collectSymbols grammar = 
   case grammar of
      p :*: q  -> collectSymbols p ++ collectSymbols q
      p :|: q  -> collectSymbols p ++ collectSymbols q
      Fix _ p  -> collectSymbols p
      Symbol a -> [a]
      _        -> []

-- | The (monadic) join                      
join :: Grammar (Grammar a) -> Grammar a
join grammar =
   case grammar of
      p :*: q  -> join p <*> join q
      p :|: q  -> join p <|> join q
      Fix i p  -> Fix i (join p)
      Var i    -> Var i
      Symbol a -> a
      Succeed  -> succeed
      Fail     -> fail

-- | Label all symbols with an index (from left to right)
withIndex :: Grammar a -> Grammar (Int, a)
withIndex = snd . rec 0
 where
   rec n grammar =
      case grammar of  
         p :*: q  -> let (n1, a) = rec n  p
                         (n2, b) = rec n1 q
                     in (n2, a :*: b)
         p :|: q  -> let (n1, a) = rec n  p
                         (n2, b) = rec n1 q
                     in (n2, a :|: b)
         Fix i p  -> let (n1, a) = rec n p
                     in (n1, Fix i a)
         Var i    -> (n, Var i)
         Symbol a -> (n+1, Symbol (n, a))
         Succeed  -> (n, Succeed)
         Fail     -> (n, Fail)

-----------------------------------------------------------
-- Local helper functions
               
getFixVars :: Grammar a -> [Int]
getFixVars grammar =
   case grammar of
      p :*: q  -> getFixVars p ++ getFixVars q
      p :|: q  -> getFixVars p ++ getFixVars q
      Fix i p  -> i : getFixVars p
      _        -> []

replaceFixVar :: Int -> Grammar a -> Grammar a -> Grammar a
replaceFixVar i new = rec
 where
   rec grammar =
      case grammar of
         p :*: q  -> rec p <*> rec q
         p :|: q  -> rec p <|> rec q
         Fix j p | i /= j -> Fix j (rec p)
         Var j   | i == j -> new
         _        -> grammar
      
unfoldFix :: Int -> Grammar a -> Grammar a
unfoldFix i p = replaceFixVar i (Fix i p) p
           
--------------------------------------------------------
-- QuickCheck generator

instance Arbitrary a => Arbitrary (Grammar a) where
   arbitrary = sized arbGrammar
   coarbitrary grammar =
      case grammar of
         p :*: q  -> variant 0 . coarbitrary p . coarbitrary q
         p :|: q  -> variant 1 . coarbitrary p . coarbitrary q
         Fix i p  -> variant 2 . coarbitrary i . coarbitrary p
         Var i    -> variant 3 . coarbitrary i
         Symbol a -> variant 4 . coarbitrary a
         Succeed  -> variant 5
         Fail     -> variant 6

-- Use smart constructors here
arbGrammar :: Arbitrary a => Int -> Gen (Grammar a)
arbGrammar n
   | n == 0 = oneof
        [ liftM symbol arbitrary
        , return succeed
        , return fail 
        ]
   | otherwise = oneof
        [ arbGrammar 0
        , liftM2 (<*>) rec rec
        , liftM2 (<|>) rec rec
        , liftM star rec
        ]
 where 
   rec = arbGrammar (n `div` 2)

--------------------------------------------------------
-- QuickCheck properties                                                                 

propSymbols :: (Int -> Int) -> Grammar Int -> Bool
propSymbols f p = map f (collectSymbols p) == collectSymbols (fmap f p)

propIndexId :: Grammar Int -> Bool
propIndexId p = fmap snd (withIndex p) === p

propIndexUnique :: Grammar Int -> Bool
propIndexUnique p = is == nub is
 where is = map fst $ collectSymbols $ withIndex p

propSound :: Grammar Int -> Property
propSound p = not (null xs) ==> all (`member` p) xs
 where xs = take 20 $ language p

propNonEmpty :: Grammar Int -> Bool
propNonEmpty p = not $ member [] (nonEmpty p)

propSplitSucceed :: Grammar Int -> Bool
propSplitSucceed p = p === if acceptsEmpty p then succeed <|> new else new
 where new = nonEmpty p

propFirsts :: Grammar Int -> Bool
propFirsts p = nonEmpty p === foldr op fail (firsts p)
 where op (a, q) r = (symbol a <*> q) <|> r

propJoin :: Grammar Int -> Bool
propJoin p = join (fmap symbol p) === p
          
propMap :: (Int -> Int) -> (Int -> Int) -> Grammar Int -> Bool
propMap f g p = fmap f (fmap g p) === fmap (f . g) p

-- Does not hold: see remark for fix-points
-- propBind :: Grammar Int -> (Int -> Grammar Int) -> (Int -> Grammar Int) -> Bool
-- propBind m f g = ((m >>= f) >>= g) === (m >>= \x -> (f x >>= g))

propReturnLeft :: Int -> (Int -> Grammar Int) -> Bool
propReturnLeft x f = (return x >>= f) === f x

propReturnRight :: Grammar Int -> Bool
propReturnRight m = (m >>= return) === m	

propFix :: Grammar Int -> Property
propFix this@(Fix i p) = property (unfoldFix i p === this)
propFix _              = False ==> True

propSucceed :: Grammar Int -> Bool
propSucceed p = acceptsEmpty p == member [] p

infixl 1 ===
 
(===) :: Grammar Int -> Grammar Int -> Bool
p === q = all (`member` p) ys && all (`member` q) xs 
 where
   xs = take 20 $ language p
   ys = take 20 $ language q
   
associative op p q r  =  p `op` (q `op` r) === (p `op` q) `op` r
commutative op p q    =  p `op` q === q `op` p
idempotent  op p      =  p `op` p === p
leftUnit    op e p    =  e `op` p === p
rightUnit   op e p    =  p `op` e === p
unit        op e p    =  leftUnit op e p && rightUnit op e p
propStar         p    =  star p === succeed <|> (p <*> star p)

checks :: IO ()
checks = do
   quickCheck propReturnLeft
   quickCheck propReturnRight
   -- quickCheck propBind
   quickCheck propMap
   quickCheck propJoin
   quickCheck propSymbols
   quickCheck propIndexId
   quickCheck propIndexUnique
   quickCheck propSound
   quickCheck propNonEmpty
   quickCheck propSplitSucceed
   quickCheck propFirsts
   quickCheck propFix
   quickCheck propStar
   quickCheck propSucceed
   quickCheck $ associative (<|>)
   quickCheck $ commutative (<|>)
   quickCheck $ idempotent  (<|>)
   quickCheck $ unit (<|>) fail
   quickCheck $ associative (<*>)
   quickCheck $ unit (<*>) succeed