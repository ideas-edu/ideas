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
-- This module defines a set of combinators for context-free grammars. These
-- grammars are the basis of the strategies. The fix-point combinator 'fix' 
-- makes it context-free. The code is based on the RTS'08 paper
-- "Recognizing Strategies"
--
-----------------------------------------------------------------------------
module Common.Strategy.Grammar
   ( -- * Abstract data type
     Grammar
     -- * Smart constructor functions
   , (<*>), (<|>), (<||>), var, rec, fix, many, succeed, fail, symbol
     -- * Elementary operations
   , empty, firsts, nonempty 
     -- * Membership and generated language
   , member, language, languageBF
     -- * Additional functions
   , collectSymbols, join, withIndex
     -- * QuickCheck properties
   , testMe
   ) where

import Common.Uniplate
import Common.TestSuite
import Control.Monad (liftM, liftM2)
import Data.List
import Prelude hiding (fail)
import Test.QuickCheck
import qualified Data.Set as S

----------------------------------------------------------------------
-- Abstract data type

data Grammar a  =  Grammar a :*:  Grammar a 
                |  Grammar a :|:  Grammar a 
                |  Grammar a :||: Grammar a
                |  Rec Int (Grammar a) 
                |  Symbol a | Var Int | Succeed | Fail  deriving Show

infixr 3 :|:, <|>
infixr 4 :||:, <||>
infixr 5 :*:, <*>

----------------------------------------------------------------------
-- Smart constructor functions

-- simple constructors
succeed, fail ::        Grammar a
var           :: Int -> Grammar a
symbol        :: a   -> Grammar a

succeed  = Succeed
fail     = Fail   
symbol   = Symbol
var      = Var

-- | Smart constructor for sequences: removes fails and succeeds in the
-- operands
(<*>) :: Grammar a -> Grammar a -> Grammar a
Succeed    <*> t        = t
s          <*> Succeed  = s
Fail       <*> _        = fail
_          <*> Fail     = fail
(s :*: t)  <*> u        = s :*: (t <*> u)
s          <*> t        = s :*: t

-- | Smart constructor for alternatives: removes fails in the operands, and 
-- merges succeeds if present in both arguments
(<|>) :: Grammar a -> Grammar a -> Grammar a
Fail       <|> t       = t
s          <|> Fail    = s
(s :|: t)  <|> u       = s :|: (t <|> u)
Succeed    <|> Succeed = Succeed
s          <|> t       = s :|: t

-- | Smart constructor for parallel execution: removes fails and succeeds in the operands
(<||>) :: Grammar a -> Grammar a -> Grammar a
Succeed     <||> t        = t
s           <||> Succeed  = s
Fail        <||> _        = fail
_           <||> Fail     = fail
(s :||: t)  <||> u        = s :||: (t <||> u)
s           <||> t        = s :||: t

-- | For constructing a recursive grammar
rec :: Int -> Grammar a -> Grammar a
rec i s = if i `S.member` freeVars s then Rec i s else s


-- | Fix-point combinator to model recursion. Be careful: this combinator is 
-- VERY powerfull, and it is your own responsibility that the result
-- is a valid, non-left-recursive grammar
fix :: (Grammar a -> Grammar a) -> Grammar a
fix f = Rec i (f (Var i)) -- disadvantage: function f is applied twice
 where
   s = allVars (f Succeed)
   i = if S.null s then 0 else S.findMax s + 1

-- | Zero or more occurrences
many :: Grammar a -> Grammar a
many s = rec 0 (succeed <|> (nonempty s <*> var 0))
{- TODO: deal with free variables?
many s = rec i (succeed <|> (nonempty s <*> var i))
 where
   vs = freeVars s
   i  = if S.null vs then 0 else 1 + S.findMax vs -}
   
----------------------------------------------------------------------
-- Elementary operations

-- | Tests whether the grammar accepts the empty string
empty :: Grammar a -> Bool
empty (s :*: t)   =  empty s && empty t
empty (s :|: t)   =  empty s || empty t
empty (s :||: t)  =  empty s && empty t
empty (Rec _ s)   =  empty s
empty Succeed     =  True
empty _           =  False

-- | Returns the firsts set of the grammar, where each symbol is
-- paired with the remaining grammar
firsts :: Grammar a -> [(a, Grammar a)]
firsts (s :*: t)   =  [ (a, s' <*> t) | (a, s') <- firsts s ] ++
                      (if empty s then firsts t else [])
firsts (s :|: t)   =  firsts s ++ firsts t
firsts (s :||: t)  =  [ (a, s'  <||>  t   ) | (a, s') <- firsts s ] ++
                      [ (a, s   <||>  t'  ) | (a, t') <- firsts t]
firsts (Rec i s)   =  firsts (replaceVar i (Rec i s) s)
firsts (Symbol a)  =  [(a, succeed)]
firsts _           =  []

-- | Returns the grammar without the empty string alternative
nonempty :: Grammar a -> Grammar a
nonempty s = foldr (<|>) fail [ symbol a <*> t | (a, t) <- firsts s ]

----------------------------------------------------------------------
-- Membership and generated language

-- | Checks whether a string is member of the grammar's language
member :: Eq a => [a] -> Grammar a -> Bool
member [] g     = empty g
member (a:as) g = not $ null [ () | (b, t) <- firsts g, a==b, member as t ]

-- | Generates the language of the grammar (list can be infinite). The sentences are 
-- returned sorted by length, thus in a breadth-first order. The integer that is passed
-- is the cut-off depth (the maximal length of the sentences) needed to avoid non-termination
language :: Int -> Grammar a -> [[a]]
language n = concat . take n . languageBF

-- | Generates the language of a grammar in a breadth-first manner, which is made explicit
-- by the outermost list. Sentences are grouped by their length
languageBF :: Grammar a -> [[[a]]]
languageBF s = [ [] | empty s ] : merge [ map (map (a:)) $ languageBF t | (a, t) <- firsts s ]
 where merge = map concat . transpose

----------------------------------------------------------------------
-- Additional functions

-- | Collect all the symbols of the grammar
collectSymbols :: Grammar a -> [a]
collectSymbols (Symbol a) = [a]
collectSymbols g          = compos [] (++) collectSymbols g

-- | The (monadic) join 
join :: Grammar (Grammar a) -> Grammar a
join = mapSymbol id

-- | Label all symbols with an index (from left to right)
withIndex :: Grammar a -> Grammar (Int, a)
withIndex = snd . rec 0
 where
   rec :: Int -> Grammar a -> (Int, Grammar (Int, a))
   rec n grammar =
      case grammar of  
         p :*: q   -> let (n1, a) = rec n  p
                          (n2, b) = rec n1 q
                      in (n2, a :*: b)
         p :|: q   -> let (n1, a) = rec n  p
                          (n2, b) = rec n1 q
                      in (n2, a :|: b)
         p :||: q  -> let (n1, a) = rec n  p
                          (n2, b) = rec n1 q
                      in (n2, a :||: b)
         Rec i s   -> let (n1, a) = rec n s
                      in (n1, Rec i a)
         Var i     -> (n, Var i)
         Symbol a  -> (n+1, Symbol (n, a))
         Succeed   -> (n, Succeed)
         Fail      -> (n, Fail)

----------------------------------------------------------------------
-- Local helper functions and instances

instance Uniplate (Grammar a) where
   uniplate (s :*: t)  = ([s,t], \[a,b] -> a :*: b)
   uniplate (s :|: t)  = ([s,t], \[a,b] -> a :|: b)
   uniplate (s :||: t) = ([s,t], \[a,b] -> a :||: b)
   uniplate (Rec i s)  = ([s]  , \[a]   -> Rec i a)
   uniplate g          = ([]   , \[]    -> g)

instance Functor Grammar where
   fmap f = mapSymbol (symbol . f)

freeVars :: Grammar a -> S.Set Int
freeVars (Rec i s) = freeVars s S.\\ S.singleton i
freeVars (Var i)   = S.singleton i
freeVars g         = compos S.empty S.union freeVars g

allVars :: Grammar a -> S.Set Int
allVars (Var i) = S.singleton i
allVars g       = compos S.empty S.union allVars g

replaceVar :: Int -> Grammar a -> Grammar a -> Grammar a
replaceVar i new = rec 
 where
   rec g =
      case g of 
         Var j   | i==j -> new
         Rec j _ | i==j -> g
         _              -> f $ map rec cs
          where (cs, f) = uniplate g

mapSymbol :: (a -> Grammar b) -> Grammar a -> Grammar b
mapSymbol f (p :*: q)   =  mapSymbol f p  <*>   mapSymbol f q
mapSymbol f (p :|: q)   =  mapSymbol f p  <|>   mapSymbol f q
mapSymbol f (p :||: q)  =  mapSymbol f p  <||>  mapSymbol f q
mapSymbol f (Rec i p)   =  Rec i (mapSymbol f p) 
mapSymbol _ (Var i)     =  Var i
mapSymbol f (Symbol a)  =  f a
mapSymbol _ Succeed     =  Succeed
mapSymbol _ Fail        =  Fail

--------------------------------------------------------
-- QuickCheck generator

instance Arbitrary a => Arbitrary (Grammar a) where
   arbitrary = sized (arbGrammar [])
instance CoArbitrary a => CoArbitrary (Grammar a) where
   coarbitrary grammar =
      case grammar of
         p :*: q  -> variant 0 . coarbitrary p . coarbitrary q
         p :|: q  -> variant 1 . coarbitrary p . coarbitrary q
         p :||: q -> variant 2 . coarbitrary p . coarbitrary q
         Rec i p  -> variant 3 . coarbitrary i . coarbitrary p
         Var i    -> variant 4 . coarbitrary i
         Symbol a -> variant 5 . coarbitrary a
         Succeed  -> variant 6
         Fail     -> variant 7

-- Use smart constructors here
arbGrammar :: Arbitrary a => [Grammar a] -> Int -> Gen (Grammar a)
arbGrammar xs n
   | n == 0 = oneof $
        liftM symbol arbitrary :
        map return ([succeed, fail] ++ xs)
   | otherwise = oneof
        [ arbGrammar xs 0
        , liftM2 (<*>)  rec rec
        , liftM2 (<|>)  rec rec
        , liftM2 (<||>) rec rec
        , liftM many rec
--         , liftM fix (promote (\x -> arbGrammar (x:xs) (n `div` 2)))
{-        , do i <- oneof $ map return [1::Int ..5]
             x <- arbGrammar (Var i:xs) (n `div` 2)
             return $ Rec i x -}
        ]
 where 
   rec = arbGrammar xs (n `div` 2)
   
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
 where xs = take 20 $ language 10 p

propEmpty :: Grammar Int -> Bool
propEmpty s = empty s == member [] s

propNonEmpty :: Grammar Int -> Bool
propNonEmpty = not . member [] . nonempty

propSplitSucceed :: Grammar Int -> Bool
propSplitSucceed p = p === if empty p then succeed <|> new else new
 where new = nonempty p

propFirsts :: Grammar Int -> Bool
propFirsts p = nonempty p === foldr op fail (firsts p)
 where op (a, q) r = (symbol a <*> q) <|> r

propJoin :: Grammar Int -> Bool
propJoin p = join (fmap symbol p) === p
          
propMap :: (Int -> Int) -> (Int -> Int) -> Grammar Int -> Bool
propMap f g p = fmap (f . g) p === fmap (f . g) p

propRec :: Grammar Int -> Property
propRec this@(Rec i p) = property (replaceVar i this p === this)
propRec _              = False ==> True

propSucceed :: Grammar Int -> Bool
propSucceed p = empty p == member [] p

infixl 1 ===
 
(===) :: Grammar Int -> Grammar Int -> Bool
p === q = all (`member` p) ys && all (`member` q) xs 
 where
   xs = take 20 $ language 10 p
   ys = take 20 $ language 10 q
   
associative op p q r  =  p `op` (q `op` r) === (p `op` q) `op` r
commutative op p q    =  p `op` q === q `op` p
idempotent  op p      =  p `op` p === p
leftUnit    op e p    =  e `op` p === p
rightUnit   op e p    =  p `op` e === p
unit        op e p    =  leftUnit op e p && rightUnit op e p
absorb      op e p    =  (e `op` p === e) && (p `op` e === e)
propStar         p    =  many p === succeed <|> (p <*> many p)
propStarStar     p    =  many (many p) === many p

testMe :: TestSuite
testMe = suite "Grammar combinators" $ do
   addProperty "map" propMap
   addProperty "join" propJoin
   addProperty "symbols" propSymbols
   addProperty "index" propIndexId
   addProperty "index unique" propIndexUnique
   addProperty "sound" propSound
   addProperty "empty" propEmpty
   addProperty "nonempty" propNonEmpty
   addProperty "split succeed" propSplitSucceed
   addProperty "firsts" propFirsts
   addProperty "rec" propRec
   addProperty "star" propStar
   addProperty "star star" propStarStar
   addProperty "succeed" propSucceed
   addProperty "associative or" $ associative (<|>)
   addProperty "commutative or" $ commutative (<|>)
   addProperty "idempotent or" $ idempotent  (<|>)
   addProperty "unit or" $ unit (<|>) fail
   addProperty "associative and" $ associative (<*>)
   addProperty "unit and" $ unit (<*>) succeed
   addProperty "absorb and" $ absorb (<*>) fail
   addProperty "associative parallel" $ associative (<||>)
   addProperty "commutative parallel" $ commutative (<||>)
   addProperty "unit parallel" $ unit (<||>) succeed
   addProperty "absorb parallel" $ absorb (<||>) fail