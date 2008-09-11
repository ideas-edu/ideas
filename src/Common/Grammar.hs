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
-- makes it context-free. The code is based on the RTS'08 paper
-- "Recognizing Strategies"
--
-----------------------------------------------------------------------------
module Common.Grammar
   ( -- * Abstract data type
     Grammar
     -- * Smart constructor functions
   , (<*>), (<|>), (<||>), var, rec, fix, many, succeed, fail, symbol
     -- * Elementary operations
   , empty, firsts, nonempty 
     -- * Membership and generated language
   , member, language, languageBF, run, runBF
     -- * Additional functions
   , collectSymbols, join, withIndex
     -- * QuickCheck properties
   , checks
   ) where

import Prelude hiding (fail)
import Data.List
import qualified Data.Set as S
import Common.Uniplate
import Common.Apply
import Test.QuickCheck
import Control.Monad (liftM, liftM2)

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
fix f = rec i (f (Var i)) -- disadvantage: function f is applied twice
 where
   s = allVars (f Succeed)
   i = if S.null s then 0 else S.findMax s + 1

-- | Zero or more occurrences
many :: Grammar a -> Grammar a
many s = rec 0 (succeed <|> (nonempty s <*> var 0))

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

-- | Runs a grammar with an initial term in a depth-first manner: assumes that the symbols of the 
-- grammars are applicable (e.g., that they are rules)
run :: Apply f => Grammar (f a) -> a -> [a]
run s a = [ a | empty s ] ++ [ c | (r, t) <- firsts s, b <- applyAll r a, c <- run t b ]

-- | Runs a grammar with an initial term in a breadth-first manner.
runBF :: Apply f => Grammar (f a) -> a -> [[a]]
runBF s a = [ a | empty s ] : merge [ runBF t b | (r, t) <- firsts s, b <- applyAll r a ]
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

----------------------------------------------------------------------
-- Experimental code for removing left recursion

{-
removeLeftRecursion :: Grammar a -> Grammar a
removeLeftRecursion grammar = 
   case grammar of
      Rec i s -> helper i s
      _       -> grammar

helper i s
   | null xs   = if i `S.member` freeVars basis then Rec i basis else basis
   | otherwise = helper i (basis <*> many recpart)
 where
   is = S.toList (freeVars s)
   s2 = foldr op (fmap Right s) is
   op i s = replaceVar i (Symbol (Left i)) s
   (xs, ys) = partition (either (==i) (const False) . fst) (firsts s2)
   basis   = alternatives ([ Succeed | empty (Rec i s) ] ++ map make ys)
   recpart = mapSymbol (either Var Symbol) (alternatives $ map snd xs)
   make (eva, t) = (either Var Symbol eva) <*> mapSymbol (either Var Symbol) t 

   alternatives = foldr (<|>) fail 

extraChecks = do
   let f ex xs = quickCheck $ property $ shorts ex == xs
   f ex1 [""]
   f ex2 ["","b"]
   f ex3 ["a"]
   f ex4 ["a", "aa", "aaa", "aaaa"]
   f ex5 ["", "ba", "baba"]
   f ex6 ["", "ba", "baba"]
   f ex7 ["","a","b","aa","ab","bb","aaa","aab","abb","bbb","aaaa","aaab","aabb","abbb","bbbb"]
 where   
   ex1 = many succeed
   ex2 = fix $ \this -> ((this <*> this <*> Symbol 'a') <|> Symbol 'b' <|> succeed)
   ex3 = fix $ \this -> this <|> Symbol 'a'
   ex4 = fix $ \this -> (this <|> succeed) <*> Symbol 'a'
   ex5 = fix $ \x -> fix $ \_ -> ((x <*> Symbol 'b') <*> Symbol 'a') <|> Succeed
   ex6 = fix $ \x -> (x <*> Symbol 'b' <*> Symbol 'a') <|> Succeed
   ex7 = many (symbol 'a') <*> many (symbol 'b')

   shorts = take 15 . concat . take 5 . languageBF -}
   
----------------------------------------------------------------------
-- Experimental code for turning Grammar into a Monad

{-
instance Monad Grammar where
   return  = symbol
   m >>= f = join (fmap f m) 

-- Does not hold: see remark for fix-points
propBind :: Grammar Int -> (Int -> Grammar Int) -> (Int -> Grammar Int) -> Bool
propBind m f g = ((m >>= f) >>= g) === (m >>= \x -> (f x >>= g))

propReturnLeft :: Int -> (Int -> Grammar Int) -> Bool
propReturnLeft x f = (return x >>= f) === f x

propReturnRight :: Grammar Int -> Bool
propReturnRight m = (m >>= return) === m

--   quickCheck propReturnLeft
--   quickCheck propReturnRight
--   quickCheck propBind
-}
   
--------------------------------------------------------
-- QuickCheck generator

instance Arbitrary a => Arbitrary (Grammar a) where
   arbitrary = sized arbGrammar
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
arbGrammar :: Arbitrary a => Int -> Gen (Grammar a)
arbGrammar n
   | n == 0 = oneof $
        liftM symbol arbitrary :
        map return [succeed, fail]
   | otherwise = oneof
        [ arbGrammar 0
        , liftM2 (<*>)  rec rec
        , liftM2 (<|>)  rec rec
        , liftM2 (<||>) rec rec
        , liftM many rec
--         , liftM fix (promote (\x -> arbGrammar (x:xs) (n `div` 2)))
{-        , do n <- oneof $ map return [1..5]
             x <- arbGrammar (Var n:xs) (n `div` 2)
             return $ removeLeftRecursion $ Rec n x -}
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
 where xs = take 20 $ language 10 p

propEmpty :: Grammar Int -> Bool
propEmpty s = empty s == member [] s

propNonEmpty :: Grammar Int -> Bool
propNonEmpty p = not $ member [] (nonempty p)

propSplitSucceed :: Grammar Int -> Bool
propSplitSucceed p = p === if empty p then succeed <|> new else new
 where new = nonempty p

propFirsts :: Grammar Int -> Bool
propFirsts p = nonempty p === foldr op fail (firsts p)
 where op (a, q) r = (symbol a <*> q) <|> r

propJoin :: Grammar Int -> Bool
propJoin p = join (fmap symbol p) === p
          
propMap :: (Int -> Int) -> (Int -> Int) -> Grammar Int -> Bool
propMap f g p = fmap f (fmap g p) === fmap (f . g) p

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
absorbe     op e p    =  (e `op` p === e) && (p `op` e === e)
propStar         p    =  many p === succeed <|> (p <*> many p)
propStarStar     p    =  many (many p) === many p

checks :: IO ()
checks = do
   quickCheck propMap
   quickCheck propJoin
   quickCheck propSymbols
   quickCheck propIndexId
   quickCheck propIndexUnique
   quickCheck propSound
   quickCheck propEmpty
   quickCheck propNonEmpty
   quickCheck propSplitSucceed
   quickCheck propFirsts
   quickCheck propRec
   quickCheck propStar
   quickCheck propStarStar
   quickCheck propSucceed
   quickCheck $ associative (<|>)
   quickCheck $ commutative (<|>)
   quickCheck $ idempotent  (<|>)
   quickCheck $ unit (<|>) fail
   quickCheck $ associative (<*>)
   quickCheck $ unit (<*>) succeed
   quickCheck $ absorbe (<*>) fail
   quickCheck $ associative (<||>)
   quickCheck $ commutative (<||>)
   quickCheck $ unit (<||>) succeed
   quickCheck $ absorbe (<||>) fail