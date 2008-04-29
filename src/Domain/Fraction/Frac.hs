-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Fraction.Frac where

import Common.Context (Uniplate(..))
import Common.Unification
import Common.Utils
import Common.Transformation
import Data.List
import Data.Maybe
import Ratio
import qualified Data.Set as S


infixl 7 :*:, :/: 
infixl 6 :+:, :-:

-- | The data type Frac is the abstract syntax for the domain
-- | of arithmetic expressions.
-- Perhaps expand with Neg and Mixed for mixed numbers: Mix Int Rational
data Frac =  Var String          -- variable
          |  Con Integer         -- literal
          |  Frac :*: Frac       -- multiplication
          |  Frac :/: Frac       -- fraction
          |  Frac :+: Frac       -- addition
          |  Frac :-: Frac       -- subtraction
          |  Neg Frac            -- Negate
 deriving (Show, Eq, Ord)


-- | The type FracAlg is the algebra for the data type Frac
-- | Used in the fold for Frac.
type FracAlg a = (String -> a,
                  Integer -> a,
                  a -> a -> a, 
                  a -> a -> a, 
                  a -> a -> a,
                  a -> a -> a,
                  a -> a)                  

-- | foldFrac is the standard folfd for Frac.
foldFrac :: FracAlg a -> Frac -> a
foldFrac (var, lit, mul, div, add, sub, neg) = rec
 where
   rec frac = 
      case frac of
         Var x    -> var x
         Con x    -> lit x
         x :*: y  -> rec x `mul`  rec y
         x :/: y  -> rec x `div`  rec y
         x :+: y  -> rec x `add`  rec y
         x :-: y  -> rec x `sub`  rec y
         Neg x    -> neg (rec x)
              
-- | evalFrac takes a function that gives a expression value to a variable,
-- | and a Frac expression, and evaluates the expression.
evalFrac :: (String -> Rational) -> Frac -> Rational
evalFrac env = foldFrac (env, (\x -> x%1), (*), (/), (+), (-), negate)

-- | Function to unify to fraction formulas: a returned substitution maps 
-- | variables (String) to fraction formulas 
unifyFrac :: Frac -> Frac -> Maybe (Substitution Frac)
unifyFrac x y = 
   case (x, y) of
      (Var v, Var w) | v == w -> return emptySubst
      (Var v, _)     | not (v `S.member` getVars y) -> return (singletonSubst v y)
      (_    , Var w) | not (w `S.member` getVars x) -> return (singletonSubst w x)
      (Con x, Con y) | x == y -> return emptySubst
      (x1 :*: x2,  y1 :*: y2) -> unifyList [x1, x2] [y1, y2]
      (x1 :/: x2,  y1 :/: y2) -> unifyList [x1, x2] [y1, y2]
      (x1 :+: x2,  y1 :+: y2) -> unifyList [x1, x2] [y1, y2]
      (x1 :-: x2,  y1 :-: y2) -> unifyList [x1, x2] [y1, y2]
      (Neg x, Neg y)          -> unify x y
      _ -> Nothing


-- | eqFrac determines whether or not two Frac expression are arithmetically 
-- | equal, by evaluating the expressions on all valuations.
eqFrac :: Frac -> Frac -> Bool
eqFrac = (~=)

-- | Function varsFrac returns the variables that appear in a Frac expression.
varsFrac :: Frac -> [String]
varsFrac = foldFrac (return, (\x -> []), union, union, union, union, id)

instance Uniplate Frac where
   uniplate x =
      case x of 
         x :*: y -> ([x, y], \[a, b] -> a :*: b)
         x :/: y -> ([x, y], \[a, b] -> a :/: b)
         x :+: y -> ([x, y], \[a, b] -> a :+: b)
         x :-: y -> ([x, y], \[a, b] -> a :-: b)
         Neg x   -> ([x], \[y] -> Neg y) 
         _       -> ([], \[] -> x)
         
instance HasVars Frac where
   getVars = S.fromList . varsFrac

instance MakeVar Frac where
   makeVar = Var

instance Substitutable Frac where 
   (|->) sub = foldFrac (var, Con, (:*:), (:/:), (:+:), (:-:), Neg)
       where var x = fromMaybe (Var x) (lookupVar x sub)

instance Unifiable Frac where
   unify = unifyFrac

infix 1 ~=
x ~= y = let (a, b) = numFraction x
             (c, d) = numFraction y
         in normaliseM (a * d) == normaliseM (b * c)

pushNeg :: Frac -> Frac -- push Negs inside
pushNeg this = 
   case this of
      Neg a -> case a of
                  Var _  -> this
                  Con x  -> Con (negate x)
                  b :*: c -> pushNeg (Neg b) :*: pushNeg c
                  b :/: c -> pushNeg (Neg b) :/: pushNeg c
                  b :+: c -> pushNeg (Neg b) :-: pushNeg c
                  b :-: c -> pushNeg (Neg b) :+: pushNeg c
                  Neg b   -> pushNeg b
      a :+: b -> pushNeg a :+: pushNeg b
      a :-: b -> pushNeg a :-: pushNeg b
      a :*: b -> pushNeg a :*: pushNeg b
      a :/: b -> pushNeg a :/: pushNeg b
      _ -> this

simplifyM' :: Frac -> Maybe Frac
simplifyM' this = do
   case this of
      a :+: b -> do a' <- simplifyM' a
                    b' <- simplifyM' b
                    case (a', b') of
                      (Con x, Con y) -> Just $ Con (x+y)
                      (Con 0, c) -> Just c
                      (c, Con 0) -> Just c
                      (c :+: d, e) -> Just $ c :+: (d :+: e)
                      (c, d) -> Just $ c :+: d
      a :*: b -> do a' <- simplifyM' a
                    b' <- simplifyM' b
                    case (a', b') of
                      (Con x, Con y) -> Just $ Con (x*y)
                      (Con 0, c) -> Just $ Con 0
                      (c, Con 0) -> Just $ Con 0
                      (Con 1, c) -> Just c
                      (c, Con 1) -> Just c
                      (c :*: d, e) -> Just $ c :*: (d :*: e)
                      (c, d) -> Just $ c :*: d
      a :/: b -> do a' <- simplifyM' a
                    b' <- simplifyM' b
                    case (a', b') of
                      (c, Con 0) -> Nothing
                      (Con 0, c) -> Just $ Con 0
                      (c, Con 1) -> Just c
                      (c, Con (-1)) -> Just $ Con (-1) :*: c
                      (c, d) -> Just $ c :/: d
      a :-: b -> do a' <- simplifyM' a
                    b' <- simplifyM' b
                    case (a', b') of
                      (Con x, Con y) -> Just $ Con (x-y)
                      (c, Con 0) -> Just c
                      (c :-: d, e) -> Just $ c :-: (d :+: e)
                      (c, d) -> Just $ c :-: d
      _ -> Just this

simplifyM :: Frac -> Maybe Frac
simplifyM = simplifyM' . pushNeg

normaliseM' :: Frac -> [String] -> Maybe Frac
normaliseM' f []     = simplifyM f  -- no variables left, so only constants
normaliseM' f (v:vs) = do let (a, b) = fracSplit v f
                          a' <- simplifyM a
                          b' <- normaliseM' b vs
                          return (Var v :*: a' :+: b')

normaliseM :: Frac -> Maybe Frac
normaliseM f = do fn <- normaliseM' f (varsFrac f)
                  simplifyM fn

nf :: Frac -> Maybe Frac
nf f = do let (n, d) = numFraction f 
          n' <- normaliseM n
          d' <- normaliseM d
          case (n', d') of 
            (_, Con 0)     -> Nothing
            (Con 0, _)     -> return (Con 0)
            (a, Con 1)     -> return a
            (a, b)         -> return (a :/: b)

numFraction :: Frac -> (Frac, Frac)
numFraction this =
   case this of
      Var _   -> (this, Con 1)
      Con _   -> (this, Con 1)
      a :+: b -> let (a1, a2) = numFraction a
                     (b1, b2) = numFraction b
                 in ((a1:*:b2) :+: (b1:*:a2), a2 :*: b2)
      a :*: b -> let (a1, a2) = numFraction a
                     (b1, b2) = numFraction b
                 in (a1:*:b1, a2:*:b2)
      a :-: b -> let (a1, a2) = numFraction a
                     (b1, b2) = numFraction b
                 in ((a1:*:b2) :-: (b1:*:a2), a2 :*: b2)
      a :/: b -> let (a1, a2) = numFraction a
                     (b1, b2) = numFraction b
                 in (a1:*:b2, a2:*:b1)
      Neg a   -> let (a1, a2) = numFraction a
                 in (Neg a1, a2)

fracSplit :: String -> Frac -> (Frac, Frac)
fracSplit x this =
   case this of
      Var y | x==y -> (Con 1, Con 0)
      a :+: b -> let (a1, a2) = fracSplit x a
                     (b1, b2) = fracSplit x b
                 in (a1 :+: b1, a2 :+: b2)
      a :*: b -> let (a1, a2) = fracSplit x a
                     (b1, b2) = fracSplit x b
                 in (a1 :*: b2 :+: a2 :*: b1, a2 :*: b2)
      a :-: b -> let (a1, a2) = fracSplit x a
                     (b1, b2) = fracSplit x b
                 in (a1 :-: b1, a2 :-: b2)
      a :/: b -> let (a1, a2) = fracSplit x a
                     (b1, b2) = fracSplit x b      
                     p = case b2 of
                              Con 0 -> Con 0
                              _     -> a1 :/: b2
                     q = case b1 of
                              Con 0 -> Con 0
                              _     -> a2 :/: b1
                     r = case b2 of
                              Con 0 -> Con 0
                              _     -> a2 :/: b2
                 in (p :+: q, r)
      Neg a   -> let (a1, a2) = fracSplit x a
                 in (Neg a1, Neg a2)
      _ -> (Con 0, this)

countVar :: Frac -> String -> Int
countVar f v = foldFrac (\ x -> if x == v then 1 else 0, const 0, (+), (+), (+), (+), const 0) f

countCon :: Frac -> Int
countCon = foldFrac (const 0, \x -> 1, (*), (*), (+), (+), const 0)

instance Num Frac where
  (+)          = (:+:)
  (-)          = (:-:)
  (*)          = (:*:)
  negate x     = case x of
                      Con a -> Con $ negate a
                      _     -> Neg x
  fromInteger  = Con
  abs          = error "Not supported: abs"
  signum       = error "Not supported: signum"

isZero, notZero :: Frac -> Bool
notZero = not . isZero
isZero (Con n)   = n == 0
isZero (Var _)   = False
isZero (n :+: m) = n ~= negate m
isZero (n :*: m) = isZero n || isZero m
isZero (n :/: m) = isZero n
isZero (n :-: m) = n ~= m
isZero (Neg n)   = isZero n

e1 = (Var "x" :*: (Con 1 :/: Con 2) :+: Var "x" :*: Con 3 :+: Neg (Con 5 :+: (Con 2 :/: Con 3)) )

e2 = ((Con (-3) :/: Con 5) :-: (Con (-12) :/: Con 5))

e3 = ((Var "x" :/: Var "y") :/: (Con 3 :/: Var "z"))

e4 = "((x*1199)*62)"

e5 = "(x * ((-4 - 4) / (-5 * 6)))"