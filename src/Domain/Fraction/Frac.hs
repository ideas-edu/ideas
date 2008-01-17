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

import Common.Unification
import Common.Utils
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
          |  Lit Rational        -- literal
          |  Frac :*: Frac       -- multiplication
          |  Frac :/: Frac       -- fraction
          |  Frac :+: Frac       -- addition
          |  Frac :-: Frac       -- substraction
 deriving (Show, Eq, Ord)


-- | The type FracAlg is the algebra for the data type Frac
-- | Used in the fold for Frac.
type FracAlg a = (String -> a,
                  Rational -> a,
                  a -> a -> a, 
                  a -> a -> a, 
                  a -> a -> a,
                  a -> a -> a)                  

-- | foldFrac is the standard folfd for Frac.
foldFrac :: FracAlg a -> Frac -> a
foldFrac (var, lit, mul, div, add, sub) = rec
 where
   rec frac = 
      case frac of
         Var x    -> var x
         Lit x    -> lit x
         x :*: y  -> rec x `mul`  rec y
         x :/: y  -> rec x `div`  rec y
         x :+: y  -> rec x `add`  rec y
         x :-: y  -> rec x `sub`  rec y
              
-- | evalFrac takes a function that gives a expression value to a variable,
-- | and a Frac expression, and evaluates the expression.
evalFrac :: (String -> Rational) -> Frac -> Rational
evalFrac env = foldFrac (env, id, (*), (/), (+), (-))

-- | Function to unify to fraction formulas: a returned substitution maps 
-- | variables (String) to fraction formulas 
unifyFrac :: Frac -> Frac -> Maybe (Substitution Frac)
unifyFrac x y = 
   case (x, y) of
      (Var v, Var w) | v == w -> return emptySubst
      (Var v, _)              -> return (singletonSubst v y)
      (_    , Var w)          -> return (singletonSubst w x)
      (Lit x, Lit y) | x == y -> return emptySubst
      (x1 :*: x2,  y1 :*: y2) -> unifyList [x1, x2] [y1, y2]
      (x1 :/: x2,  y1 :/: y2) -> unifyList [x1, x2] [y1, y2]
      (x1 :+: x2,  y1 :+: y2) -> unifyList [x1, x2] [y1, y2]
      (x1 :-: x2,  y1 :-: y2) -> unifyList [x1, x2] [y1, y2]
      _ -> Nothing


-- | eqFrac determines whether or not two Frac expression are arithmetically 
-- | equal, by evaluating the expressions on all valuations.
eqFrac :: Frac -> Frac -> Bool
eqFrac = (~=)

-- | Function varsFrac returns the variables that appear in a Frac expression.
varsFrac :: Frac -> [String]
varsFrac = foldFrac (return, (\x -> []), union, union, union, union)

instance HasVars Frac where
   getVars = S.fromList . varsFrac

instance MakeVar Frac where
   makeVar = Var

instance Substitutable Frac where 
   (|->) sub = foldFrac (var, Lit, (:*:), (:/:), (:+:), (:-:))
       where var x = fromMaybe (Var x) (lookupVar x sub)

instance Unifiable Frac where
   unify = unifyFrac

infix 1 ~=
x ~= y = let (a, b) = numFraction x
             (c, d) = numFraction y
         in normaliseM (a * d) == normaliseM (b * c)

normalise :: Frac -> Frac
normalise x = 
   let vs = S.toList $ getVars x
       v  = minimum vs
       (a, b) = fracSplit v x
       lit = normalise (simplify b)
       var = simplify (Var v :*: normalise (simplify a))
   in if null vs then simplify x else 
      case lit of 
        Lit 0 -> var
        _     -> var :+: lit
   
simplify :: Frac ->  Frac
simplify this = 
   case this of
      a :+: b -> case (simplify a, simplify b) of
                   (Lit x, Lit y) -> Lit (x+y)
                   (Lit 0, c) -> c
                   (c, Lit 0) -> c
                   (c :+: d, e) -> c :+: (d :+: e)
                   (c, d) -> c :+: d
      a :*: b -> case (simplify a, simplify b) of
                   (Lit x, Lit y) -> Lit (x*y)
                   (Lit 0, c) -> Lit 0
                   (c, Lit 0) -> Lit 0
                   (Lit 1, c) -> c
                   (c, Lit 1) -> c
                   (c :*: d, e) -> c :*: (d :*: e)
                   (c, d) -> c :*: d
      a :/: b -> case (simplify a, simplify b) of
                   (Lit x, Lit y) -> if y==0 then error "Div by zero" else Lit (x/y)
                   (Lit 0, c) -> Lit 0
                   (c, Lit 1) -> c
                   (c, d) -> c :/: d
      a :-: b -> case (simplify a, simplify b) of
                   (Lit x, Lit y) -> Lit (x-y)
                   (c, Lit 0) -> c
                   (c :-: d, e) -> c :-: (d :+: e)
                   (c, d) -> c :-: d
      _ -> this

simplifyM :: Frac -> Maybe Frac
simplifyM this = do
   case this of
      a :+: b -> do a' <- simplifyM a
                    b' <- simplifyM b
                    case (a', b') of
                      (Lit x, Lit y) -> Just $ Lit (x+y)
                      (Lit 0, c) -> Just c
                      (c, Lit 0) -> Just c
                      (c :+: d, e) -> Just $ c :+: (d :+: e)
                      (c, d) -> Just $ c :+: d
      a :*: b -> do a' <- simplifyM a
                    b' <- simplifyM b
                    case (a', b') of
                      (Lit x, Lit y) -> Just $ Lit (x*y)
                      (Lit 0, c) -> Just $ Lit 0
                      (c, Lit 0) -> Just $ Lit 0
                      (Lit 1, c) -> Just c
                      (c, Lit 1) -> Just c
                      (c :*: d, e) -> Just $ c :*: (d :*: e)
                      (c, d) -> Just $ c :*: d
      a :/: b -> do a' <- simplifyM a
                    b' <- simplifyM b
                    case (a', b') of
                      (Lit x, Lit y) -> if y==0 then Nothing else Just $ Lit (x/y)
                      (Lit 0, c) -> Just $ Lit 0
                      (c, Lit 1) -> Just c
                      (c, d) -> Just $ c :/: d
      a :-: b -> do a' <- simplifyM a
                    b' <- simplifyM b
                    case (a', b') of
                      (Lit x, Lit y) -> Just $ Lit (x-y)
                      (c, Lit 0) -> Just c
                      (c :-: d, e) -> Just $ c :-: (d :+: e)
                      (c, d) -> Just $ c :-: d
      _ -> Just this


normaliseM' :: Frac -> [String] -> Maybe Frac
normaliseM' f []     = simplifyM f  -- no variables left, so only constants
normaliseM' f (v:vs) = do let (a, b) = fracSplit v f
                          a' <- simplifyM a
                          b' <- normaliseM' b vs
                          return (Var v :*: a' :+: b')

normaliseM :: Frac -> Maybe Frac
normaliseM f = do fn <- normaliseM' f (S.toList $ getVars f)
                  simplifyM fn

nf :: Frac -> Maybe Frac
nf f = do let (n, d) = numFraction f 
          n' <- normaliseM n
          d' <- normaliseM d
          case (n', d') of 
            (Lit a, Lit b) -> return (Lit (a/b))
            (Lit 0, _)     -> return (Lit 0)
            (a, Lit 1)     -> return a
            (a, b)         -> return (a / b)

numFraction :: Frac -> (Frac, Frac)
numFraction this =
   case this of
      Var _   -> (this, Lit 1)
      Lit _   -> (this, Lit 1)
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

fracSplit :: String -> Frac -> (Frac, Frac)
fracSplit x this =
   case this of
      Var y | x==y -> (Lit 1, Lit 0)
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
                              Lit 0 -> Lit 0
                              _     -> a1 :/: b2
                     q = case b1 of
                              Lit 0 -> Lit 0
                              _     -> a2 :/: b1
                     r = case b2 of
                              Lit 0 -> Lit 0
                              _     -> a2 :/: b2
                 in (p :+: q, r)
      _ -> (Lit 0, this)

isSimplified :: Frac -> Bool
isSimplified e = all (==1) $ countLit e : map (countVar e) (varsFrac e) 

countVar :: Frac -> String -> Int
countVar f v = foldFrac (\ x -> if x == v then 1 else 0, const 0, (+), (+), (+), (+)) f

countLit :: Frac -> Int
countLit = foldFrac (const 0, \x -> 1, (*), (*), (+), (+))

instance Fractional Frac where
  (/)           = (:/:)
  fromRational  = Lit
  recip n       = 1 /n

instance Num Frac where
  (+)          = (:+:)
  (-)          = (:-:)
  (*)          = (:*:)
  negate x     = (Lit 0 :-: x)
  fromInteger  = fromRational. fromInteger
  abs          = error "Not supported: abs"
  signum       = error "Not supported: signum"

isZero, notZero :: Frac -> Bool
notZero = not . isZero
isZero (Lit n)   = n == 0
isZero (Var _)   = False
isZero (n :+: m) = n ~= negate m
isZero (n :*: m) = isZero n || isZero m
isZero (n :/: m) = isZero n
isZero (n :-: m) = n ~= m



e1 = Var "x" :/: Lit 3 :+: Lit 2 :*: Var "x" :+: Lit (9::Rational) :+: Var "y"
e2 = Lit (7%3) :*: Var "x" :+: Lit (9::Rational) :+: Var "y"

-- quickcheck generated frac which fails ruleDivReciprocal due to the
-- fact that it becomes a quadratic function, which can't be normalised by normalise
e = (Var "x" :+: Lit (3%2)) :/: (Var "x" :/: Var "x")

f = (Lit (2%1) :*: Lit (3%1) :+: Var "x" :*: Var "x") :-: Lit (1%1) :/: (Lit (3%2) :-: (Lit (3%1) :-: Var "x"))
