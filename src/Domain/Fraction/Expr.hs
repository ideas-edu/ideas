-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Fraction.Expr where

import Common.Unification
import Common.Utils
import Data.List
import Data.Maybe
import Ratio
import qualified Data.Set as S

infixr 1 :*:
infixr 2 :/: 
infixr 3 :+: 
infixr 4 :-:

-- | The data type Expr is the abstract syntax for the domain
-- | of arithmetic expressions.
data Expr =  Var String               -- variable
           | Expr :*: Expr            -- multiplication
           | Expr :/: Expr            -- fraction
           | Expr :+: Expr            -- addition
           | Expr :-: Expr            -- substraction
 deriving (Show, Eq, Ord)

-- | The type ExprAlg is the algebra for the data type Expr
-- | Used in the fold for Expr.
type ExprAlg a = (String -> a,
                  a -> a -> a, 
                  a -> a -> a, 
                  a -> a -> a,
                  a -> a -> a)                  

-- | foldExpr is the standard folfd for Expr.
foldExpr :: ExprAlg a -> Expr -> a
foldExpr (var, mul, frac, add, sub) = rec
 where
   rec expr = 
      case expr of
         Var x    -> var x
         x :*: y  -> rec x `mul`  rec y
         x :/: y  -> rec x `frac` rec y
         x :+: y  -> rec x `add`  rec y
         x :-: y  -> rec x `sub`  rec y
              
-- | evalExpr takes a function that gives a expression value to a variable,
-- | and a Expr expression, and evaluates the expression.
evalExpr :: Fractional a => (String -> a) -> Expr -> a
evalExpr env = foldExpr (env, (*), (/), (+), (-))


-- | Function to unify to logic formulas: a returned substitution maps 
-- | variables (String) to logic formulas 
unifyExpr :: Expr -> Expr -> Maybe (Substitution Expr)
unifyExpr x y = 
   case (x, y) of
      (Var v, Var w) | v == w -> return emptySubst
      (Var v, _)              -> return (singletonSubst v y)
      (_    , Var w)          -> return (singletonSubst w x)
      (x1 :*: x2,  y1 :*: y2) -> unifyList [x1, x2] [y1, y2]
      (x1 :/: x2,  y1 :/: y2) -> unifyList [x1, x2] [y1, y2]
      (x1 :+: x2,  y1 :+: y2) -> unifyList [x1, x2] [y1, y2]
      (x1 :-: x2,  y1 :-: y2) -> unifyList [x1, x2] [y1, y2]
      _ -> Nothing

{-

-- | eqExpr determines whether or not two Expr expression are arithmetically 
-- | equal, by evaluating the expressions on all valuations.
eqExpr :: Expr -> Expr -> Bool
eqExpr p q = all (\f -> evalExpr f p == evalExpr f q) fs
 where 
   xs = varsExpr p `union` varsExpr q
   fs = map (flip elem) (subsets xs) 


-- | Functions noNot, noOr, and noAnd determine whether or not a Expr 
-- | expression contains a not, or, and and constructor, respectively.
noNot, noOr, noAnd :: Expr -> Bool
noNot = foldExpr (const True, (&&), (&&), (&&), (&&), const False, True, True)
noOr  = foldExpr (const True, (&&), (&&), (&&), \_ _ -> False, id, True, True)
noAnd = foldExpr (const True, (&&), (&&), \_ _ -> False, (&&), id, True, True)

-- | A Expr expression is atomic if it is a variable or a constant True or False.
isAtomic :: Expr -> Bool
isAtomic logic = 
   case logic of
      Var _       -> True
      Not (Var _) -> True
      _           -> False

-- | Functions isDNF, and isCNF determine whether or not a Logix expression
-- | is in disjunctive normal form, or conjunctive normal form, respectively. 
isDNF, isCNF :: Expr -> Bool
isDNF = all isAtomic . concatMap conjunctions . disjunctions
isCNF = all isAtomic . concatMap disjunctions . conjunctions

-- | Function disjunctions returns all Expr expressions separated by an or
-- | operator at the top level.
disjunctions :: Expr -> [Expr]
disjunctions F          = []
disjunctions (p :||: q) = disjunctions p ++ disjunctions q
disjunctions logic      = [logic]

-- | Function conjunctions returns all Expr expressions separated by an and
-- | operator at the top level.
conjunctions :: Expr -> [Expr]
conjunctions T          = []
conjunctions (p :&&: q) = conjunctions p ++ conjunctions q
conjunctions logic      = [logic]

-}

-- | Function varsExpr returns the variables that appear in a Expr expression.
varsExpr :: Expr -> [String]
varsExpr = foldExpr (return, union, union, union, union)

instance HasVars Expr where
   getVars = S.fromList . varsExpr

instance MakeVar Expr where
   makeVar = Var
   
instance Substitutable Expr where 
   (|->) sub = foldExpr (var, (:*:), (:/:), (:+:), (:-:))
       where var x = fromMaybe (Var x) (lookupVar x sub)
       
instance Unifiable Expr where
   unify = unifyExpr
