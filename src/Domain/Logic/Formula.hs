-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Logic.Formula where

import Common.Unification
import Common.Utils
import Data.List
import Data.Maybe
import qualified Data.Set as S

infixr 1 :<->:
infixr 2 :->: 
infixr 3 :||: 
infixr 4 :&&:

-- | The data type Logic is the abstract syntax for the domain
-- | of logic expressions.
data Logic = Var String
           | Logic :->:  Logic            -- implication
           | Logic :<->: Logic            -- equivalence
           | Logic :&&:  Logic            -- and (conjunction)
           | Logic :||:  Logic            -- or (disjunction)
           | Not Logic                    -- not
           | T                            -- true
           | F                            -- false
 deriving (Show, Eq, Ord)
 
-- | The type LogicAlg is the algebra for the data type Logic
-- | Used in the fold for Logic.
type LogicAlg a = (String -> a, a -> a -> a, a -> a -> a, a -> a -> a, a -> a -> a, a -> a, a, a)

-- | foldLogic is the standard folfd for Logic.
foldLogic :: LogicAlg a -> Logic -> a
foldLogic (var, impl, equiv, and, or, not, true, false) = rec
 where
   rec logic = 
      case logic of
         Var x     -> var x
         p :->: q  -> rec p `impl`  rec q
         p :<->: q -> rec p `equiv` rec q
         p :&&: q  -> rec p `and`   rec q
         p :||: q  -> rec p `or`    rec q
         Not p     -> not (rec p)
         T         -> true 
         F         -> false
              
-- | evalLogic takes a function that gives a logic value to a variable,
-- | and a Logic expression, and evaluates the boolean expression.
evalLogic :: (String -> Bool) -> Logic -> Bool
evalLogic env = foldLogic (env, impl, (==), (&&), (||), not, True, False)
 where
   impl p q = not p || q

-- | Function to unify to logic formulas: a returned substitution maps 
-- | variables (String) to logic formulas 
unifyLogic :: Logic -> Logic -> Maybe (Substitution Logic)
unifyLogic p q = 
   case (p, q) of
      (Var x, Var y) | x==y      -> return emptySubst
      (Var x, _)                 -> return (singletonSubst x q)
      (_    , Var y)             -> return (singletonSubst y p)
      (p1 :->: p2,  q1 :->:  q2) -> unifyList [p1, p2] [q1, q2]
      (p1 :<->: p2, q1 :<->: q2) -> unifyList [p1, p2] [q1, q2]
      (p1 :&&: p2,  q1 :&&:  q2) -> unifyList [p1, p2] [q1, q2]
      (p1 :||: p2,  q1 :||:  q2) -> unifyList [p1, p2] [q1, q2]
      (Not p1,      Not q1     ) -> unify p1 q1
      (T,           T          ) -> return emptySubst
      (F,           F          ) -> return emptySubst
      _ -> Nothing

-- | eqLogic determines whether or not two Logic expression are logically 
-- | equal, by evaluating the logic expressions on all valuations.
eqLogic :: Logic -> Logic -> Bool
eqLogic p q = all (\f -> evalLogic f p == evalLogic f q) fs
 where 
   xs = varsLogic p `union` varsLogic q
   fs = map (flip elem) (subsets xs) 

-- | Functions noNot, noOr, and noAnd determine whether or not a Logic 
-- | expression contains a not, or, and and constructor, respectively.
noNot, noOr, noAnd :: Logic -> Bool
noNot = foldLogic (const True, (&&), (&&), (&&), (&&), const False, True, True)
noOr  = foldLogic (const True, (&&), (&&), (&&), \_ _ -> False, id, True, True)
noAnd = foldLogic (const True, (&&), (&&), \_ _ -> False, (&&), id, True, True)

-- | A Logic expression is atomic if it is a variable or a constant True or False.
isAtomic :: Logic -> Bool
isAtomic logic = 
   case logic of
      Var _       -> True
      Not (Var _) -> True
      _           -> False

-- | Functions isDNF, and isCNF determine whether or not a Logix expression
-- | is in disjunctive normal form, or conjunctive normal form, respectively. 
isDNF, isCNF :: Logic -> Bool
isDNF = all isAtomic . concatMap conjunctions . disjunctions
isCNF = all isAtomic . concatMap disjunctions . conjunctions

-- | Function disjunctions returns all Logic expressions separated by an or
-- | operator at the top level.
disjunctions :: Logic -> [Logic]
disjunctions F          = []
disjunctions (p :||: q) = disjunctions p ++ disjunctions q
disjunctions logic      = [logic]

-- | Function conjunctions returns all Logic expressions separated by an and
-- | operator at the top level.
conjunctions :: Logic -> [Logic]
conjunctions T          = []
conjunctions (p :&&: q) = conjunctions p ++ conjunctions q
conjunctions logic      = [logic]

-- | Function varsLogic returns the variables that appear in a Logic expression.
varsLogic :: Logic -> [String]
varsLogic = foldLogic (return, union, union, union, union, id, [], [])      

instance HasVars Logic where
   getVars = S.fromList . varsLogic

instance MakeVar Logic where
   makeVar = Var
   
instance Substitutable Logic where 
   (|->) sub = foldLogic (var, (:->:), (:<->:), (:&&:), (:||:), Not, T, F)
       where var x = fromMaybe (Var x) (lookupVar x sub)
       
instance Unifiable Logic where
   unify = unifyLogic