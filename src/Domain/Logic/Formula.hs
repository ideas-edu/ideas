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
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.Logic.Formula where

import Common.Uniplate (Uniplate(..), universe)
import Common.Rewriting
import Common.Utils
import Data.Char
import Data.List
import Data.Maybe

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
 
-- | Equality module associative-commutativity of operators
equalLogicAC:: Logic -> Logic -> Bool
equalLogicAC = equalWith operators

-- | The type LogicAlg is the algebra for the data type Logic
-- | Used in the fold for Logic.
type LogicAlg a = (String -> a, a -> a -> a, a -> a -> a, a -> a -> a, a -> a -> a, a -> a, a, a)

-- | foldLogic is the standard fold for Logic.
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

-- | eqLogic determines whether or not two Logic expression are logically 
-- | equal, by evaluating the logic expressions on all valuations.
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
      T           -> True
      F           -> True
      _           -> False

-- | Functions isDNF, and isCNF determine whether or not a Logix expression
-- | is in disjunctive normal form, or conjunctive normal form, respectively. 
isDNF, isCNF :: Logic -> Bool
isDNF = all isAtomic . concatMap conjunctions . disjunctions
isCNF = all isAtomic . concatMap disjunctions . conjunctions

-- | Function disjunctions returns all Logic expressions separated by an or
-- | operator at the top level.
disjunctions :: Logic -> [Logic]
disjunctions = collectWithOperator orOperator

-- | Function conjunctions returns all Logic expressions separated by an and
-- | operator at the top level.
conjunctions :: Logic -> [Logic]
conjunctions = collectWithOperator andOperator

-- | Count the number of implicationsations :: Logic -> Int
countImplications :: Logic -> Int
countImplications p = length [ () | _ :->: _ <- universe p ] 
 
-- | Count the number of equivalences
countEquivalences :: Logic -> Int
countEquivalences p = length [ () | _ :<->: _ <- universe p ]

-- | Count the number of binary operators
countBinaryOperators :: Logic -> Int
countBinaryOperators = foldLogic (const 0, binop, binop, binop, binop, id, 0, 0)
 where binop x y = x + y + 1

-- | Count the number of double negations 
countDoubleNegations :: Logic -> Int
countDoubleNegations p = length [ () | Not (Not _) <- universe p ] 

-- | Function varsLogic returns the variables that appear in a Logic expression.
varsLogic :: Logic -> [String]
varsLogic p = nub [ s | Var s <- universe p ]   

instance Uniplate Logic where
   uniplate p =
      case p of 
         p :->: q  -> ([p, q], \[a, b] -> a :->:  b)
         p :<->: q -> ([p, q], \[a, b] -> a :<->: b)
         p :&&: q  -> ([p, q], \[a, b] -> a :&&:  b)
         p :||: q  -> ([p, q], \[a, b] -> a :||:  b)
         Not p     -> ([p], \[a] -> Not a)
         _         -> ([], \[] -> p)

instance ShallowEq Logic where
   shallowEq expr1 expr2 =
      case (expr1, expr2) of
         (Var a, Var b)         -> a==b
         (_ :->: _ , _ :->: _ ) -> True
         (_ :<->: _, _ :<->: _) -> True
         (_ :&&: _ , _ :&&: _ ) -> True
         (_ :||: _ , _ :||: _ ) -> True
         (Not _    , Not _    ) -> True
         (T        , T        ) -> True
         (F        , F        ) -> True
         _                      -> False

instance Rewrite Logic where
   operators = logicOperators

instance MetaVar Logic where
   isMetaVar (Var ('_':xs)) | not (null xs) && all isDigit xs = return (read xs)
   isMetaVar _ = Nothing
   metaVar n = Var ("_" ++ show n)
   
logicOperators :: Operators Logic
logicOperators = [andOperator, orOperator]
   
-- The "and" operator is also commutative, but not (yet) in the equational theory
andOperator :: Operator Logic
andOperator = associativeOperator (:&&:) isAnd
 where
   isAnd (p :&&: q) = Just (p, q)
   isAnd _          = Nothing

-- The "or" operator is also commutative, but not (yet) in the equational theory
orOperator :: Operator Logic
orOperator = associativeOperator (:||:) isOr
 where
   isOr (p :||: q) = Just (p, q)
   isOr _          = Nothing