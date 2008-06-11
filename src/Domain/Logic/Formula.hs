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

import Common.Context (Uniplate(..), universe)
import Common.Unification
import Common.Utils
import Data.Char
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
 deriving Show

-- Equality modulo associativity
instance Eq Logic where
   p==q = case (p, q) of
             (Var x, Var y)             -> x==y
             (p1 :->: p2,  q1 :->:  q2) -> p1==q1 && p2==q2
             (p1 :<->: p2, q1 :<->: q2) -> p1==q1 && p2==q2
             (_ :&&: _,    _ :&&:  _  ) -> conjunctions p == conjunctions q
             (_ :||: _,    _ :||:  _  ) -> disjunctions p == disjunctions q
             (Not p1,      Not q1     ) -> p1==q1
             (T,           T          ) -> True
             (F,           F          ) -> True
             _ -> False


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

-- | Function to unify to logic formulas: the resulting substitution maps 
-- | variables (String) to logic formulas 
unifyLogic :: Logic -> Logic -> Maybe (Substitution Logic)
unifyLogic p q =
   case (isMetaVar p, isMetaVar q) of
      (Just i, Just j) -> return $ if i==j then emptySubst else singletonSubst i q
      (Just i, Nothing) -> if i `S.member` getMetaVars q then Nothing else return (singletonSubst i q)
      (Nothing, Just j) -> if j `S.member` getMetaVars p then Nothing else return (singletonSubst j p)
      _ -> case (rightSpine p, rightSpine q) of
             (Var x, Var y) | x==y      -> return emptySubst
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
disjunctions (p :||: q) = disjunctions p ++ disjunctions q
disjunctions logic      = [logic]

-- | Function conjunctions returns all Logic expressions separated by an and
-- | operator at the top level.
conjunctions :: Logic -> [Logic]
conjunctions (p :&&: q) = conjunctions p ++ conjunctions q
conjunctions logic      = [logic]

-- | Count the number of implicationsations :: Logic -> Int
countImplications :: Logic -> Int
countImplications = foldLogic (const 0, \x y -> x+y+1, (+), (+), (+), id, 0, 0)
 
-- | Count the number of equivalences
countEquivalences :: Logic -> Int
countEquivalences = foldLogic (const 0, (+), \x y -> x+y+1, (+), (+), id, 0, 0)

-- | Count the number of binary operators
countBinaryOperators :: Logic -> Int
countBinaryOperators = foldLogic (const 0, binop, binop, binop, binop, id, 0, 0)
 where binop x y = x + y + 1

-- | Count the number of double negations 
countDoubleNegations :: Logic -> Int
countDoubleNegations = fst . foldLogic (const zero, bin, bin, bin, bin, notf, zero, zero)
 where
   zero = (0, False)
   bin (n, _) (m, _) = (n+m, False)
   notf (n, b) = if b then (n+1, False) else (n, True)

-- | Function varsLogic returns the variables that appear in a Logic expression.
varsLogic :: Logic -> [String]
varsLogic = foldLogic (return, union, union, union, union, id, [], [])      

rightSpine :: Logic -> Logic
rightSpine ((p :&&: q) :&&: r) = rightSpine (p :&&: (q :&&: r))
rightSpine (p :&&: q) = p :&&: rightSpine q
rightSpine ((p :||: q) :||: r) = rightSpine (p :||: (q :||: r))
rightSpine (p :||: q) = p :||: rightSpine q
rightSpine p = p

instance Uniplate Logic where
   uniplate p =
      case rightSpine p of 
         p :->: q  -> ([p, q], \[a, b] -> a :->:  b)
         p :<->: q -> ([p, q], \[a, b] -> a :<->: b)
         p :&&: q  -> ([p, q], \[a, b] -> a :&&:  b)
         p :||: q  -> ([p, q], \[a, b] -> a :||:  b)
         Not p     -> ([p], \[a] -> Not a)
         _         -> ([], \[] -> p)
         
instance HasMetaVars Logic where
   getMetaVarsList = catMaybes . map isMetaVar . universe

instance MetaVar Logic where
   isMetaVar (Var ('_':xs)) | not (null xs) && all isDigit xs = return (read xs)
   isMetaVar _ = Nothing
   metaVar n = Var ("_" ++ show n)

   
instance Substitutable Logic where 
   (|->) sub = foldLogic (var, (:->:), (:<->:), (:&&:), (:||:), Not, T, F)
       where 
         var s = case isMetaVar (Var s) of
                    Just i -> fromMaybe (Var s) (lookupVar i sub)
                    _      -> Var s
       
instance Unifiable Logic where
   unify = unifyLogic