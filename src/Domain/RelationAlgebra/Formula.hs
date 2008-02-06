module Domain.RelationAlgebra.Formula where

import Common.Unification
import Common.Utils
import Data.List
import Data.Maybe
import qualified Data.Set as S

infixr 1 :.:
infixr 2 :+: 
infixr 3 :||: 
infixr 4 :&&:

-- | The data type RelAlg is the abstract syntax for the domain
-- | of logic expressions.
data RelAlg = Var String
           | RelAlg :.:  RelAlg            -- composition
           | RelAlg :+: RelAlg            -- relative addition
           | RelAlg :&&:  RelAlg            -- and (conjunction)
           | RelAlg :||:  RelAlg            -- or (disjunction)
           | Not RelAlg                    -- not
	   | Inv RelAlg                    -- inverse
           | U                            -- universe
           | E                            -- empty
 deriving (Show, Eq, Ord)

{-

-- | The type RelAlgAlgebra is the algebra for the data type RelAlg
-- | Used in the fold for RelAlg.
type RelAlgAlgebra a = (String -> a, a -> a -> a, a -> a -> a, a -> a -> a, a -> a -> a, a -> a, a -> a, a, a)

-- | foldRelAlg is the standard folfd for RelAlg.
foldRelAlg :: RelAlgAlgebra a -> RelAlg -> a
foldRelAlg (var, comp, add, intersection, union, not, inverse, universe, empty) = rec
 where
   rec logic = 
      case logic of
         Var x     -> var x
         p :.: q  -> rec p `comp`  rec q
         p :+: q -> rec p `add` rec q
         p :&&: q  -> rec p `intersection`   rec q
         p :||: q  -> rec p `union`    rec q
         Not p     -> not (rec p)
	 Inv p	   -> inverse (rec p)
         U         -> universe 
         E         -> empty
              
-- | evalRelAlg takes a function that gives a logic value to a variable,
-- | and a RelAlg expression, and evaluates the boolean expression.
{-
evalRelAlg :: (String -> Bool) -> RelAlg -> Bool
evalRelAlg env = foldRelAlg (env, impl, (==), (&&), (||), not, True, False)
 where
   impl p q = not p || q
-}
-- | Function to unify to relationalgebra formulas: a returned substitution maps 
-- | variables (String) to relationalgebra formulas 
unifyRelAlg :: RelAlg -> RelAlg -> Maybe (Substitution RelAlg)
unifyRelAlg p q = 
   case (p, q) of
      (Var x, Var y) | x==y      -> return emptySubst
      (Var x, _)                 -> return (singletonSubst x q)
      (_    , Var y)             -> return (singletonSubst y p)
      (p1 :.: p2,  q1 :.:  q2) -> unifyList [p1, p2] [q1, q2]
      (p1 :+: p2, q1 :+: q2) -> unifyList [p1, p2] [q1, q2]
      (p1 :&&: p2,  q1 :&&:  q2) -> unifyList [p1, p2] [q1, q2]
      (p1 :||: p2,  q1 :||:  q2) -> unifyList [p1, p2] [q1, q2]
      (Not p1,      Not q1     ) -> unify p1 q1
      (Inv p1,	    Inv q1     ) -> unify p1 q1 
      (U,           U          ) -> return emptySubst
      (E,           E          ) -> return emptySubst
      _ -> Nothing

-- | eqRelAlg determines whether or not two RelAlg expression are logically 
-- | equal, by evaluating the logic expressions on all valuations.
{-
eqRelAlg :: RelAlg -> RelAlg -> Bool
eqRelAlg p q = all (\f -> evalRelAlg f p == evalRelAlg f q) fs
 where 
   xs = varsRelAlg p `union` varsRelAlg q
   fs = map (flip elem) (subsets xs) 
-}
-- | Functions noInv, noNot, noUnion, and noIntersection determine whether or not a RelAlg 
-- | expression contains a  inverse, not, union, and intersection constructor, respectively.
noInv, noNot, noUnion, noIntersection :: RelAlg -> Bool
noNot = foldRelAlg (const True, (&&), (&&), (&&), (&&), (&&), const False, True, True)
noOr  = foldRelAlg (const True, (&&), (&&), (&&), (&&), \_ _ -> False, id, True, True)
noUnion = foldRelAlg (const True, (&&), (&&), (&&), \_ _ -> False, (&&), id, True, True)
noIntersection = foldRelAlg (const True, (&&), (&&), \_ _ -> False,(&&), (&&), id, True, True)

-- | A RelAlg expression is atomic if it is a variable or a constant Universe or Empty.
isAtomic :: RelAlg -> Bool 
isAtomic relalg = 
   case relalg of
      Var _       -> True
      Not (Var _) -> True
      Inv (Var _) -> True
      _           -> False

 -- | A RelAlg expression is a molecule if it is a composition of atomic expressions.
isMolecule :: RelAlg -> Bool 
isMolecule relalg = 
   case relalg of
     Var  _           	->  True
     (f1 :&&: f2)  	->  False 
     (f1 :||: f2) 	->  False
     (f1 :.: f2)  	->  isMolecule f1 && isMolecule f2
     (f1 :+: f2)  	->  isMolecule f1 && isMolecule f2
     Not (Var _) 	-> True
     Inv (Var _) 	-> True   
     U            	->  True
     E             	->  True
     _           	-> False

-- | Functions isDNF, and isCNF determine whether or not a RelAlg expression
-- | is in disjunctive normal form, or conjunctive normal form, respectively. 
isDNF, isCNF :: RelAlg -> Bool
isDNF = all isMolecule . concatMap conjunctions . disjunctions
isCNF = all isMolecule . concatMap disjunctions . conjunctions

-- | Function disjunctions returns all RelAlg expressions separated by an or
-- | operator at the top level.
disjunctions :: RelAlg -> [RelAlg]
disjunctions E          = []
disjunctions (p :||: q) = disjunctions p ++ disjunctions q
disjunctions relalg      = [relalg]

-- | Function conjunctions returns all RelAlg expressions separated by an and
-- | operator at the top level.
conjunctions :: RelAlg -> [RelAlg]
conjunctions U          = []
conjunctions (p :&&: q) = conjunctions p ++ conjunctions q
conjunctions relalg      = [relalg]
{-
-- | Count the number of implicationsations :: RelAlg -> Int
countImplications :: RelAlg -> Int
countImplications = foldRelAlg (const 0, \x y -> x+y+1, (+), (+), (+), id, 0, 0)
 -}
-- | Count the number of equivalences
{-
countEquivalences :: RelAlg -> Int
countEquivalences = foldRelAlg (const 0, (+), \x y -> x+y+1, (+), (+), id, 0, 0)
-}
-- | Count the number of binary operators
countBinaryOperators :: RelAlg -> Int
countBinaryOperators = foldRelAlg (const 0, binop, binop, binop, binop, id, id,  0, 0)
 where binop x y = x + y + 1

-- | Count the number of double negations 
countDoubleNegations :: RelAlg -> Int
countDoubleNegations = fst . foldRelAlg (const zero, bin, bin, bin, bin, notf, invf, zero, zero)
 where
   zero = (0, False)
   bin (n, _) (m, _) = (n+m, False)
   invf = error "ZIE Domain.RA.Formula"
   notf (n, b) = if b then (n+1, False) else (n, True)

-- | Function varsRelAlg returns the variables that appear in a RelAlg expression.
varsRelAlg :: RelAlg -> [String]
varsRelAlg = foldRelAlg (return, union, union, union, union, id, [], [])      

test = associativityAnd $ (Var "a" :||: Var "b") :||: (Var "c" :||: Var "d" :||: Var "e")

associativityAnd, associativityOr :: RelAlg -> [RelAlg]
associativityAnd = associativity conjunctions (:&&:) [T]
associativityOr  = associativity disjunctions (:||:) [F]

-- Helper function (polymorphic, domain independent)
associativity :: (a -> [a]) -> (a -> a -> a) -> [a] -> a -> [a]
associativity f op nil = rec . f
 where
   rec ps
      | n == 0    = nil
      | n == 1    = ps
      | otherwise = concatMap f [1 .. n-1]
    where
      n = length ps
      f i = let (xs, ys) = splitAt i ps
            in [ x `op` y | x <- rec xs, y <- rec ys ]

eqAssociative :: RelAlg -> RelAlg -> Bool
eqAssociative p q =
   case (p, q) of
      (Var x, Var y)             -> x==y
      (p1 :->: p2,  q1 :->:  q2) -> eqAssociative p1 q1 && eqAssociative p2 q2
      (p1 :<->: p2, q1 :<->: q2) -> eqAssociative p1 q1 && eqAssociative p2 q2
      (_ :&&: _,  _ :&&:  _) -> and $ zipWith eqAssociative (conjunctions p) (conjunctions q)
      (_ :||: _,  _ :||:  _) -> and $ zipWith eqAssociative (disjunctions p) (disjunctions q)
      (Not p1,      Not q1     ) -> eqAssociative p1 q1
      (T,           T          ) -> True
      (F,           F          ) -> True
      _ -> False
         
instance HasVars RelAlg where
   getVars = S.fromList . varsRelAlg

instance MakeVar RelAlg where
   makeVar = Var
   
instance Substitutable RelAlg where 
   (|->) sub = foldRelAlg (var, (:->:), (:<->:), (:&&:), (:||:), Not, T, F)
       where var x = fromMaybe (Var x) (lookupVar x sub)
       
instance Unifiable RelAlg where
   unify = unifyRelAlg -}