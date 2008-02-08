module Domain.RelationAlgebra.Formula where

import Common.Unification
import Common.Utils
import Data.List
import Data.Maybe
import qualified Data.Set as S

import Test.QuickCheck

infixr 1 :.:
infixr 2 :+: 
infixr 3 :||: 
infixr 4 :&&:

-- | The data type RelAlg is the abstract syntax for the domain
-- | of logic expressions.
data RelAlg = Var String
            | RelAlg :.:  RelAlg           -- composition
            | RelAlg :+: RelAlg            -- relative addition
            | RelAlg :&&:  RelAlg          -- and (conjunction)
            | RelAlg :||:  RelAlg          -- or (disjunction)
            | Not RelAlg                   -- not
	    | Inv RelAlg                   -- inverse
            | U                            -- universe
            | E                            -- empty
 deriving (Show, Eq, Ord)

-----------------------------------
 
isAtom :: RelAlg -> Bool
isAtom  r = 
    case r of
      Var x             -> True
      Not (Var x)       -> True
      Inv (Var x)       -> True
      Not (Inv (Var x)) -> True
      otherwise         -> False
 
isMolecule :: RelAlg -> Bool
isMolecule (r :.: s) = isMolecule r && isMolecule s
isMolecule (r :+: s) = isMolecule r && isMolecule s
isMolecule r = isAtom r
 
isDisj :: RelAlg -> Bool
isDisj (r :||: s) = isDisj r && isDisj s
isDisj r = isMolecule r
      
isCNF :: RelAlg -> Bool
isCNF (r :&&: s) = isCNF r && isCNF s
isCNF r = isDisj r
 

-----------------------------------
 
 
-- | The type RelAlgAlgebra is the algebra for the data type RelAlg
-- | Used in the fold for RelAlg.
type RelAlgAlgebra a = (String -> a, a -> a -> a, a -> a -> a, a -> a -> a, a -> a -> a, a -> a, a -> a, a, a)

-- | foldRelAlg is the standard folfd for RelAlg.
foldRelAlg :: RelAlgAlgebra a -> RelAlg -> a
foldRelAlg (var, comp, add, conj, disj, not, inverse, universe, empty) = rec
 where
   rec term =
      case term of
         Var x     -> var x
         p :.: q   -> rec p `comp` rec q
         p :+: q   -> rec p `add`  rec q
         p :&&: q  -> rec p `conj` rec q
         p :||: q  -> rec p `disj` rec q
         Not p     -> not (rec p)
	 Inv p	   -> inverse (rec p)
         U         -> universe 
         E         -> empty

type Relation a = a -> a -> Bool

evalRelAlg :: (String -> Relation a) -> [a] -> RelAlg -> Relation a
evalRelAlg f as = rec 
 where
   rec term =
      case term of
         Var x     -> f x
         p :.: q   -> \a b -> any (\c -> rec p a c && rec q c b) as
         p :+: q   -> \a b -> all (\c -> rec p a c || rec q c b) as
         p :&&: q  -> \a b -> rec p a b && rec q a b
         p :||: q  -> \a b -> rec p a b || rec q a b
         Not p     -> \a b -> not (rec p a b)
	 Inv p	   -> \a b -> rec p b a
         U         -> \_ _ -> True 
         E         -> \_ _ -> False

-- Test on a limited domain whether two relation algebra terms are equivalent
(===) :: RelAlg -> RelAlg -> Property
p === q = forAll arbitrary $ \f ->
   let test a b = evalRelAlg f [0..3] p a b == evalRelAlg f [0..3] q a b
   in and [ test a b | a <- [0::Int .. 3], b <- [0..3] ]
     
testje = quickCheck $ (Not (Not (Var "x"))) === Var "x"
         
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

-- | Function varsRelAlg returns the variables that appear in a RelAlg expression.
varsRelAlg :: RelAlg -> [String]
varsRelAlg = foldRelAlg (return, union, union, union, union, id, id, [], [])      

instance HasVars RelAlg where
   getVars = S.fromList . varsRelAlg

instance MakeVar RelAlg where
   makeVar = Var
   
instance Substitutable RelAlg where 
   (|->) sub = foldRelAlg (var, (:.:), (:+:), (:&&:), (:||:), Not, Inv, U, E)
    where var x = fromMaybe (Var x) (lookupVar x sub)

instance Unifiable RelAlg where
   unify = unifyRelAlg
   
   