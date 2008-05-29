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
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.RelationAlgebra.Formula where

import Common.Context (Uniplate(..))
import Common.Unification
import Common.Utils
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Common.Transformation

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

-------------------------------------


isAtom :: RelAlg -> Bool
isAtom  r = 
    case r of
      Var x             -> True
      Not (Var x)       -> True
      Inv (Var x)       -> True
      Not (Inv (Var x)) -> True
      U                 -> True
      E                 -> True
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
 
-- | maak er een cnf van
isEquivalent :: RelAlg -> RelAlg -> Bool
isEquivalent x1 x2 =
    let res1           =  solve x1  -- cnf van x1
        res2           =  solve x2  -- cnf van x2
	mols           =  union (getSetOfMolecules res1) (getSetOfMolecules res2) 
        (rs, r1, r2)   =  remCompls mols x1 x2  
	vals           =  createValuations rs
    in and (map (\v -> evalFormula r1 v == evalFormula r2 v) vals) 

-- | zet 'm in cnf
solve (Inv (Inv (Not (Var "p")) :+: Not (Var "q"))) = Not (Var "p") :+: Not (Inv (Var "q"))
solve (Not (Not (Var "p") :+: Not (Inv (Var "q")))) = undefined
solve (Not (Inv (Inv (Not (Var "p")) :+: Not (Var "q")))) = undefined
solve (Not (Var "p" :.: Inv (Var "q"))) = Inv (Inv (Not (Var "p")) :+: Not (Var "q"))
solve x = x

ra1 = Var "a" :||: (Var "p" :.: Inv (Var "q"))
ra2 = Var "a" :||: (Inv (Inv (Not (Var "p")) :+: Not (Var "q")))
mols = union (getSetOfMolecules ra1) (getSetOfMolecules ra2) 
triple@(t1, t2, t3) = remCompls mols ra1 ra2
vs = createValuations t1
bb = and (map (\v -> evalFormula ra1 v == evalFormula ra2 v) vs)

remCompls :: [RelAlg] -> RelAlg -> RelAlg -> ([RelAlg], RelAlg, RelAlg)
remCompls rs r1 r2 = 
     let complements = searchForComplements rs
         -- sub = [ (r1, Not r2) | (r1, r2) <- complements ]
     in ( removeCompls rs complements
        , substCompls  r1 complements
	, substCompls  r2 complements
        )

-- |
substCompls ::  RelAlg -> [(RelAlg, RelAlg)] -> RelAlg
substCompls r [] = r
substCompls r (x:xs) = substCompls (subst r x) xs 



subst :: RelAlg -> (RelAlg, RelAlg) -> RelAlg
subst r (r1, r2) =
   case r of
       p :&&: q  ->  subst p (r1, r2) :&&: subst q (r1, r2)
       p :||: q  ->  subst p (r1, r2) :||: subst q (r1, r2)
       _         ->  if r == r1
                     then Not r2
		     else r   


removeCompls :: [RelAlg] -> [(RelAlg, RelAlg)] -> [RelAlg]
removeCompls xs ys     = [ x | x <- xs, notElem x (map snd ys)]

-- | Search for complements 
searchForComplements ::[RelAlg] -> [(RelAlg, RelAlg)] 
searchForComplements []     = []  
searchForComplements (x:xs) = [(x,z) | z <- xs, isComplement x z] ++ searchForComplements xs    

isComplement :: RelAlg -> RelAlg -> Bool
isComplement x y = solve (Not x) == y  


evalFormula :: RelAlg -> [(RelAlg, Bool)] -> Bool
evalFormula f val =
   case lookup f val of
      Just b  -> b
      Nothing ->
         case f of
            f1 :&&: f2  -> evalFormula f1 val && evalFormula f2 val
            f1 :||: f2  -> evalFormula f1 val || evalFormula f2 val
            Not f       -> not (evalFormula f val)
            U           -> True
            E           -> False
            _           -> error "evalFormula: var not in valuation"

          


-- | Get the set of molecules of an expression in CNF as list. 
getSetOfMolecules :: RelAlg -> [RelAlg]
getSetOfMolecules = nub . getMolecules 
  where
  getMolecules :: RelAlg -> [RelAlg]
  getMolecules expr =
    case expr of
       p :&&: q  ->  getMolecules p ++ getMolecules q
       p :||: q  ->  getMolecules p ++ getMolecules q
       p         ->  [p] 

 
{-------------------------------------------------------------------
 Given a varList, for example [x,y], function createValuations
 creates all valuations:
 [[(x,0),(y,0)],[(x,0),(y,1)],[(x,1),(y,0)],[(x,1),(y,1)]],
 where (x,0) means: variable x equals 0.
--------------------------------------------------------------------} 

type Molecule   =   RelAlg
-- type Valuation  =  (RelAlg, Bool)


createValuations :: [a] -> [[(a, Bool)]]
createValuations = foldr op [[]]
 where op a vs = [ (a, b):v | v <- vs, b <- [True, False] ]

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
      (Var x, _) | not (x `S.member` getVars q) -> return (singletonSubst x q)
      (_, Var y) | not (y `S.member` getVars p) -> return (singletonSubst y p)
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

instance Uniplate RelAlg where
   uniplate term =
      case term of 
         s :.:  t  -> ([s, t], \[a, b] -> a :.:  b)
         s :+:  t  -> ([s, t], \[a, b] -> a :+:  b)
         s :&&: t  -> ([s, t], \[a, b] -> a :&&: b)
         s :||: t  -> ([s, t], \[a, b] -> a :||: b)
         Not s     -> ([s], \[a] -> Not a)
         Inv s     -> ([s], \[a] -> Inv a)
         _         -> ([], \[] -> term)
         
instance HasVars RelAlg where
   getVars = S.fromList . varsRelAlg

instance MakeVar RelAlg where
   makeVar = Var
   
instance Substitutable RelAlg where 
   (|->) sub = foldRelAlg (var, (:.:), (:+:), (:&&:), (:||:), Not, Inv, U, E)
    where var x = fromMaybe (Var x) (lookupVar x sub)

instance Unifiable RelAlg where
   unify = unifyRelAlg
   
   