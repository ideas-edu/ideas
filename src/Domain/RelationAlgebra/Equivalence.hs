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
module Domain.RelationAlgebra.Equivalence where

import Common.Context (Uniplate(..), universe)
import Common.Unification
import Common.Utils
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Common.Transformation
import Domain.RelationAlgebra.Formula
import Common.Apply
import Common.Context
import Domain.RelationAlgebra.Strategies

import Test.QuickCheck
{-
infixr 1 :.:
infixr 2 :+: 
infixr 3 :||: 
infixr 4 :&&:
-}
{-
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
-}
-- | maak er een cnf van
-- isEquivalent :: RelAlg -> RelAlg -> Bool
isEquivalent x1 x2 =
    let res1           =  fromContext (applyD toCNF (inContext x1))  -- cnf van x1
        res2           =  fromContext (applyD toCNF (inContext x2)) -- cnf van x2
	mols           =  union (getSetOfMolecules res1) (getSetOfMolecules res2) 
        (rs, r1, r2)   =  remCompls mols res1 res2  
	vals           =  createValuations rs
    in and (map (\v -> evalFormula r1 v == evalFormula r2 v) vals) 
{-
-- | zet 'm in cnf
solve (Inv (Inv (Not (Var "p")) :+: Not (Var "q"))) = Not (Var "p") :+: Not (Inv (Var "q"))
solve (Not (Not (Var "p") :+: Not (Inv (Var "q")))) = undefined
solve (Not (Inv (Inv (Not (Var "p")) :+: Not (Var "q")))) = undefined
solve (Not (Var "p" :.: Inv (Var "q"))) = Inv (Inv (Not (Var "p")) :+: Not (Var "q"))
solve x = x
-}

ra1 = Var "a" :||: (Var "p" :.: Inv (Var "q"))
ra2 = Var "a" :||: (Inv (Inv (Not (Var "p")) :+: Not (Var "q")))

fa1 = Inv (Var "r" :+: Var "s")
fa2 = Inv (Var "s") :+: Inv (Var "r")
{-
mols = union (getSetOfMolecules ra1) (getSetOfMolecules ra2) 
triple@(t1, t2, t3) = remCompls mols ra1 ra2
vs = createValuations t1
bb = and (map (\v -> evalFormula ra1 v == evalFormula ra2 v) vs)
-}
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
isComplement x y = (fromContext (applyD toCNF (inContext (Not x)))) == y  


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
	    x           -> let value = lookup x val
                           in if value == Nothing
                              then error $ "evalFormula: molecule not in valuation  " ++ show (f, val)
                              else fromJust value 
	    
            

          


-- | Get the set of molecules of an expression in CNF as list. 
getSetOfMolecules :: RelAlg -> [RelAlg]
getSetOfMolecules = nub . getMolecules 
  where
  getMolecules :: RelAlg -> [RelAlg]
  getMolecules expr =
    case expr of
       p :&&: q  ->  getMolecules p ++ getMolecules q
       p :||: q  ->  getMolecules p ++ getMolecules q
       Not p     ->  getMolecules p
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

prop :: RelAlg -> RelAlg -> Bool
prop p q = (isEquivalent p q) == (probablyEqual p q)