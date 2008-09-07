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

import Common.Uniplate (Uniplate(..))
import Common.Rewriting
import Common.Utils
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import System.Random (StdGen, mkStdGen, split)
import Test.QuickCheck
import Control.Monad

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
      Var _             -> True
      Not (Var _)       -> True
      Inv (Var _)       -> True
      Not (Inv (Var _)) -> True
      U                 -> True
      E                 -> True
      _                 -> False
 
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
         Inv p           -> inverse (rec p)
         U         -> universe 
         E         -> empty

type Relation a = S.Set (a, a)

evalRelAlg :: Ord a => (String -> Relation a) -> [a] -> RelAlg -> Relation a
evalRelAlg var as = foldRelAlg (var, comp, add, conj, disj, not, inverse, universe, empty) 
 where
   pairs = cartesian as as
   comp     = \p q -> let f (a1, a2) c = (a1, c) `S.member` p && (c, a2) `S.member` q
                      in S.fromAscList [ x | x <- pairs, any (f x) as ] 
   add      = \p q -> let f (a1, a2) c = (a1, c) `S.member` p || (c, a2) `S.member` q
                      in S.fromAscList [ x | x <- pairs, all (f x) as ] 
   conj     = S.intersection
   disj     = S.union
   not      = \p -> S.fromAscList [ x | x <- pairs, x `S.notMember` p ]
   inverse  = S.map (\(x, y) -> (y, x))
   universe = S.fromAscList pairs
   empty    = S.empty

-- | Try to find a counter-example showing that the two formulas are not equivalent.
probablyEqual :: RelAlg -> RelAlg -> Bool
probablyEqual = probablyEqualWith (mkStdGen 28)

probablyEqualWith :: StdGen -> RelAlg -> RelAlg -> Bool
probablyEqualWith rng p q = all (\i -> eval i p == eval i q) (makeRngs 10 rng)
 where
   -- size of (co-)domain
   as     = [0..4]
   -- number of attemps (with different randomly generated relations)
   makeRngs n g
      | n == 0    = []
      | otherwise = let (g1, g2) = split g in g1 : makeRngs (n-1) g2
   eval g = evalRelAlg (generate 100 g (arbRelations as)) as

probablyEqualWithG :: StdGen -> RelAlg -> RelAlg -> Maybe Int
probablyEqualWithG rng p q = safeHead $ catMaybes $ zipWith f [1..] (makeRngs 500 rng)
 where
   f i g = if eval g p == eval g q then Nothing else Just i
   -- size of (co-)domain
   as     = [0..1]
   -- number of attemps (with different randomly generated relations)
   makeRngs n g
      | n == 0    = []
      | otherwise = let (g1, g2) = split g in g1 : makeRngs (n-1) g2
   eval g = evalRelAlg (generate 100 g (arbRelations as)) as
   
inspect :: [Int]
inspect = map f [1..100]
 where f i = S.size $ generate 100 (mkStdGen i) (arbRelations [0..9]) "p"

arbRelations :: Eq a => [a] -> Gen (String -> Relation a)
arbRelations as = promote (\s -> coarbitrary s (arbRelation as))

arbRelation :: Eq a => [a] -> Gen (Relation a)
arbRelation = arbRelation1

arbRelation4 as = do 
   n  <- choose (0, 100)
   let f x = do
          m <- choose (1::Int, 100)
          return [ x | n < m ]
   xs <- mapM f $ cartesian as as
   return $ S.fromAscList $ concat xs 

arbRelation3 as = do
   xs <- mapM f as 
   return $ S.fromAscList (concat xs)
 where
   f a = do
      n  <- choose (0, 2)
      bs <- replicateM n $ oneof $ map return as
      return $ zip (repeat a) bs

arbRelation2 as = 
   do i <- choose (0, length as ^ 2)
      remove i (cartesian as as)
 where
   remove 0 xs = return (S.fromAscList xs)
   remove n xs = do
      i <- choose (0, length xs - 1)
      let (ys, _:zs) = splitAt i xs
      remove (n-1) (ys++zs)

arbRelation1 as = do
   let f _ = oneof $ map return [True, False]
   xs <- filterM f (cartesian as as)
   return (S.fromAscList xs)
   

-- Test on a limited domain whether two relation algebra terms are equivalent
(===) :: RelAlg -> RelAlg -> Property
p === q = forAll arbitrary $ \n -> probablyEqualWith (mkStdGen n) p q

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

instance MetaVar RelAlg where
   isMetaVar (Var ('_':xs)) | not (null xs) && all isDigit xs = return (read xs)
   isMetaVar _ = Nothing
   metaVar n = Var ("_" ++ show n)

instance ShallowEq RelAlg where
   shallowEq expr1 expr2 = 
      case (expr1, expr2) of
         (Var a   , Var b   ) -> a==b
         (_ :.: _ , _ :.: _ ) -> True
         (_ :+: _ , _ :+: _ ) -> True
         (_ :&&: _, _ :&&: _) -> True
         (_ :||: _, _ :||: _) -> True
         (Not _   , Not _   ) -> True
         (Inv _   , Inv _   ) -> True
         (U       , U       ) -> True
         (E       , E       ) -> True
         _                    -> False