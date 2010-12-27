-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Substitutions on terms. Substitutions are idempotent, and non-cyclic.
--
-----------------------------------------------------------------------------
module Common.Rewriting.Substitution 
   ( Substitution, emptySubst, singletonSubst, dom
   , (@@), (|->), listToSubst
   , tests
   ) where

import Common.Uniplate
import Common.Rewriting.Term
import Common.TestSuite
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Maybe
import Data.Monoid
import Test.QuickCheck

-----------------------------------------------------------
--- * Substitution

-- | Abstract data type for substitutions
newtype Substitution = S { unS :: IM.IntMap Term } 
   deriving Eq
   
instance Monoid Substitution where
   mempty  = emptySubst
   mappend = (@@)
 
infixr 5 |->
infixr 6 @@

instance Show Substitution where
   show = show . unS

-- | Returns the empty substitution
emptySubst :: Substitution
emptySubst = S IM.empty

-- | Returns a singleton substitution
singletonSubst :: Int -> Term -> Substitution
singletonSubst i a
   | a == Meta i         = emptySubst
   | i `elem` metaVars a = error "Substitution: cyclic"
   | otherwise           = S (IM.singleton i a)

-- | Turns a list into a substitution
listToSubst :: [(Int, Term)] -> Substitution
listToSubst = mconcat . map (uncurry singletonSubst)

-- | Combines two substitutions. The left-hand side substitution is first applied to
-- the co-domain of the right-hand side substitution
(@@) :: Substitution -> Substitution -> Substitution
s1 @@ s2
   | composable s1 s2 = S $ IM.map (s1 |->) (unS s2) `IM.union` (unS s1)
   | otherwise        = error "Substitution: cyclic"

composable :: Substitution -> Substitution -> Bool
composable s1 s2 = 
   let f = IS.unions . map metaVarSet . IM.elems . unS
   in IS.null (IS.intersection (f s1) (dom s2))

-- | Lookups a variable in a substitution. Nothing indicates that the variable is
-- not in the domain of the substitution
lookupVar :: Int -> Substitution -> Maybe Term
lookupVar s = IM.lookup s . unS

-- | Returns the domain of a substitution (as a set)
dom :: Substitution -> IS.IntSet
dom = IM.keysSet . unS

-- | Apply the substitution
(|->) :: Substitution -> Term -> Term
s |-> term = 
   case term of
      Meta i -> fromMaybe term (lookupVar i s)
      _      -> descend (s |->) term
      
-----------------------------------------------------------
--- * Test substitution properties

instance Arbitrary Substitution where
   arbitrary = do 
      n  <- choose (1, 10)
      ts <- vector n
      let is = [0..] \\ concatMap metaVars ts
      return (listToSubst (zip is ts))

tests :: TestSuite
tests = suite "Substitution" $ do
   addProperty "left unit" $ \s ->
      mempty @@ s == s
   addProperty "right unit" $ \s ->
      s @@ mempty == s
   addProperty "associative" $ \s1 s2 s3 ->
      composable s1 s2 && composable (s1 @@ s2) s3
      && composable s2 s3 && composable s1 (s2 @@ s3)  
      ==> (s1 @@ s2) @@ s3 == s1 @@ (s2 @@ s3)
   addProperty "idempotence" $ \s ->
      s @@ s == s
   addProperty "idempotence/application" $ \s a -> 
      s |-> a == s |-> (s |-> a)
   addProperty "composition" $ \s1 s2 a ->
      composable s1 s2 
      ==> s1 |-> (s2 |-> a) == (s1 @@ s2) |-> a