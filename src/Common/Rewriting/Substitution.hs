module Common.Rewriting.Substitution 
   ( Substitution, emptySubst, singletonSubst, listToSubst, (@@), (@@@)
   , lookupVar, dom, removeDom, ran, (|->)
   ) where

import Common.Uniplate
import Common.Rewriting.MetaVar
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Maybe

-----------------------------------------------------------
--- Substitution


-- | Abstract data type for substitutions
newtype Substitution a = S { unS :: IM.IntMap a }

invariant :: (Uniplate a, MetaVar a) => Substitution a -> Bool
invariant s = IS.null (dom s `IS.intersection` getMetaVarsList (ran s))

makeS :: (Uniplate a, MetaVar a) => IM.IntMap a -> Substitution a
makeS m | invariant new = new
        | otherwise     = error "Rewriting.Substitution: invariant was violated"
 where
   new = S m
   
infixr 4 |->
infixr 5 @@, @@@

instance Show a => Show (Substitution a) where
   show = show . unS

-- | Returns the empty substitution
emptySubst :: (Uniplate a, MetaVar a) => Substitution a
emptySubst = makeS IM.empty

-- | Returns a singleton substitution
singletonSubst :: (MetaVar a, Uniplate a) => Int -> a -> Substitution a
singletonSubst i a
   | isMetaVar a == Just i = emptySubst
   | otherwise             = makeS (IM.singleton i a)

-- | Turns a list into a substitution
listToSubst :: (Uniplate a, MetaVar a) => [(Int, a)] -> Substitution a
listToSubst = makeS . IM.fromListWith (error "Substitution: keys are not unique")

-- | Combines two substitutions. The left-hand side substitution is first applied to
-- the co-domain of the right-hand side substitution
(@@) :: (Uniplate a, MetaVar a) => Substitution a -> Substitution a -> Substitution a
S a @@ S b = makeS $ a `IM.union` IM.map (S a |->) b

-- | Combines two substitutions with disjoint domains. If the domains are not disjoint,
-- an error is reported
(@@@) :: (Uniplate a, MetaVar a) => Substitution a -> Substitution a -> Substitution a
S a @@@ S b = makeS (IM.unionWith err a b)
 where err _ _ = error "Unification.(@@@): domains of substitutions are not disjoint"

-- | Lookups a variable in a substitution. Nothing indicates that the variable is
-- not in the domain of the substitution
lookupVar :: Int -> Substitution a -> Maybe a
lookupVar s = IM.lookup s . unS

-- | Returns the domain of a substitution (as a list)
dom :: Substitution a -> IS.IntSet
dom = IM.keysSet . unS

-- | Removes variables from the domain of a substitution
removeDom :: IS.IntSet -> Substitution a -> Substitution a
removeDom s (S a) = S (IM.filterWithKey (\k _ -> IS.member k s) a)

ran :: Substitution a -> [a]
ran = IM.elems . unS

-- | Apply the substitution
(|->) :: (MetaVar a, Uniplate a) => Substitution a -> a -> a
s |-> e = 
   case isMetaVar e of
      Just i  -> fromMaybe e (lookupVar i s)
      Nothing -> let (cs, f) = uniplate e
                 in f (map (s |->) cs)