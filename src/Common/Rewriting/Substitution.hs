module Common.Rewriting.Substitution where

import Common.Uniplate
import Common.Rewriting.MetaVar
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List

-----------------------------------------------------------
--- Substitution

infixr 4 |->
infixr 5 @@, @@@

-- | Abstract data type for substitutions
newtype Substitution a = S { unS :: IM.IntMap a }

instance Show a => Show (Substitution a) where
   show = show . unS

-- | Returns the empty substitution
emptySubst :: Substitution a
emptySubst = S IM.empty

-- | Returns a singleton substitution
singletonSubst :: (MetaVar a, Uniplate a) => Int -> a -> Substitution a
singletonSubst i a
   | isMetaVar a == Just i = emptySubst
   | i `IS.member` getMetaVars a = error "Unification.singletonSubst: occurs check failed"
   | otherwise = S (IM.singleton i a)

-- | Turns a list into a substitution
listToSubst :: (Uniplate a, MetaVar a) => [(Int, a)] -> Substitution a
listToSubst s
   | nub xs /= xs       = error "Unification.listToSubst: keys are not unique"
   | any (`elem` xs) ys = error "Unification.listToSubst: occurs check failed"
   | otherwise          = S (IM.fromList s) 
 where
   xs = map fst s
   ys = IS.toList $ getMetaVarsList $ map snd s

-- | Combines two substitutions. The left-hand side substitution is first applied to
-- the co-domain of the right-hand side substitution
(@@) :: (Uniplate a, MetaVar a) => Substitution a -> Substitution a -> Substitution a
S a @@ S b = S $ a `IM.union` IM.map (S a |->) b

-- | Combines two substitutions with disjoint domains. If the domains are not disjoint,
-- an error is reported
(@@@) :: Substitution a -> Substitution a -> Substitution a
S a @@@ S b = S (IM.unionWith err a b)
 where err _ _ = error "Unification.(@@@): domains of substitutions are not disjoint"

-- | Lookups a variable in a substitution. Nothing indicates that the variable is
-- not in the domain of the substitution
lookupVar :: Int -> Substitution a -> Maybe a
lookupVar s = IM.lookup s . unS

-- | Returns the domain of a substitution (as a list)
dom :: Substitution a -> IS.IntSet
dom = IM.keysSet . unS

-- | Returns the domain of a substitution (as a list)
domList :: Substitution a -> [Int]
domList = IM.keys . unS

-- | Removes variables from the domain of a substitution
removeDom :: IS.IntSet -> Substitution a -> Substitution a
removeDom s (S a) = S (IM.filterWithKey (\k _ -> IS.member k s) a)

-- | Apply the substitution
(|->) :: (MetaVar a, Uniplate a) => Substitution a -> a -> a
s |-> e = 
   case isMetaVar e of
      Just i  -> maybe e id (lookupVar i s)
      Nothing -> let (cs, f) = uniplate e
                 in f (map (s |->) cs)