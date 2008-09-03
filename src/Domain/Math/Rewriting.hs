module Domain.Math.Rewriting where

import Common.Utils
import Common.Uniplate
import Common.Rewriting
import Domain.Math.Classes
import Domain.Math.Rules
import Domain.Math.Constrained
import Control.Monad
import Data.List
import qualified Data.IntSet as IS
import Data.Maybe
import Data.Monoid
      
-----------------------------------------------------------
-- Matching

rulematch :: Rewrite a => Rule a -> a -> Maybe (a, Prop (Con a))
rulematch r e = safeHead (rulematchAC [] r e)

rulematchM :: (Monad m, Rewrite a) => Rule a -> a -> m (a, Prop (Con a))
rulematchM r e = maybe (fail "match") return (rulematch r e) 
      
rulematchAC :: Rewrite a => [OperatorAC a] -> Rule a -> a -> [(a, Prop (Con a))]
rulematchAC acs r e = do 
   let Triple lhs rhs p0 = ruleTriple r (nextMetaVar e)
       wfs = [ return (WF (metaVar a)) | a <- IS.toList (getMetaVars lhs IS.\\ getMetaVars rhs) ]
       p   = mconcat (p0:wfs)
   s <- unifyAC acs lhs e
   if IS.null (getMetaVars e IS.\\ dom s)
      then return (s |-> rhs, fmap (fmap (s |->)) p)
      else []

normalFormAC :: (Rewrite a, Ord a) => [OperatorAC a] -> [Rule a] -> a -> a
normalFormAC acs rs = fixpoint $ transform $ \a ->
   case [ b | r <- rs, (b, _) <- rulematchAC acs r a ] of
      hd:_ -> normalizeACs acs hd
      _    -> a
      
normalForm :: (Rewrite a, Ord a) => [Rule a] -> a -> a
normalForm = normalFormAC []