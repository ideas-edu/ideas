module Common.Rewriting.Rule 
   ( RulePair(..), lhs, rhs
   , Rule, ruleName, nrOfMetaVars, rulePair
   , rule0, rule1, rule2, rule3, rule4, rule5
   , inverse, bothWays, checkScope
   , rewrite, rewriteM, rewriteAC
   , normalFormAC, normalForm
   ) where

import Common.Uniplate (transform)
import Common.Utils
import Common.Rewriting.MetaVar
import Common.Rewriting.AC
import Common.Rewriting.Substitution
import Common.Rewriting.Unification
import Control.Monad
import Data.Maybe
import qualified Data.IntSet as IS
      
-----------------------------------------------------------
-- Rewrite rules

infixl 1 :~>

data RulePair a = a :~> a

lhs, rhs :: RulePair a -> a
lhs (x :~> _) = x
rhs (_ :~> y) = y

data Rule a = R { ruleName :: String, nrOfMetaVars :: Int, rulePair :: Int -> RulePair a }

instance Show (Rule a) where
   show r = "[" ++ ruleName r ++ "]" 

instance Eq (Rule a) where
   r1 == r2 = ruleName r1 == ruleName r2

rule0 :: MetaVar b => String -> RulePair b -> Rule b
rule0 s = R s 0 . const

rule1 :: MetaVar a => String -> (a -> RulePair b) -> Rule b
rule1 s f = R s 1 $ \i -> f (metaVar i)

rule2 :: MetaVar a => String -> (a -> a -> RulePair b) -> Rule b
rule2 s f = R s 2 $ \i -> f (metaVar i) (metaVar (i+1))

rule3 :: MetaVar a => String -> (a -> a -> a -> RulePair b) -> Rule b
rule3 s f = R s 3 $ \i -> f (metaVar i) (metaVar (i+1)) (metaVar (i+2))

rule4 :: MetaVar a => String -> (a -> a -> a -> a -> RulePair b) -> Rule b
rule4 s f = R s 4 $ \i -> f (metaVar i) (metaVar (i+1)) (metaVar (i+2)) (metaVar (i+3))

rule5 :: MetaVar a => String -> (a -> a -> a -> a -> a -> RulePair b) -> Rule b
rule5 s f = R s 5 $ \i -> f (metaVar i) (metaVar (i+1)) (metaVar (i+2)) (metaVar (i+3)) (metaVar (i+4))

inverse :: Rewrite a => Rule a -> Maybe (Rule a)
inverse r = if checkScope new then Just new else Nothing
 where 
   swap (x :~> y) = y :~> x
   new = r { rulePair = swap . rulePair r }

bothWays :: Rewrite a => [Rule a] -> [Rule a]
bothWays rs = rs ++ catMaybes (map inverse rs)

checkScope :: Rewrite a => Rule a -> Bool
checkScope r = IS.null (getMetaVars rhs IS.\\ getMetaVars lhs)
 where lhs :~> rhs = rulePair r 0

-----------------------------------------------------------
-- Applying rewrite rules

rewrite :: Rewrite a => Rule a -> a -> Maybe a
rewrite r e = safeHead (rewriteAC [] r e)

rewriteM :: (Monad m, Rewrite a) => Rule a -> a -> m a
rewriteM r e = maybe (fail "match") return (rewrite r e) 
      
rewriteAC :: Rewrite a => [OperatorAC a] -> Rule a -> a -> [a]
rewriteAC acs r e = do
   let lhs :~> rhs = rulePair r (nextMetaVar e)
   s <- matchAC acs lhs e
   return (s |-> rhs)
      
-----------------------------------------------------------
-- Normal forms


normalFormAC :: (Rewrite a, Ord a) => [OperatorAC a] -> [Rule a] -> a -> a
normalFormAC acs rs = fixpoint $ transform $ \a ->
   case [ b | r <- rs, b <- rewriteAC acs r a ] of
      hd:_ -> normalizeACs acs hd
      _    -> a
      
normalForm :: (Rewrite a, Ord a) => [Rule a] -> a -> a
normalForm = normalFormAC []