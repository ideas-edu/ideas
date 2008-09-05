{-# OPTIONS -fglasgow-exts #-}
module Common.Rewriting.RewriteRule 
   ( RuleSpec(..), lhs, rhs, (|-)
   , RewriteRule, Builder, rewriteRule, ruleName, nrOfMetaVars, rulePair
   , inverse, bothWays, checkScope
   , rewrite, rewriteM, rewriteWith
   , normalForm, normalFormWith
   ) where

import Common.Uniplate (transform)
import Common.Utils
import Common.Rewriting.MetaVar
import Common.Rewriting.AC
import Common.Rewriting.Substitution
import Common.Rewriting.Unification
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.IntSet as IS
import Common.Apply

-----------------------------------------------------------
-- Rewrite rules

infixl 1 :~>, |-

data RuleSpec a = a :~> a

lhs, rhs :: RuleSpec a -> a
lhs (x :~> _) = x
rhs (_ :~> y) = y

data RewriteRule a = Rewrite a => R { ruleName :: String, nrOfMetaVars :: Int, rulePair :: Int -> RuleSpec a }

instance Show (RewriteRule a) where
   show r = "[" ++ ruleName r ++ "]" 

instance Eq (RewriteRule a) where
   r1 == r2 = ruleName r1 == ruleName r2

-- to disappear
(|-) :: Rewrite a => a -> a -> RewriteRule a
a |- b = R "" (length vs) (\i -> renameMetaVars (f i) a :~> renameMetaVars (f i) b)
 where
   vs  = IS.toList (getMetaVars a)
   f i = (i+) . fromMaybe 0 . (`elemIndex` vs)

class Rewrite a => Builder t a | t -> a where
   buildSpec :: t -> Int -> RuleSpec a
   countVars :: t -> Int

instance Rewrite a => Builder (RewriteRule a) a where
   buildSpec = rulePair
   countVars = nrOfMetaVars

instance Rewrite a => Builder (RuleSpec a) a where
   buildSpec rp _ = rp
   countVars _    = 0

instance (Rewrite a, Builder b a) => Builder (a -> b) a where
   buildSpec f i = buildSpec (f (metaVar i)) (i+1)
   countVars f   = countVars (f $ error "countVars")

rewriteRule :: Builder f a => String -> f -> RewriteRule a
rewriteRule s f = R s (countVars f) (buildSpec f)

inverse :: RewriteRule a -> Maybe (RewriteRule a)
inverse r@(R _ _ _) = if checkScope new then Just new else Nothing
 where 
   swap (x :~> y) = y :~> x
   new = R (ruleName r) (nrOfMetaVars r) (swap . rulePair r)

bothWays :: Rewrite a => [RewriteRule a] -> [RewriteRule a]
bothWays rs = rs ++ catMaybes (map inverse rs)

checkScope :: Rewrite a => RewriteRule a -> Bool
checkScope r = IS.null (getMetaVars rhs IS.\\ getMetaVars lhs)
 where lhs :~> rhs = rulePair r 0

-----------------------------------------------------------
-- Applying rewrite rules

instance Apply RewriteRule where
   applyAll = rewriteM
 
rewrite :: RewriteRule a -> a -> [a]
rewrite r@(R _ _ _) = rewriteWith operators r

rewriteM :: MonadPlus m => RewriteRule a -> a -> m a
rewriteM r@(R _ _ _) e = msum $ map return $ rewriteWith operators r e
      
rewriteWith :: [Operator a] -> RewriteRule a -> a -> [a]
rewriteWith ops r@(R _ _ _) e = do
   let lhs :~> rhs = rulePair r (nextMetaVar e)
   s <- matchWith ops lhs e
   return (s |-> rhs)
      
-----------------------------------------------------------
-- Normal forms

normalFormWith :: (Rewrite a, Ord a) => [Operator a] -> [RewriteRule a] -> a -> a
normalFormWith ops rs = fixpoint $ transform $ \a ->
   case [ b | r <- rs, b <- rewriteWith ops r a ] of
      hd:_ -> normalizeWith ops hd
      _    -> a
      
normalForm :: (Rewrite a, Ord a) => [RewriteRule a] -> a -> a
normalForm rs = normalFormWith operators rs