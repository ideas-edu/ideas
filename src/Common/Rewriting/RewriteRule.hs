{-# OPTIONS -fglasgow-exts #-}
module Common.Rewriting.RewriteRule 
   ( RuleSpec(..), lhs, rhs
   , RewriteRule, Builder, rewriteRule, BuilderList, rewriteRules
   , ruleName, nrOfMetaVars, rulePair
   , inverse, bothWays, checkScope
   , rewrite, rewriteM, rewriteWith
   , normalForm, normalFormWith
   , smartGenerator
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
import Test.QuickCheck

-----------------------------------------------------------
-- Rewrite rules

infixl 1 :~>

data RuleSpec a = a :~> a
   deriving Show

lhs, rhs :: RuleSpec a -> a
lhs (x :~> _) = x
rhs (_ :~> y) = y

data RewriteRule a = Rewrite a => R { ruleName :: String, nrOfMetaVars :: Int, rulePair :: Int -> RuleSpec a }

instance Show (RewriteRule a) where
   show r = "[" ++ ruleName r ++ "]" 

instance Eq (RewriteRule a) where
   r1 == r2 = ruleName r1 == ruleName r2

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
   countVars f   = countVars (f $ error "countVars") + 1

class Rewrite a => BuilderList t a | t -> a where
   getSpecNr   :: t -> Int -> Int -> RuleSpec a
   countSpecsL :: t -> Int
   countVarsL  :: t -> Int
   
instance Rewrite a => BuilderList (RewriteRule a) a where
   getSpecNr r n = if n==0 then rulePair r else error "getSpecNr"
   countSpecsL _ = 1
   countVarsL    = nrOfMetaVars
  
instance Rewrite a => BuilderList [RuleSpec a] a where
   getSpecNr rs n = buildSpec (rs!!n)
   countSpecsL    = length
   countVarsL _   = 0

instance BuilderList b a => BuilderList (a -> b) a where 
   getSpecNr f n i = getSpecNr (f (metaVar i)) n (i+1)
   countSpecsL f   = countSpecsL (f $ error "countSpecsL")
   countVarsL f    = countVarsL (f $ error "countSpecsL") + 1
   
rewriteRule :: Builder f a => String -> f -> RewriteRule a
rewriteRule s f = R s (countVars f) (buildSpec f)

rewriteRules :: BuilderList f a => String -> f -> [RewriteRule a]
rewriteRules s f = map (R s (countVarsL f) . getSpecNr f) [0 .. countSpecsL f-1]

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
      
rewriteWith :: Operators a -> RewriteRule a -> a -> [a]
rewriteWith ops r0@(R _ _ _) e = do
   r <- extendContext ops r0
   let lhs :~> rhs = rulePair r (nextMetaVar e)
   s <- matchWith ops lhs e
   return (s |-> rhs)
      
extendContext :: Operators a -> RewriteRule a -> [RewriteRule a]
extendContext ops r =
   case findOperator ops (lhs $ rulePair r 0) of
      Just op | isAssociative op -> 
         [r, extend (leftContext op) r, extend (rightContext op) r]
      _ -> [r]
 where
   leftContext op a (x :~> y) =
      constructor op a x :~> constructor op a y
   
   rightContext op a (x :~> y) =
      constructor op x a :~> constructor op y a

extend :: (a -> RuleSpec a -> RuleSpec a) -> RewriteRule a -> RewriteRule a
extend f (R s n g) = R s (n+1) (\i -> f (metaVar (i+n)) (g i))
      
-----------------------------------------------------------
-- Normal forms

normalFormWith :: (Rewrite a, Ord a) => [Operator a] -> [RewriteRule a] -> a -> a
normalFormWith ops rs = fixpoint $ transform $ \a ->
   case [ b | r <- rs, b <- rewriteWith ops r a ] of
      hd:_ -> normalizeWith ops hd
      _    -> a
      
normalForm :: (Rewrite a, Ord a) => [RewriteRule a] -> a -> a
normalForm rs = normalFormWith operators rs

-----------------------------------------------------------
-- Smart generator that creates instantiations of the left-hand side

smartGenerator :: RewriteRule a -> Gen a
smartGenerator r@(R _ _ _) = do 
   let a :~> _ = rulePair r 0
   let vs      = getMetaVars a
   list <- vector (IS.size vs) 
   let sub = listToSubst $ zip (IS.toList vs) list
   return (sub |-> a)