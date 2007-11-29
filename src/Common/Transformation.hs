-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Common.Transformation 
   ( Apply(..), applyM, applicable, applyList, applyListM, applyListD, minorRule
   , Rule(..), makeRule, makeRuleList, makeSimpleRule, (|-), combineRules, Transformation
   ) where

import qualified Data.Set as S
import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck
import Control.Monad
import Common.Unification

class Apply t where
   apply  :: t a -> a -> Maybe a
   applyD :: t a -> a -> a         -- with value as default
   -- default definitions
   --   (minimal complete definition: apply or applyD)
   apply  ta a = Just (applyD ta a)
   applyD ta a = fromMaybe a (apply ta a)

applyM :: (Apply t, Monad m) => t a -> a -> m a
applyM ta a = maybe (fail "applyM") return (apply ta a)

applicable :: Apply t =>t a -> a -> Bool
applicable ta = isJust . apply ta

applyList :: Apply t => [t a] -> a -> Maybe a
applyList = applyListM

applyListM :: (Apply t, Monad m) => [t a] -> a -> m a
applyListM ts a = foldl (\ma t -> ma >>= applyM t) (return a) ts

applyListD :: Apply t => [t a] -> a -> a
applyListD ts a = foldl (flip applyD) a ts

-----------------------------------------------------------
--- Transformations

infix  6 |- 

data Transformation a
   = Function (a -> Maybe a)
   | Unifiable a => Pattern (ForAll (a, a))
   
instance Apply Transformation where
   apply (Function f) = f
   apply (Pattern  p) = applyPattern p

-- | Constructs a transformation based on two terms (a left-hand side and a
-- | right-hand side). The terms must be unifiable. It is checked that no
-- | free variables appear in the right-hand side term.
(|-) :: Unifiable a => a -> a -> Transformation a
p |- q | S.null frees = Pattern $ generalizeAll (p, q)
       | otherwise    = error $ "Transformation: free variables in transformation"
 where
   frees = getVars q S.\\ getVars p

applyPattern :: Unifiable a => ForAll (a, a) -> a -> Maybe a
applyPattern pair a = do
   mkVar <- return (makeVarInt `asTypeOf` \_ -> a)
   (lhs, rhs) <- return $ unsafeInstantiateWith mkVar pair
   sub <-  match lhs a
   return (sub |-> rhs)

-----------------------------------------------------------
--- Rules

data Rule a = Rule 
   { name            :: String
   , transformations :: [Transformation a]
   , isBuggyRule     :: Bool
   , isMinorRule     :: Bool
   }

-- | Smart constructor
makeRuleList :: String -> [Transformation a] -> Rule a
makeRuleList n ts = Rule n ts False False

-- | Smart constructor
makeRule :: String -> Transformation a -> Rule a
makeRule n ts = makeRuleList n [ts]

-- | Smart constructor
makeSimpleRule :: String -> (a -> Maybe a) -> Rule a
makeSimpleRule n f = makeRule n (Function f)

-- | Combine a list of rules. Select the first rule that is applicable (is such a rule exists)
combineRules :: [Rule a] -> Rule a
combineRules rs = Rule
   { name            = concat $ intersperse "/" $ map name rs
   , transformations = concatMap transformations rs
   , isBuggyRule     = any isBuggyRule rs
   , isMinorRule     = all isMinorRule rs
   }

minorRule :: Rule a -> Rule a 
minorRule r = r {isMinorRule = True}
   
instance Show (Rule a) where
   show = name

instance Eq (Rule a) where
   r1 == r2 = name r1 == name r2

instance Ord (Rule a) where
   r1 `compare` r2 = name r1 `compare` name r2
     
instance Apply Rule where
   apply r a = msum . map (`apply` a) . transformations $ r

-----------------------------------------------------------
--- QuickCheck generator
   
instance Arbitrary a => Arbitrary (Rule a) where
   arbitrary     = liftM4 Rule arbName arbitrary arbitrary arbitrary
   coarbitrary r = coarbitrary (map ord $ name r) . coarbitrary (transformations r)

instance Arbitrary a => Arbitrary (Transformation a) where
   arbitrary = oneof [liftM Function arbitrary]
   coarbitrary (Function f) = variant 0 . coarbitrary f
   coarbitrary (Pattern _)  = variant 0

-- generates sufficiently long names
arbName :: Gen String
arbName = oneof $ map (return . ('r':) . show) [1..10000]