-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Transformation 
   ( Apply(..), applyM, applicable, applyList, applyListM, applyListD
   , Rule(..), makeRule, makePatternRule, (|-), combineRules, Transformation
   ) where

import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck
import Control.Monad
import Unification

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

-- cbeck that rhs does not contain free vars
(|-) :: Unifiable a => a -> a -> [Transformation a]
p |- q = [Pattern $ generalizeAll (p, q)]

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
makeRule :: String -> (a -> Maybe a) -> Rule a
makeRule n f = Rule n [Function f] False False

makePatternRule :: String -> [Transformation a] -> Rule a
makePatternRule n ts = Rule n ts False False

-- | Combine a list of rules. Select the first rule that is applicable (is such a rule exists)
combineRules :: [Rule a] -> Rule a
combineRules rs = Rule
   { name            = concat $ intersperse "/" $ map name rs
   , transformations = concatMap transformations rs
   , isBuggyRule     = any isBuggyRule rs
   , isMinorRule     = all isMinorRule rs
   }
   
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