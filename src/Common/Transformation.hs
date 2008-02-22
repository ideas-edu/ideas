{-# OPTIONS -fglasgow-exts #-} 
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
   ( Apply(..), applyD, applicable, applyList, applyListAll, applyListD, applyListM, minorRule
   , Rule(..), makeRule, makeRuleList, makeSimpleRule, makeSimpleRuleList, (|-), combineRules
   , Transformation, makeTrans, makeTransList, hasArguments
   , LiftPair(..), liftTrans, liftRule, idRule, emptyRule, app, app2, app3, inverseRule, buggyRule
   , smartGen, checkRule, checkRuleSmart, propRule, propRuleConditional, checkRuleConditional, arguments
   ) where

import qualified Data.Set as S
import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck hiding (arguments)
import Common.Utils
import Control.Monad
import Common.Unification

class Apply t where
   apply    :: t a -> a -> Maybe a
   applyAll :: t a -> a -> [a] 
   -- default definitions
   apply    ta = safeHead . applyAll ta
   applyAll ta = maybe [] return . apply ta

applicable :: Apply t => t a -> a -> Bool
applicable ta = isJust . apply ta

applyD :: Apply t => t a -> a -> a
applyD ta a = fromMaybe a (apply ta a)

applyM :: (Apply t, Monad m) => t a -> a -> m a
applyM ta a = maybe (fail "applyM") return (apply ta a)
 
applyList :: Apply t => [t a] -> a -> Maybe a
applyList xs a = foldl (\ma t -> join $ fmap (apply t) ma) (Just a) xs

applyListAll :: Apply t => [t a] -> a -> [a]
applyListAll xs a = foldl (\ma t -> concatMap (applyAll t) ma) [a] xs

applyListD :: Apply t => [t a] -> a -> a
applyListD xs a = foldl (\a t -> applyD t a) a xs

applyListM :: (Apply t, Monad m) => [t a] -> a -> m a
applyListM xs a = foldl (\ma t -> ma >>= applyM t) (return a) xs

-----------------------------------------------------------
--- Transformations

infix  6 |- 

data Transformation a
   = Function (a -> [a])
   | Unifiable a => Pattern (ForAll (a, a))
   | forall b . Show b => App (a -> Maybe b) (b -> Transformation a)
   | forall b . Lift (LiftPair b a) (Transformation b)
   
instance Apply Transformation where
   applyAll (Function f) = f
   applyAll (Pattern  p) = maybe [] return . applyPattern p
   applyAll (App f g   ) = \a -> maybe [] (\b -> applyAll (g b) a) (f a)
   applyAll (Lift lp t ) = \b -> maybe [] (map (\new -> setter lp new b) . applyAll t) (getter lp b)

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
   (lhs, rhs) <- return $ unsafeInstantiateWith substitutePair pair
   sub <-  match lhs a
   return (sub |-> rhs)

makeTrans :: (a -> Maybe a) -> Transformation a
makeTrans f = makeTransList (maybe [] return . f)

makeTransList :: (a -> [a]) -> Transformation a
makeTransList = Function

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
makeSimpleRule n f = makeSimpleRuleList n  (maybe [] return . f)

-- | Smart constructor
makeSimpleRuleList :: String -> (a -> [a]) -> Rule a
makeSimpleRuleList n f = makeRule n (Function f)

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

buggyRule :: Rule a -> Rule a 
buggyRule r = r {isBuggyRule = True}

app  :: Show x => (x ->           Transformation a) -> (a -> Maybe x)         -> Transformation a
app2 :: Show (x, y) => (x -> y ->      Transformation a) -> (a -> Maybe (x, y))    -> Transformation a
app3 :: Show (x, y, z) => (x -> y -> z -> Transformation a) -> (a -> Maybe (x, y, z)) -> Transformation a

app  f g = App g f
app2 f g = App g $ uncurry  f
app3 f g = App g $ uncurry3 f

hasArguments :: Rule a -> Bool
hasArguments rule =
   case transformations rule of
      [App _ _]  -> True
      [Lift _ t] -> hasArguments rule {transformations = [t]}
      _          -> False

arguments :: Rule a -> a -> Maybe String
arguments rule a =
   case transformations rule of
      [App f g]   -> fmap show (f a)
      [Lift lp t] -> getter lp a >>= arguments (rule {transformations = [t]})
      _           -> Nothing

-- | Returns the inverse of a rule: only rules that use unifications (i.e., that are constructed
-- | with (|-)), have a computable inverse.
inverseRule :: Rule a -> Maybe (Rule a)
inverseRule r = do
   ts <- mapM inverseTrans (transformations r)
   return r
      { name = name r ++ " [inverse]"
      , transformations = ts
      }
 where
   inverseTrans :: Transformation a -> Maybe (Transformation a)
   inverseTrans trans = 
      case trans of
         Pattern pair -> return $ Pattern $ fmap (\(lhs, rhs) -> (rhs, lhs)) pair
         _ -> Nothing

-- | Identity rule 
idRule :: Rule a
idRule = minorRule $ makeSimpleRule "Identity" return
   
emptyRule :: Rule a
emptyRule = minorRule $ makeSimpleRule "Empty" (const Nothing)
   
instance Show (Rule a) where
   show = name

instance Eq (Rule a) where
   r1 == r2 = name r1 == name r2

instance Ord (Rule a) where
   r1 `compare` r2 = name r1 `compare` name r2
     
instance Apply Rule where
   applyAll r a = concatMap (`applyAll` a) (transformations r)

-----------------------------------------------------------
--- Lifting transformations and rules

data LiftPair a b = LiftPair { getter :: b -> Maybe a, setter :: a -> b -> b }

liftTrans :: LiftPair a b -> Transformation a -> Transformation b
liftTrans = Lift

liftRule :: LiftPair a b -> Rule a -> Rule b
liftRule lp r = r {transformations = map (liftTrans lp) (transformations r)}

-----------------------------------------------------------
--- QuickCheck generator

checkRule :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Rule a -> IO ()
checkRule eq rule = do
   putStr $ "[" ++ name rule ++ "] "
   quickCheck (propRule arbitrary eq rule)

checkRuleConditional :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Rule a -> (a -> Bool) -> IO ()
checkRuleConditional eq rule condition = do
   putStr $ "[" ++ name rule ++ "] "
   quickCheck (propRuleConditional arbitrary eq rule condition)

checkRuleSmart :: (Arbitrary a, Substitutable a, Show a) => (a -> a -> Bool) -> Rule a -> IO ()
checkRuleSmart eq rule = do
   putStr $ "[" ++ name rule ++ "] "
   quickCheck (propRule (smartGen rule) eq rule)
   
propRule :: (Arbitrary a, Show a) => Gen a -> (a -> a -> Bool) -> Rule a -> (a -> Bool) -> Property
propRule gen eq rule condition = 
   forAll gen $ \a ->
      applicable rule a ==> (a `eq` applyD rule a)

propRuleConditional :: (Arbitrary a, Show a) => Gen a -> (a -> a -> Bool) -> Rule a -> (a -> Bool) -> Property
propRuleConditional gen eq rule condition = 
   forAll gen $ \a ->
      condition a ==> applicable rule a ==> (a `eq` applyD rule a)

smartGen :: (Arbitrary a, Substitutable a) => Rule a -> Gen a
smartGen rule = 
   let normal = (2*total - length pairs, arbitrary)
       total = length (transformations rule)
       pairs = [ x | p@(Pattern x) <- transformations rule ]
       special p = let ((lhs, _), unique) = instantiateWith substitutePair 1000 p
                   in do list <- vector (unique - 1000) 
                         let sub = listToSubst $ zip (map (('_':) . show) [1000..]) list
                         return (sub |-> lhs)
   in frequency $ normal : zip (repeat 1) (map special pairs)
          
instance Arbitrary a => Arbitrary (Rule a) where
   arbitrary     = liftM4 Rule arbName arbitrary arbitrary arbitrary
   coarbitrary r = coarbitrary (map ord $ name r) . coarbitrary (transformations r)

instance Arbitrary a => Arbitrary (Transformation a) where
   arbitrary = oneof [liftM Function arbitrary]
   coarbitrary (Function f) = variant 0 . coarbitrary f
   coarbitrary (Pattern _)  = variant 1
   coarbitrary (App _ _)    = variant 2

-- generates sufficiently long names
arbName :: Gen String
arbName = oneof $ map (return . ('r':) . show) [1..10000]