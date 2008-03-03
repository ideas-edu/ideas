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
   ( minorRule
   , Rule(..), makeRule, makeRuleList, makeSimpleRule, makeSimpleRuleList, (|-), combineRules
   , Transformation, makeTrans, makeTransList, hasArguments
   , LiftPair(..), liftTrans, liftRule, idRule, emptyRule, app, app2, app3, inverseRule, buggyRule
   , smartGen, checkRule, checkRuleSmart, propRule, propRuleConditional, checkRuleConditional, arguments
   , Argument(..), makeArgument, getArguments, useArguments, ratioArgument, ToArgument(..)
   ) where

import qualified Data.Set as S
import Data.List
import Data.Char
import Data.Ratio
import Data.Maybe
import Test.QuickCheck hiding (arguments)
import Common.Apply
import Common.Utils
import Control.Monad
import Control.Arrow (second)
import Common.Unification

-----------------------------------------------------------
--- Transformations

infix  6 |- 

data Transformation a
   = Function (a -> [a])
   | Unifiable a => Pattern (ForAll (a, a))
   | forall b . App (ArgumentList b) (a -> Maybe b) (b -> Transformation a)
   | forall b . Lift (LiftPair b a) (Transformation b)
   
instance Apply Transformation where
   applyAll (Function f) = f
   applyAll (Pattern  p) = maybe [] return . applyPattern p
   applyAll (App _ f g)  = \a -> maybe [] (\b -> applyAll (g b) a) (f a)
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

data ArgumentList a
   = Nil a
   | forall b c . Cons ((b, c) -> a, a -> (b, c)) (Argument b) (ArgumentList c)

-- smart constructor
nil :: ArgumentList ()
nil = Nil ()

-- smart constructor (provides the isomorphism proofs)
cons :: Argument a -> ArgumentList b -> ArgumentList (a, b)
cons arg list = Cons (id, id) arg list

showArguments :: ArgumentList a -> a -> [String]
showArguments (Nil _) _ = []
showArguments (Cons (_, f) arg list) a =
   let (b, c) = f a
   in showArgument arg b : showArguments list c
   
parseArguments :: ArgumentList a -> [String] -> Maybe a
parseArguments (Nil a) [] = Just a 
parseArguments (Cons (f, _) arg list) (x:xs) = do
   b <- parseArgument  arg  x
   c <- parseArguments list xs
   return $ f (b, c)
parseArguments _ _ = Nothing
   
packedArguments :: ArgumentList a -> [Some Argument]
packedArguments (Nil _) = []
packedArguments (Cons _ arg list) = Some arg : packedArguments list

{- SOLUTION WITH GADTs
data ArgumentList a where
   Cons :: Argument a -> ArgumentList b -> ArgumentList (a, b)
   Nil  :: ArgumentList ()
   
packedArguments :: ArgumentList a -> [Some Argument]
packedArguments (Cons a b) = Some a : packedArguments b
packedArguments Nil        = []

parseArguments :: ArgumentList a -> [String] -> Maybe a
parseArguments (Cons a b) (s:ss) = liftM2 (,) (parseArgument a s) (parseArguments b ss)
parseArguments Nil        []     = Just ()
parseArguments _ _               = Nothing

showArguments :: ArgumentList a -> a -> [String]
showArguments (Cons x xs) (a, b) = showArgument x a : showArguments xs b
showArguments Nil _ = [] -}

--arg1 = makeArgument "int" :: Argument Int
--arg2 = makeArgument "bool" :: Argument Bool

data Argument a = Argument
   { argumentDescription :: String
   , argumentDefault     :: Maybe a
   , parseArgument       :: String -> Maybe a
   , showArgument        :: a -> String
   }

makeArgument :: (Show a, Read a) => String -> Argument a
makeArgument descr = Argument descr Nothing parse show
 where 
   parse s = case reads s of
                [(a, xs)] | all isSpace xs -> return a
                _ -> Nothing


class ToArgument a where
   toArgument :: String -> Argument a

instance ToArgument Int where
   toArgument = makeArgument

instance ToArgument Integer where
   toArgument = makeArgument

instance Integral a => ToArgument (Ratio a) where
   toArgument = ratioArgument

ratioArgument :: Integral a => String -> Argument (Ratio a)
ratioArgument descr = Argument descr Nothing parseRatio showRatio
 where
   showRatio  r = show (numerator r) ++ if denominator r == 1 then "" else "/" ++ show (denominator r)
   parseRatio s = 
      let readDivOp s = 
             case dropWhile isSpace s of
                ('/':rest) -> return rest
                [] -> return "1"
                _  -> fail "no (/) operator" 
      in safeHead [ fromInteger x / fromInteger y 
                  | (x, s1) <- reads s
                  , s2 <- readDivOp s1
                  , (y, s3) <- reads s2
                  , y /= 0
                  , all isSpace s3 
                  ]

app :: (a -> Transformation ctx) -> Argument a -> (ctx -> Maybe a) -> Transformation ctx
app f arg g = 
   let args = cons arg nil
       nest a = (a, ())
   in App args (fmap nest . g) (\(a, ()) -> f a)
            
app2 :: (a -> b -> Transformation ctx) -> (Argument a, Argument b) -> (ctx -> Maybe (a, b)) -> Transformation ctx
app2 f (arg1, arg2) g = 
   let args = cons arg1 (cons arg2 nil)
       nest (a, b) = (a, (b, ()))
   in App args (fmap nest . g) (\(a, (b, ())) -> f a b)

app3 :: (a -> b -> c -> Transformation ctx) -> (Argument a, Argument b, Argument c) -> (ctx -> Maybe (a, b, c)) -> Transformation ctx
app3 f (arg1, arg2, arg3) g =
   let args = cons arg1 (cons arg2 (cons arg3 nil))
       nest (a, b, c) = (a, (b, (c, ())))
   in App args (fmap nest . g) (\(a, (b, (c, ()))) -> f a b c)

hasArguments :: Rule a -> Bool
hasArguments rule =
   case transformations rule of
      [App _ _ _ ] -> True
      [Lift _ t]   -> hasArguments rule {transformations = [t]}

      _            -> False


getArguments :: Rule a -> [Some Argument]
getArguments rule = fromMaybe [] $ 
   case transformations rule of
      [App args f g] -> Just (packedArguments args)
      [Lift lp t]    -> Just (getArguments (rule {transformations = [t]}))
      _              -> Nothing
      
arguments :: Rule a -> a -> Maybe String
arguments rule a =
   case transformations rule of
      [App args f g] -> fmap (showList . showArguments args) (f a)
      [Lift lp t]    -> getter lp a >>= arguments (rule {transformations = [t]})
      _              -> Nothing
 where
   showList xs = "(" ++ concat (intersperse "," xs) ++ ")"

useArguments :: [String] -> Rule a -> a -> [a]
useArguments list rule a =
   case transformations rule of
      [App args f g] -> maybe [] (flip applyAll a . g) (parseArguments args list) 
      [Lift lp t]    -> case getter lp a of
                           Just b  -> map (\c -> setter lp c a) $ useArguments list (rule {transformations = [t]}) b
                           Nothing -> []
      _              -> applyAll rule a

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
   coarbitrary (App _ _ _)  = variant 2

-- generates sufficiently long names
arbName :: Gen String
arbName = oneof $ map (return . ('r':) . show) [1..10000]