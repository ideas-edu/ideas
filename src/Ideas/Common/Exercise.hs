{-# LANGUAGE Rank2Types, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- The 'Exercise' record defines all the components that are needed for
-- calculating feedback for one class of exercises. The fields of an exercise
-- have to be consistent; consistency can be checked with the
-- "Ideas.Common.ExerciseTests" module.
--
-----------------------------------------------------------------------------

module Ideas.Common.Exercise
   ( -- * Exercise record
     Exercise(..), emptyExercise, makeExercise
     -- * Convenience functions
   , prettyPrinterContext, isReady, isSuitable
   , ruleset, getRule, ruleOrderingWith
     -- * Status
   , Status(..), isPublic, isPrivate
     -- * Examples
   , Examples, Difficulty(..), readDifficulty
   , level, mapExamples, examplesContext
     -- * Context
   , inContext, withoutContext
     -- * Type casting
   , useTypeable, castFrom, castTo
     -- * Exercise properties
   , setProperty, getProperty
     -- * Random generators
   , simpleGenerator, useGenerator, randomTerm, randomTerms
     -- * Derivations
   , showDerivation, showDerivations, printDerivation, printDerivations
   , diffEnvironment, defaultDerivation, allDerivations
   ) where

import Data.Char
import Data.Data
import Data.Dynamic
import Data.List
import Data.Maybe
import Data.Ord
import Ideas.Common.Classes
import Ideas.Common.Context
import Ideas.Common.Derivation
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Predicate
import Ideas.Common.Rewriting
import Ideas.Common.Rule
import Ideas.Common.Strategy hiding (not, fail, repeat, replicate)
import Ideas.Common.Utils (ShowString(..))
import Ideas.Common.View
import System.Random
import Test.QuickCheck hiding (label)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random (QCGen)
import qualified Data.Map as M
import qualified Ideas.Common.Strategy as S

-----------------------------------------------------------------------------
-- Exercise record

-- | For constructing an empty exercise, use function 'emptyExercise' or
-- 'makeExercise'.
data Exercise a =
  NewExercise
   { -- | Identifier that uniquely determines the exercise: see 'HasId' for
     -- how to use values with identifiers.
     exerciseId :: Id
     -- | The status of the exercise.
   , status :: Status
     -- | Parser for expressions of the exercise class, which either results
     -- in an error ('Left') or a result ('Right').
   , parser :: String -> Either String a
     -- | Pretty-printer for expressions of the exercise class. Pretty-printing
     -- should be the inverse of parsing.
   , prettyPrinter :: a -> String
     -- | Tests wether two expressions (with their contexts) are semantically
     -- equivalent. Use 'withoutContext' for defining the equivalence check
     -- when the context is not relevant.
   , equivalence :: Context a -> Context a -> Bool
     -- | Tests wether two expressions (with their contexts) are syntactically
     -- the same, or nearly so. Expressions that are similar must also be
     -- equivalent. Use 'withoutContext' if the context is not relevant for the
     -- similarity check.
   , similarity :: Context a -> Context a -> Bool
     -- | Predicate suitable identifies which expressions can be solved by the
     -- strategy of the exercise class. It acts as the pre-condition of the
     -- strategy.
   , suitable :: Predicate a
     -- | Predicate ready checks if an expression is in a solved form (accepted
     -- as a final solution). It acts as the post-condition of the strategy.
   , ready :: Predicate a
     -- | The rewrite strategy that specifies how to solve an exercise.
   , strategy :: LabeledStrategy (Context a)
     -- | Is it possible to restart the rewrite strategy at any point in time?
     -- Restarting the strategy is needed when a student deviates from the
     -- strategy (detour). By default, restarting is assumed to be possible.
   , canBeRestarted :: Bool
     -- | Are there extra rules, possibly buggy, that do not appear in the
     -- strategy? Use 'ruleset' to get all rules.
   , extraRules :: [Rule (Context a)]
     -- | The rule ordering is a tiebreaker in situations where more than one
     -- rule can be used (e.g. feedback services onefirst and derivation; other
     -- feedback services return all possible rules).
   , ruleOrdering :: Rule (Context a) -> Rule (Context a) -> Ordering
     -- | A navigator is needed for traversing the expression and for using the
     -- traversal strategy combinators. By default, an exercise has no
     -- navigator.
   , navigation :: a -> ContextNavigator a
     -- | A finite list of examples, each with an assigned difficulty.
   , examples :: Examples a
     -- | A generator for random exercises of a certain difficulty.
   , randomExercise :: Maybe (StdGen -> Maybe Difficulty -> a)
     -- | An exercise generator for testing purposes (including corner cases).
   , testGenerator  :: Maybe (Gen a)
     -- | Conversion to and from the (generic) 'Term' datatype. Needed for
     -- representing the expression in the OpenMath standard.
   , hasTermView :: Maybe (View Term a)
     -- | Representation of the type of expression: this provides a back door
     -- for exercise-specific functionality.
   , hasTypeable :: Maybe (IsTypeable a)
     -- | Extra exercise-specific properties, not used by the default
     -- feedback services.
   , properties :: M.Map String Dynamic -- extra, domain-specific properties
   }

instance Eq (Exercise a) where
   e1 == e2 = getId e1 == getId e2

instance Ord (Exercise a) where
   compare = comparing getId

instance Apply Exercise where
   applyAll ex = mapMaybe fromContext . applyAll (strategy ex) . inContext ex

instance HasId (Exercise a) where
   getId = exerciseId
   changeId f ex = ex { exerciseId = f (exerciseId ex) }

-- | The 'emptyExercise' constructor function provides sensible defaults for
-- all fields of the 'Exercise' record.
emptyExercise :: Exercise a
emptyExercise = NewExercise
   { -- identification and meta-information
     exerciseId     = mempty
   , status         = Experimental
     -- parsing and pretty-printing
   , parser         = const (Left "<<no parser>>")
   , prettyPrinter  = const "<<no pretty-printer>>"
     -- syntactic and semantic checks
   , equivalence    = \_ _ -> True
   , similarity     = \_ _ -> True
   , ready          = true
   , suitable       = true
   , hasTermView    = Nothing
   , hasTypeable    = Nothing
   , properties     = M.empty
     -- strategies and rules
   , strategy       = label "Fail" S.fail
   , navigation     = noNavigator
   , canBeRestarted = True
   , extraRules     = []
   , ruleOrdering   = compareId
     -- testing and exercise generation
   , testGenerator  = Nothing
   , randomExercise = Nothing
   , examples       = []
   }

-- | In addition to the defaults of 'emptyExercise', this constructor sets
-- the fields 'prettyPrinter', 'similarity', and 'hasTermView'.
makeExercise :: (Show a, Eq a, IsTerm a) => Exercise a
makeExercise = emptyExercise
   { prettyPrinter = show
   , similarity    = (==)
   , hasTermView   = Just termView
   }

-----------------------------------------------------------------------------
-- Convenience functions

-- | Pretty print a value in its context.
prettyPrinterContext :: Exercise a -> Context a -> String
prettyPrinterContext ex =
   maybe "<<invalid term>>" (prettyPrinter ex) . fromContext

-- | Checks if an expression is in a solved form.
isReady :: Exercise a -> a -> Bool
isReady = evalPredicate . ready

-- | Checks if the expression is suitable and can be solved by the strategy.
isSuitable :: Exercise a -> a -> Bool
isSuitable = evalPredicate . suitable

-- | Returns a sorted list of rules, without duplicates.
ruleset :: Exercise a -> [Rule (Context a)]
ruleset ex = nub (sortBy compareId list)
 where
   list = extraRules ex ++ rulesInStrategy (strategy ex)

-- | Finds a rule of an exercise based on its identifier.
getRule :: Monad m => Exercise a -> Id -> m (Rule (Context a))
getRule ex a =
   case filter ((a ==) . getId) (ruleset ex) of
      [hd] -> return hd
      []   -> fail $ "Could not find ruleid " ++ showId a
      _    -> fail $ "Ambiguous ruleid " ++ showId a

-- | Makes a rule ordering based on a list of values with identifiers (e.g.,
-- a list of rules). Rules with identifiers that are not in the list are
-- considered after the rules in the list, and are sorted based on their
-- identifier.
ruleOrderingWith :: HasId b => [b] -> Rule a -> Rule a -> Ordering
ruleOrderingWith bs r1 r2 =
   let xs = map getId bs in
   case (elemIndex (getId r1) xs, elemIndex (getId r2) xs) of
      (Just i,  Just j ) -> i `compare` j
      (Just _,  Nothing) -> LT
      (Nothing, Just _ ) -> GT
      (Nothing, Nothing) -> compareId r1 r2

-----------------------------------------------------------------------------
-- Status

-- | The status of an exercise class.
data Status
   = Stable       -- ^ A released exercise that has undergone some thorough testing
   | Provisional  -- ^ A released exercise, possibly with some deficiencies
   | Alpha        -- ^ An exercise that is under development
   | Experimental -- ^ An exercise for experimentation purposes only
   deriving (Show, Eq)

-- | An exercise with the status 'Stable' or 'Provisional'
isPublic :: Exercise a -> Bool
isPublic ex = status ex `elem` [Stable, Provisional]

-- | An exercise that is not public
isPrivate :: Exercise a -> Bool
isPrivate = not . isPublic

-----------------------------------------------------------------------------
-- Examples

type Examples a = [(Difficulty, a)]

data Difficulty = VeryEasy | Easy | Medium | Difficult | VeryDifficult
   deriving (Eq, Ord, Enum, Data, Typeable)

instance Show Difficulty where
   show = (xs !!) . fromEnum
    where
      xs = ["very_easy", "easy", "medium", "difficult", "very_difficult"]

instance Read Difficulty where
   readsPrec _ s =
      case concatMap f txt of
         "veryeasy"      -> [(VeryEasy, xs)]
         "easy"          -> [(Easy, xs)]
         "medium"        -> [(Medium, xs)]
         "difficult"     -> [(Difficult, xs)]
         "verydifficult" -> [(VeryDifficult, xs)]
         _               -> []
    where
      (txt, xs) = span p (dropWhile isSpace s)
      p c   = isAlpha c || c `elem` "_-"
      f c   = [toLower c | c `notElem` "_-"]

-- | Parser for difficulty levels, which ignores non-alpha charactes (including
-- spaces) and upper/lower case distinction.
readDifficulty :: String -> Maybe Difficulty
readDifficulty s =
   case filter p [VeryEasy .. VeryDifficult] of
            [a] -> Just a
            _   -> Nothing
 where
   normal = filter isAlpha . map toLower
   p = (== normal s) . normal . show

-- | Assigns a difficulty level to a list of expressions.
level :: Difficulty -> [a] -> Examples a
level = zip . repeat

mapExamples :: (a -> b) -> Examples a -> Examples b
mapExamples f = map (second f)

-- | Returns the examples of an exercise class lifted to a context.
examplesContext :: Exercise a -> Examples (Context a)
examplesContext ex = mapExamples (inContext ex) (examples ex)

-----------------------------------------------------------------------------
-- Context

-- | Puts a value into a context with an empty environment.
inContext :: Exercise a -> a -> Context a
inContext ex = newContext . navigation ex

-- | Function for defining equivalence or similarity without taking
-- the context into account.
withoutContext :: (a -> a -> Bool) -> Context a -> Context a -> Bool
withoutContext f a b = fromMaybe False (fromContextWith2 f a b)

-----------------------------------------------------------------------------
-- Type casting

data IsTypeable a = IT (forall b . Typeable b => a -> Maybe b)
                       (forall b . Typeable b => b -> Maybe a)

-- | Encapsulates a type representation (use for 'hasTypeable' field).
useTypeable :: Typeable a => Maybe (IsTypeable a)
useTypeable = Just (IT cast cast)

-- | Cast from polymorphic type (to exercise-specific type).
-- This only works if 'hasTypeable' contains the right type representation.
castFrom :: Typeable b => Exercise a -> a -> Maybe b
castFrom ex a = do
   IT f _ <- hasTypeable ex
   f a

-- | Cast to polymorphic type (from exercise-specific type).
-- This only works if 'hasTypeable' contains the right type representation.
castTo :: Typeable b => Exercise a -> b -> Maybe a
castTo ex a = do
   IT _ g <- hasTypeable ex
   g a

-----------------------------------------------------------------------------
-- Exercise-specific properties

-- | Set an exercise-specific property (with a dynamic type)
setProperty :: Typeable val => String -> val -> Exercise a -> Exercise a
setProperty key a ex =
   ex { properties = M.insert key (toDyn a) (properties ex) }

-- | Get an exercise-specific property (of a dynamic type)
getProperty :: Typeable val => String -> Exercise a -> Maybe val
getProperty key ex = M.lookup key (properties ex) >>= fromDynamic

---------------------------------------------------------------
-- Random generators

-- | Makes a random exercise generator from a QuickCheck generator; the exercise
-- generator ignores the difficulty level. See the 'randomExercise' field.
simpleGenerator :: Gen a -> Maybe (QCGen -> Maybe Difficulty -> a)
simpleGenerator = useGenerator . const

-- | Makes a random exercise generator based on a QuickCheck generator for a
-- particular difficulty level. See the 'randomExercise' field.
useGenerator :: (Maybe Difficulty -> Gen a) -> Maybe (QCGen -> Maybe Difficulty -> a)
useGenerator makeGen = Just (\rng -> rec rng . makeGen)
 where
   rec rng (MkGen f) = a
    where
      (size, r) = randomR (0, 100) rng
      a         = f r size

-- | Returns a random exercise of a certain difficulty with some random
-- number generator. The field 'randomExercise' is used; if this is not
-- defined (i.e., Nothing), one of the examples is used instead.
randomTerm :: StdGen -> Exercise a -> Maybe Difficulty -> Maybe a
randomTerm rng ex mdif =
   case randomExercise ex of
      Just f  -> return (f rng mdif)
      Nothing
         | null xs   -> Nothing
         | otherwise -> Just (snd (xs !! i))
       where
         xs = filter p (examples ex)
         p (d, _) = maybe True (==d) mdif
         i = fst (randomR (0, length xs - 1) rng)

-- | Returns a list of randomly generated terms of a certain difficulty.
randomTerms :: StdGen -> Exercise a -> Maybe Difficulty -> [a]
randomTerms rng ex mdif = rec rng
 where
   rec a = maybe id (:) (randomTerm a ex mdif) (rec (snd (next a)))

---------------------------------------------------------------
-- Derivations

-- | Shows the default derivation for a given start term. The specified rule ordering
-- is used for selection.
showDerivation :: Exercise a -> a -> String
showDerivation ex a =
   case defaultDerivation ex a of
      Just d  -> showThisDerivation d ex
      Nothing -> "no derivation"

-- | Shows all derivations for a given start term. Warning: there can be many
-- derivations.
showDerivations :: Exercise a -> a -> String
showDerivations ex a = unlines
   [ "Derivation #" ++ show i ++ "\n" ++ showThisDerivation d ex
   | (i, d) <- zip [1::Int ..] (allDerivations ex a)
   ]

-- | Prints the default derivation for a given start term. The specified rule ordering
-- is used for selection.
printDerivation :: Exercise a -> a -> IO ()
printDerivation ex = putStrLn . showDerivation ex

-- | Prints all derivations for a given start term. Warning: there can be many
-- derivations.
printDerivations :: Exercise a -> a -> IO ()
printDerivations ex = putStrLn . showDerivations ex

-- also pass derivation as an argument
showThisDerivation :: Derivation (Rule b, Environment) (Context a) -> Exercise a -> String
showThisDerivation d ex = show (present der) ++ extra
 where
   der   = diffEnvironment d
   extra =
      case fromContext (lastTerm der) of
         Nothing               -> "<<invalid term>>"
         Just b | isReady ex b -> ""
                | otherwise    -> "<<not ready>>"
   present = biMap (ShowString . f) (ShowString . prettyPrinterContext ex)
   f ((r, local), global) = showId r ++ part local ++ part global
    where
      newl = "\n      "
      part env | noBindings env = ""
               | otherwise      = newl ++ show env

-- | Adds the difference of the environments in a derivation to the steps.
-- Bindings with identifier @location@ are ignored. This utility function is
-- useful for printing derivations.
diffEnvironment :: HasEnvironment a => Derivation s a -> Derivation (s, Environment) a
diffEnvironment = updateSteps $ \old a new ->
   let keep x = not (getId x == newId "location" || x `elem` list)
       list = bindings old
   in (a, makeEnvironment $ filter keep $ bindings new)

defaultDerivation :: Exercise a -> a -> Maybe (Derivation (Rule (Context a), Environment) (Context a))
defaultDerivation ex = listToMaybe . allDerivations ex

allDerivations :: Exercise a -> a -> [Derivation (Rule (Context a), Environment) (Context a)]
allDerivations ex =
   derivationList (ruleOrdering ex) (strategy ex) . inContext ex
