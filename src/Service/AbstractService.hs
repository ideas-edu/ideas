{-# OPTIONS -XExistentialQuantification #-}
-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Service.AbstractService where

import Common.Utils (safeHead)
import Common.Context
import Common.Exercise (Exercise(..))
import Common.Transformation (name, Rule)
import qualified Service.TypedAbstractService as TAS
import Data.Char
import Data.Maybe
import Domain.Logic.Exercises
import Domain.Derivative.Exercises
import Domain.RelationAlgebra.Exercises
import Domain.Fraction.Exercises
import Common.Strategy  (makePrefix)

type ExerciseID = String
type RuleID     = String

type State      = (ExerciseID, Prefix, Expression, SimpleContext)
type Prefix     = String
type Expression = String -- concrete syntax (although this could also be abstract)
type SimpleContext = String

data Result = SyntaxError
            | Buggy [RuleID]   
            | NotEquivalent      
            | Ok [RuleID] State      -- equivalent
            | Detour [RuleID] State  -- equivalent
            | Unknown State          -- equivalent
          
generate :: ExerciseID -> Int -> IO State
generate exID level = 
   case getExercise exID of
      SE ex -> do
         s <- TAS.generate ex level
         return (toState s)

derivation :: State -> [(RuleID, Location, Expression)]
derivation s = 
   case fromState s of
      STS ts -> 
         let f (r, ca) = (name r, location ca, prettyPrinter (TAS.exercise ts) (fromContext ca))
         in map f (TAS.derivation ts)

allfirsts :: State -> [(RuleID, Location, State)]
allfirsts s = 
   case fromState s of
      STS ts -> 
         let f (r, loc, s) = (name r, loc, toState s)
         in map f (TAS.allfirsts ts)

onefirst :: State -> (RuleID, Location, State)
onefirst = fromMaybe (error "onefirst") . safeHead . allfirsts

applicable :: Location -> State -> [RuleID]
applicable loc s = 
   case fromState s of
      STS ts -> map name (TAS.applicable loc ts)

apply :: RuleID -> Location -> State -> State
apply ruleID loc s = 
   case fromState s of
      STS ts -> toState (TAS.apply (getRule ruleID (TAS.exercise ts)) loc ts)

ready :: State -> Bool
ready s = 
   case fromState s of
      STS ts -> TAS.ready ts

stepsremaining :: State -> Int
stepsremaining s = 
   case fromState s of
      STS ts -> TAS.stepsremaining ts

submit :: State -> Expression -> Result
submit s input = 
   case fromState s of
      STS ts -> 
         case parser (TAS.exercise ts) input of
            Left _  -> SyntaxError
            Right a ->
               case TAS.submit ts (inContext a) of
                  TAS.NotEquivalent -> NotEquivalent
                  TAS.Buggy   rs    -> Buggy   (map name rs)       
                  TAS.Ok      rs ns -> Ok      (map name rs) (toState ns)
                  TAS.Detour  rs ns -> Detour  (map name rs) (toState ns)
                  TAS.Unknown    ns -> Unknown               (toState ns)

-------------------------

data SomeExercise   = forall a . SE  (Exercise a)
data SomeTypedState = forall a . STS (TAS.State a)

exerciseList :: [SomeExercise]
exerciseList = 
   [ SE dnfExercise, SE derivativeExercise, SE cnfExercise, SE simplExercise ]
  
getExercise :: ExerciseID -> SomeExercise
getExercise exID = fromMaybe (error "invalid exercise ID") $ safeHead $ filter p exerciseList
 where p (SE ex) = shortTitle ex == exID

fromState :: State -> SomeTypedState
fromState (exID, p, ce, ctx) =
   case getExercise exID of
      SE ex -> 
         case (parser ex ce, parseContext ctx) of 
            (Right a, Just unit) -> STS TAS.State 
               { TAS.exercise = ex
               , TAS.prefix   = fmap (`makePrefix` strategy ex) (readPrefix p) 
               , TAS.term     = fmap (\_ -> a) unit
               }
            _ -> error "fromState"
      
toState :: TAS.State a -> State
toState state = ( shortTitle (TAS.exercise state)
                , maybe "NoPrefix" show (TAS.prefix state)
                , prettyPrinter (TAS.exercise state) (fromContext $ TAS.term state)
                , showContext (TAS.term state)
                )

readPrefix :: String -> Maybe [Int]
readPrefix input =
   case reads input of
      [(is, rest)] | all isSpace rest -> return is
      _ -> Nothing

getRule :: RuleID -> Exercise a -> Rule (Context a)
getRule ruleID ex = fromMaybe (error "invalid rule ID") $ safeHead $ 
   filter ((==ruleID) . name) (ruleset ex)