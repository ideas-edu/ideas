-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.ServiceList (serviceList, Service(..), getService, evalService) where

import qualified Common.Exercise as E
import Common.Strategy (toStrategy)
import Common.Transformation
import Common.Utils (Some(..))
import Common.Exercise hiding (Exercise)
import Control.Monad.Error
import Service.ExerciseList (exercises)
import qualified Service.TypedAbstractService as S
import qualified Service.Submit as S
import qualified Service.Diagnose as S
import Service.FeedbackText hiding (ExerciseText)
import Service.Types 
import Service.RulesInfo
import Service.ProblemDecomposition
import Data.List (sortBy)

data Service a = Service 
   { serviceName        :: String
   , serviceDescription :: String
   , serviceDeprecated  :: Bool
   , serviceFunction    :: TypedValue a
   }

------------------------------------------------------
-- Querying a service

serviceList :: [Service a]
serviceList =
   [ derivationS, allfirstsS, onefirstS, readyS
   , stepsremainingS, applicableS, applyS, generateS
   , submitS, diagnoseS
   , onefirsttextS, findbuggyrulesS
   , submittextS, derivationtextS
   , problemdecompositionS
   , exerciselistS, rulelistS, rulesinfoS, strategyinfoS
   ]

makeService :: String -> String -> TypedValue a -> Service a
makeService name descr f = Service name descr False f

deprecate :: Service a -> Service a
deprecate s = s { serviceDeprecated = True }

getService :: Monad m => String -> m (Service a)
getService txt =
   case filter ((==txt) . serviceName) serviceList of
      [hd] -> return hd
      []   -> fail $ "No service " ++ txt
      _    -> fail $ "Ambiguous service " ++ txt

evalService :: Monad m => Evaluator m inp out a -> Service a -> inp -> m out
evalService f = eval f . serviceFunction
   
------------------------------------------------------
-- Basic services

derivationS :: Service a
derivationS = makeService "derivation" 
   "Returns one possible derivation (or: worked-out example) starting with the \
   \current expression. The first optional argument lets you configure the \
   \strategy, i.e., make some minor modifications to it. Rules used and \
   \intermediate expressions are returned in a list." $ 
   S.derivation ::: Maybe StrategyCfg :-> State :-> Error (List (tuple2 Rule Context))

allfirstsS :: Service a
allfirstsS = makeService "allfirsts" 
   "Returns all next steps that are suggested by the strategy. See the \
   \onefirst service to get only one suggestion. For each suggestion, a new \
   \state, the rule used, and the location where the rule was applied are \
   \returned." $ 
   S.allfirsts ::: State :-> Error (List (tuple3 Rule Location State))
        
onefirstS :: Service a
onefirstS = makeService "onefirst" 
   "Returns a possible next step according to the strategy. Use the allfirsts \
   \service to get all possible steps that are allowed by the strategy. In \
   \addition to a new state, the rule used and the location where to apply \
   \this rule are returned." $ 
   S.onefirst ::: State :-> Elem (Error (tuple3 Rule Location State))
  
readyS :: Service a
readyS = makeService "ready" 
   "Test if the current expression is in a form accepted as a final answer. \
   \For this, the strategy is not used." $ 
   S.ready ::: State :-> Bool

stepsremainingS :: Service a
stepsremainingS = makeService "stepsremaining" 
   "Computes how many steps are remaining to be done, according to the \
   \strategy. For this, only the first derivation is considered, which \
   \corresponds to the one returned by the derivation service." $
   S.stepsremaining ::: State :-> Error Int

applicableS :: Service a
applicableS = makeService "applicable" 
   "Given a current expression and a location in this expression, this service \
   \yields all rules that can be applied at this location, regardless of the \
   \strategy." $ 
   S.applicable ::: Location :-> State :-> List Rule

applyS :: Service a
applyS = makeService "apply" 
   "Apply a rule at a certain location to the current expression. If this rule \
   \was not expected by the strategy, we deviate from it. If the rule cannot \
   \be applied, this service call results in an error." $ 
   S.apply ::: Rule :-> Location :-> State :-> Error State

generateS :: Service a
generateS = makeService "generate" 
   "Given an exercise code and a difficulty level (optional), this service \
   \returns an initial state with a freshly generated expression. The meaning \
   \of the difficulty level (an integer) depends on the exercise at hand." $ 
   S.generate ::: Exercise :-> Optional 5 Int :-> IO State

findbuggyrulesS :: Service a
findbuggyrulesS = makeService "findbuggyrules" 
   "Search for common misconceptions (buggy rules) in an expression (compared \
   \to the current state). It is assumed that the expression is indeed not \
   \correct. This service has been superseded by the diagnose service." $ 
   S.findbuggyrules ::: State :-> Term :-> List Rule

submitS :: Service a
submitS = deprecate $ makeService "submit" 
   "Analyze an expression submitted by a student. Possible answers are Buggy, \
   \NotEquivalent, Ok, Detour, and Unknown. This service has been superseded \
   \by the diagnose service." $ 
   S.submit ::: State :-> Term :-> Result

diagnoseS :: Service a
diagnoseS = makeService "diagnose" 
   "Diagnose an expression submitted by a student. Possible diagnosis are \
   \Buggy (a common misconception was detected), NotEquivalent (something is \
   \wrong, but we don't know what), Similar (the expression is pretty similar \
   \to the last expression in the derivation), Expected (the submitted \
   \expression was anticipated by the strategy), Detour (the submitted \
   \expression was not expected by the strategy, but the applied rule was \
   \detected), and Correct (it is correct, but we don't know which rule was \
   \applied)." $
   S.diagnose ::: State :-> Term :-> Diagnosis

------------------------------------------------------
-- Services with a feedback component

onefirsttextS :: Service a
onefirsttextS = makeService "onefirsttext" 
   "Similar to the onefirst service, except that the result is now returned as \
   \a formatted text message. The optional string is for announcing the event \
   \leading to this service call (which can influence the returned result). \
   \The boolean in the result specifies whether a suggestion was available or \
   \not." $ 
   onefirsttext ::: ExerciseText :-> State :-> Maybe String :-> Elem (tuple3 Bool String State)

submittextS :: Service a
submittextS = makeService "submittext" 
   "Similar to the submit service, except that the result is now returned as \
   \a formatted text message. The expression 'submitted' by the student is sent \
   \in plain text (and parsed by the exercise's parser). The optional string is \
   \for announcing the event leading to this service call. The boolean in the \
   \result specifies whether the submitted term is accepted and incorporated \
   \in the new state." $ 
   submittext ::: ExerciseText :-> State :-> String :-> Maybe String :-> Elem (tuple3 Bool String State)

derivationtextS :: Service a
derivationtextS = makeService "derivationtext" 
   "Similar to the derivation service, but the rules appearing in the derivation \
   \have been replaced by a short description of the rule. The optional string is \
   \for announcing the event leading to this service call." $ 
   derivationtext ::: ExerciseText :-> State :-> Maybe String :-> Error (List (tuple2 String Context))

------------------------------------------------------
-- Problem decomposition service

problemdecompositionS :: Service a
problemdecompositionS = makeService "problemdecomposition" 
   "Strategy service developed for the SURF project Intelligent Feedback for a \
   \binding with the MathDox system on linear algebra exercises. This is a \
   \composite service, and available for backwards compatibility." $
   problemDecomposition ::: State :-> StrategyLoc :-> Maybe Term :-> DecompositionReply

------------------------------------------------------
-- Reflective services
   
exerciselistS :: Service a
exerciselistS = makeService "exerciselist" 
   "Returns all exercises known to the system. For each exercise, its domain, \
   \identifier, a short description, and its current status are returned." $
   allExercises ::: List (tuple4 (Tag "domain" String) (Tag "identifier" String) (Tag "description" String) (Tag "status" String))

rulelistS :: Service a
rulelistS = makeService "rulelist" 
   "Returns all rules of a particular exercise. For each rule, we return its \
   \name (or identifier), whether the rule is buggy, and whether the rule was \
   \expressed as an observable rewrite rule. See rulesinfo for more details \
   \about the rules." $ 
   allRules ::: Exercise :-> List (tuple3 (Tag "name" String) (Tag "buggy" Bool) (Tag "rewriterule" Bool))
      
rulesinfoS :: Service a
rulesinfoS = makeService "rulesinfo" 
   "Returns a list of all rules of a particular exercise, with many details \
   \including Formal Mathematical Properties (FMPs) and example applications." $
   mkRulesInfo ::: RulesInfo

strategyinfoS :: Service a
strategyinfoS = makeService "strategyinfo"
   "Returns the representation of the strategy of a particular exercise." $ 
   (toStrategy . strategy) ::: Exercise :-> Strategy 
   
allExercises :: [(String, String, String, String)]
allExercises  = map make $ sortBy cmp exercises
 where
   cmp e1 e2  = f e1 `compare` f e2
   f (Some ex) = exerciseCode ex
   make (Some ex) = 
      let code = exerciseCode ex 
      in (domain code, identifier code, description ex, show (status ex))

allRules :: E.Exercise a -> [(String, Bool, Bool)]
allRules = map make . ruleset
 where  
   make r  = (name r, isBuggyRule r, isRewriteRule r)