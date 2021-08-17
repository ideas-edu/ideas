-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Service.ServiceList (serviceList, metaServiceList) where

import Ideas.Common.ExerciseTests
import Ideas.Common.Library hiding (apply, applicable, suitable, ready)
import Ideas.Service.BasicServices
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackText
import Ideas.Service.ProblemDecomposition (problemDecomposition)
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Utils.TestSuite hiding (Message)
import qualified Ideas.Service.Diagnose as Diagnose
import qualified Ideas.Service.Apply as Apply
import qualified Ideas.Service.ProblemDecomposition as ProblemDecomposition
import qualified Ideas.Service.Submit as Submit

------------------------------------------------------
-- Querying a service

serviceList :: [Service]
serviceList =
   -- basic services
   [ solutionS, derivationS, allfirstsS, onefirstS, onefinalS
   , equivalenceS, similarityS, suitableS, finishedS, readyS
   , stepsremainingS, allapplicationsS
   , applyS, generateS, createS, applicableS
   , examplesS, exampleS, submitS, diagnoseS, diagnoseStringS
   , findbuggyrulesS, problemdecompositionS
   -- textual services
   , onefirsttextS, submittextS
   , derivationtextS, feedbacktextS, microstepsS
   ]

metaServiceList :: DomainReasoner -> [Service]
metaServiceList dr =
   [ indexS dr, servicelistS dr, serviceinfoS dr, exerciselistS dr
   , rulelistS, ruleinfoS, rulesinfoS, strategyinfoS, exerciseinfoS
   , constraintlistS, stateinfoS, examplederivationsS, testreportS, logS
   ]

------------------------------------------------------
-- Basic services

solutionS :: Service
solutionS = makeService "basic.solution"
   "Returns one possible worked-out solution starting with the \
   \current expression. The first optional argument lets you configure the \
   \strategy, i.e., make some minor modifications to it. Rules used and \
   \intermediate expressions are returned in a list." $
   solution ::: tMaybe tStrategyCfg .-> tState .-> tError (tDerivation tStepInfo tContext)

derivationS :: Service
derivationS = deprecate $ makeService "basic.derivation"
   "See 'solution' service." $
   serviceFunction solutionS

allfirstsS :: Service
allfirstsS = makeService "basic.allfirsts"
   "Returns all next steps that are suggested by the strategy. See the \
   \onefirst service to get only one suggestion. For each suggestion, a new \
   \state, the rule used, and the location where the rule was applied are \
   \returned." $
   allfirsts ::: tState .-> tError (tList (Tag "first" (tPair tStepInfo tState)))

onefirstS :: Service
onefirstS = makeService "basic.onefirst"
   "Returns a possible next step according to the strategy. Use the allfirsts \
   \service to get all possible steps that are allowed by the strategy. In \
   \addition to a new state, the rule used and the location where to apply \
   \this rule are returned." $
   onefirst ::: tState .-> tString :|: Tag "first" (tPair tStepInfo tState)
   -- special tag for (legacy) xml encoding

onefinalS :: Service
onefinalS = makeService "basic.onefinal"
   "Returns a final term, after taking zero or more steps, by applying the strategy." $
   onefinal ::: tState .-> tError tContext

equivalenceS :: Service
equivalenceS = makeService "basic.equivalence"
   "Tests whether two terms are semantically equivalent." $
   equivalence ::: tExercise .-> tContext .-> tContext .-> tBool

similarityS :: Service
similarityS = makeService "basic.similarity"
   "Tests whether two terms are (nearly) the same." $
   similarity ::: tExercise .-> tContext .-> tContext .-> tBool

suitableS :: Service
suitableS = makeService "basic.suitable"
   "Identifies which terms can be solved by the strategy." $
   suitable ::: tState .-> tBool

finishedS :: Service
finishedS = makeService "basic.finished"
   "Checks whether a term is in solved form." $
   finished ::: tState .-> tBool

readyS :: Service
readyS = deprecate $ makeService "basic.ready"
   "See 'finished' service." $
   serviceFunction finishedS

stepsremainingS :: Service
stepsremainingS = makeService "basic.stepsremaining"
   "Computes how many steps are remaining to be done, according to the \
   \strategy. For this, only the first derivation is considered, which \
   \corresponds to the one returned by the derivation service." $
   stepsremaining ::: tState .-> tError tInt

applicableS :: Service
applicableS = deprecate $ makeService "basic.applicable"
   "Given a current expression and a location in this expression, this service \
   \yields all rules that can be applied at this location, regardless of the \
   \strategy." $
   applicable ::: tLocation .-> tState .-> tList tRule

allapplicationsS :: Service
allapplicationsS = makeService "basic.allapplications"
   "Given a current expression, this service yields all rules that can be \
   \applied at a certain location, regardless wether the rule used is buggy \
   \or not. Some results are within the strategy, others are not." $
   allapplications ::: tState .-> tList (tTuple3 tRule tLocation tState)

applyS :: Service
applyS = makeService "basic.apply"
   "Apply a rule at a certain location to the current expression. If this rule \
   \was not expected by the strategy, we deviate from it. If the rule cannot \
   \be applied, this service call results in an error." $
   Apply.apply ::: tRule .-> tLocation .-> tEnvironment .-> tState .-> Apply.tApplyResult

generateS :: Service
generateS = makeService "basic.generate"
   "Given an exercise code and a difficulty level (optional), this service \
   \returns an initial state with a freshly generated expression." $
   generate ::: tQCGen .-> tExercise .-> tMaybe tDifficulty .-> tMaybe tUserId .-> tError tState

createS :: Service
createS = makeService "basic.create"
    "Given an expression, this service \
    \returns an initial state with the original given expression." $
    create ::: tQCGen .-> tExercise .-> tContext .-> tMaybe tUserId .-> tError tState

examplesS :: Service
examplesS = makeService "basic.examples"
   "This services returns a list of example expresssions that can be solved \
   \with an exercise. These are the examples that appear at the page generated \
   \for each exercise. Also see the generate service, which returns a random \
   \start term." $
   examplesContext ::: tExercise .-> tExamples

exampleS :: Service
exampleS = makeService "basic.example"
   "This services returns a specific (numbered) example expresssion that can be solved \
   \with an exercise. These are the examples that appear at the page generated \
   \for each exercise. Also see the generate service, which returns a random \
   \start term." $
   f ::: tQCGen .-> tExercise .-> Tag "nr" tInt .-> tMaybe tUserId .-> tError tState
 where
   f rng ex nr userId =
      case drop nr (examplesAsList ex) of
         []  -> Left "No such example"
         a:_ -> Right $ startState rng ex userId a

findbuggyrulesS :: Service
findbuggyrulesS = makeService "basic.findbuggyrules"
   "Search for common misconceptions (buggy rules) in an expression (compared \
   \to the current state). It is assumed that the expression is indeed not \
   \correct. This service has been superseded by the diagnose service." $
   findbuggyrules ::: tState .-> tContext .-> tList (tTuple3 tRule tLocation tEnvironment)

submitS :: Service
submitS = deprecate $ makeService "basic.submit"
   "Analyze an expression submitted by a student. Possible answers are Buggy, \
   \NotEquivalent, Ok, Detour, and Unknown. This service has been superseded \
   \by the diagnose service." $
   Submit.submit ::: tState .-> tContext .-> Submit.tResult

diagnoseS :: Service
diagnoseS = makeService "basic.diagnose"
   "Diagnose an expression submitted by a student. Possible diagnosis are \
   \Buggy (a common misconception was detected), NotEquivalent (something is \
   \wrong, but we don't know what), Similar (the expression is pretty similar \
   \to the last expression in the derivation), Expected (the submitted \
   \expression was anticipated by the strategy), Detour (the submitted \
   \expression was not expected by the strategy, but the applied rule was \
   \detected), and Correct (it is correct, but we don't know which rule was \
   \applied)." $
   Diagnose.diagnose ::: tState .-> tContext .-> tMaybe tId .-> Diagnose.tDiagnosis

diagnoseStringS :: Service
diagnoseStringS = makeService "basic.diagnose-string"
   "See diagnose service, but also returns a SyntaxError for invalid input." $
   diagnoseString ::: tState .-> Tag "term" tString .-> tMaybe tId .-> Diagnose.tDiagnosis

diagnoseString :: State a -> String -> Maybe Id -> Diagnose.Diagnosis a
diagnoseString st s mot =
   case parser ex s of
      Left msg -> Diagnose.SyntaxError msg
      Right ca -> Diagnose.diagnose st (inContext ex ca) mot
 where
   ex = exercise st

------------------------------------------------------
-- Problem decomposition service

problemdecompositionS :: Service
problemdecompositionS = makeService "basic.problemdecomposition"
   "Strategy service developed for the SURF project Intelligent Feedback for a \
   \binding with the MathDox system on linear algebra exercises. This is a \
   \composite service, and available for backwards compatibility." $
   problemDecomposition ::: tMaybe tId .-> tState .-> tMaybe ProblemDecomposition.tAnswer .-> tError ProblemDecomposition.tReply

------------------------------------------------------
-- Services with a feedback component

onefirsttextS :: Service
onefirsttextS = makeService "textual.onefirsttext"
   "Similar to the onefirst service, except that the result is now returned as \
   \a formatted text message. The optional string is for announcing the event \
   \leading to this service call (which can influence the returned result)." $
   onefirsttext ::: tScript .-> tState .-> tMaybe tString .-> tPair tMessage (tMaybe tState)

derivationtextS :: Service
derivationtextS = makeService "textual.derivationtext"
   "Similar to the derivation service, but the rules appearing in the derivation \
   \have been replaced by a short description of the rule." $
   derivationtext ::: tScript .-> tState .-> tError (tDerivation tString tContext)

submittextS :: Service
submittextS = deprecate $ makeService "textual.submittext"
   "Similar to the submit service, except that the result is now returned as \
   \a formatted text message. The expression 'submitted' by the student is sent \
   \in plain text (and parsed by the exercise's parser). \
   \The boolean in the \
   \result specifies whether the submitted term is accepted and incorporated \
   \in the new state." $
   submittext ::: tScript .-> tState .-> tString .-> tPair tMessage tState

feedbacktextS :: Service
feedbacktextS = makeService "textual.feedbacktext"
   "Textual feedback for diagnose service. Experimental." $
   feedbacktext ::: tScript .-> tState .-> tContext .-> tMaybe tId .-> tPair tMessage tState

------------------------------------------------------
-- Reflective services

indexS :: DomainReasoner -> Service
indexS dr = makeService "meta.index"
   "Index of the domain reasoner" $
   dr ::: tDomainReasoner

exerciselistS :: DomainReasoner -> Service
exerciselistS dr = makeService "meta.exerciselist"
   "Returns all exercises known to the system. For each exercise, its domain, \
   \identifier, a short description, and its current status are returned." $
   exercisesSorted dr ::: tList tSomeExercise

servicelistS :: DomainReasoner -> Service
servicelistS dr = makeService "meta.servicelist"
   "List of all supported feedback services" $
   servicesSorted dr ::: tList tService

serviceinfoS :: DomainReasoner -> Service
serviceinfoS dr = makeService "meta.serviceinfo"
   "Information about a feedback service" $
   findService dr ::: tId .-> tError tService

rulelistS :: Service
rulelistS = makeService "meta.rulelist"
   "Returns all rules of a particular exercise. For each rule, we return its \
   \name (or identifier), whether the rule is buggy, and whether the rule was \
   \expressed as an observable rewrite rule. See rulesinfo for more details \
   \about the rules." $
   ruleset ::: tExercise .-> tList tRule

ruleinfoS :: Service
ruleinfoS = makeService "meta.ruleinfo"
   "Information about a rule" $
   id ::: tRule .-> tRule

rulesinfoS :: Service
rulesinfoS = makeService "meta.rulesinfo"
   "Returns a list of all rules of a particular exercise, with many details \
   \including Formal Mathematical Properties (FMPs) and example applications." $
   () ::: Tag "RulesInfo" Unit

strategyinfoS :: Service
strategyinfoS = makeService "meta.strategyinfo"
   "Returns the representation of the strategy of a particular exercise." $
   (toStrategy . strategy) ::: tExercise .-> tStrategy

exerciseinfoS :: Service
exerciseinfoS = makeService "meta.exerciseinfo"
   "Exercise information" $
   id ::: tExercise .-> tExercise

constraintlistS :: Service
constraintlistS = makeService "meta.constraintlist"
   "Returns list of constraints" $
   constraints ::: tExercise .-> tList tConstraint

stateinfoS :: Service
stateinfoS = makeService "meta.stateinfo"
   "State information" $
   id ::: tState .-> tState

microstepsS :: Service
microstepsS = makeService "meta.microsteps" "Next (minor) steps." $
   (map f . microsteps) ::: tState .-> tList (Tag "first" (tPair tStepInfo tState))
 where
   f ((s, ctx, env), st) = ((s, location ctx, env), st)

examplederivationsS :: Service
examplederivationsS = makeService "meta.examplederivations"
   "Show example derivations" $
   exampleDerivations ::: tExercise .-> tError (tList (tDerivation tStepInfo tContext))

testreportS :: Service
testreportS = makeService "meta.testreport"
   "Show test report for an exercise." $
   (\qcgen -> runTestSuiteResult False . exerciseTestSuite qcgen) ::: tQCGen .-> tExercise .-> tIO tTestSuiteResult

logS :: Service
logS = makeService "meta.log"
   "Feedback service for logging events: the reply is always empty. The \
   \optional input state can be used to record userid, sessionid, and \
   \taskid."
   (const () ::: tMaybe tState .-> tUnit)