-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Service.ServiceList (serviceList, metaServiceList) where

import Ideas.Common.ExerciseTests
import Ideas.Common.Library hiding (apply, applicable, ready)
import Ideas.Common.Utils.TestSuite
import Ideas.Service.BasicServices
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackText
import Ideas.Service.ProblemDecomposition (problemDecomposition)
import Ideas.Service.State
import Ideas.Service.Types
import qualified Ideas.Service.Diagnose as Diagnose
import qualified Ideas.Service.Submit as Submit

------------------------------------------------------
-- Querying a service

serviceList :: [Service]
serviceList =
   -- basic services
   [ derivationS, allfirstsS, onefirstS, readyS
   , stepsremainingS, allapplicationsS
   , applyS, generateS, createS, applicableS
   , examplesS, submitS, diagnoseS
   , findbuggyrulesS, problemdecompositionS
   -- textual services
   , onefirsttextS, submittextS
   , derivationtextS, feedbacktextS
   ]

metaServiceList :: DomainReasoner -> [Service]
metaServiceList dr =
   [ indexS dr, servicelistS dr, serviceinfoS dr, exerciselistS dr
   , rulelistS, ruleinfoS, rulesinfoS, strategyinfoS, exerciseinfoS
   , stateinfoS, examplederivationsS, testreportS
   ]

------------------------------------------------------
-- Basic services

derivationS :: Service
derivationS = makeService "basic.derivation"
   "Returns one possible derivation (or: worked-out example) starting with the \
   \current expression. The first optional argument lets you configure the \
   \strategy, i.e., make some minor modifications to it. Rules used and \
   \intermediate expressions are returned in a list." $
   derivation ::: typed

allfirstsS :: Service
allfirstsS = makeService "basic.allfirsts"
   "Returns all next steps that are suggested by the strategy. See the \
   \onefirst service to get only one suggestion. For each suggestion, a new \
   \state, the rule used, and the location where the rule was applied are \
   \returned." $
   allfirsts ::: typed

onefirstS :: Service
onefirstS = makeService "basic.onefirst"
   "Returns a possible next step according to the strategy. Use the allfirsts \
   \service to get all possible steps that are allowed by the strategy. In \
   \addition to a new state, the rule used and the location where to apply \
   \this rule are returned." $
   onefirst ::: typed :-> Const String :|: Tag "elem" typed
   -- special tag for (legacy) xml encoding

readyS :: Service
readyS = makeService "basic.ready"
   "Test if the current expression is in a form accepted as a final answer. \
   \For this, the strategy is not used." $
   finished ::: typed

stepsremainingS :: Service
stepsremainingS = makeService "basic.stepsremaining"
   "Computes how many steps are remaining to be done, according to the \
   \strategy. For this, only the first derivation is considered, which \
   \corresponds to the one returned by the derivation Ideas.Service." $
   stepsremaining ::: typed

applicableS :: Service
applicableS = deprecate $ makeService "basic.applicable"
   "Given a current expression and a location in this expression, this service \
   \yields all rules that can be applied at this location, regardless of the \
   \strategy." $
   applicable ::: typed

allapplicationsS :: Service
allapplicationsS = makeService "basic.allapplications"
   "Given a current expression, this service yields all rules that can be \
   \applied at a certain location, regardless wether the rule used is buggy \
   \or not. Some results are within the strategy, others are not." $
   allapplications ::: typed

applyS :: Service
applyS = makeService "basic.apply"
   "Apply a rule at a certain location to the current expression. If this rule \
   \was not expected by the strategy, we deviate from it. If the rule cannot \
   \be applied, this service call results in an error." $
   apply ::: typed

generateS :: Service
generateS = makeService "basic.generate"
   "Given an exercise code and a difficulty level (optional), this service \
   \returns an initial state with a freshly generated expression." $
   generate ::: typed

createS :: Service
createS = makeService "basic.create"
    "Given an expression, this service \
    \returns an initial state with the original given expression." $
    create ::: typed

examplesS :: Service
examplesS = makeService "basic.examples"
   "This services returns a list of example expresssions that can be solved \
   \with an exercise. These are the examples that appear at the page generated \
   \for each exercise. Also see the generate service, which returns a random \
   \start term." $
   examplesContext ::: typed

findbuggyrulesS :: Service
findbuggyrulesS = makeService "basic.findbuggyrules"
   "Search for common misconceptions (buggy rules) in an expression (compared \
   \to the current state). It is assumed that the expression is indeed not \
   \correct. This service has been superseded by the diagnose service." $
   findbuggyrules ::: typed

submitS :: Service
submitS = deprecate $ makeService "basic.submit"
   "Analyze an expression submitted by a student. Possible answers are Buggy, \
   \NotEquivalent, Ok, Detour, and Unknown. This service has been superseded \
   \by the diagnose Ideas.Service." $
   Submit.submit ::: typed

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
   Diagnose.diagnose ::: typed

------------------------------------------------------
-- Problem decomposition service

problemdecompositionS :: Service
problemdecompositionS = makeService "basic.problemdecomposition"
   "Strategy service developed for the SURF project Intelligent Feedback for a \
   \binding with the MathDox system on linear algebra exercises. This is a \
   \composite service, and available for backwards compatibility." $
   problemDecomposition ::: typed

------------------------------------------------------
-- Services with a feedback component

onefirsttextS :: Service
onefirsttextS = makeService "textual.onefirsttext"
   "Similar to the onefirst service, except that the result is now returned as \
   \a formatted text message. The optional string is for announcing the event \
   \leading to this service call (which can influence the returned result)." $
   onefirsttext ::: typed

derivationtextS :: Service
derivationtextS = makeService "textual.derivationtext"
   "Similar to the derivation service, but the rules appearing in the derivation \
   \have been replaced by a short description of the rule." $
   derivationtext ::: typed

submittextS :: Service
submittextS = deprecate $ makeService "textual.submittext"
   "Similar to the submit service, except that the result is now returned as \
   \a formatted text message. The expression 'submitted' by the student is sent \
   \in plain text (and parsed by the exercise's parser). \
   \The boolean in the \
   \result specifies whether the submitted term is accepted and incorporated \
   \in the new state." $
   submittext ::: typed

feedbacktextS :: Service
feedbacktextS = makeService "textual.feedbacktext"
   "Textual feedback for diagnose Ideas.Service. Experimental." $
   feedbacktext ::: typed

------------------------------------------------------
-- Reflective services

indexS :: DomainReasoner -> Service
indexS dr = makeService "meta.index"
   "Index of the domain reasoner" $
   dr ::: typed

exerciselistS :: DomainReasoner -> Service
exerciselistS dr = makeService "meta.exerciselist"
   "Returns all exercises known to the system. For each exercise, its domain, \
   \identifier, a short description, and its current status are returned." $
   exercisesSorted dr ::: typed

servicelistS :: DomainReasoner -> Service
servicelistS dr = makeService "meta.servicelist"
   "List of all supported feedback services" $
   servicesSorted dr ::: typed

serviceinfoS :: DomainReasoner -> Service
serviceinfoS dr = makeService "meta.serviceinfo"
   "Information about a feedback service" $
   (findService dr :: Id -> Either String Service) ::: typed

rulelistS :: Service
rulelistS = makeService "meta.rulelist"
   "Returns all rules of a particular exercise. For each rule, we return its \
   \name (or identifier), whether the rule is buggy, and whether the rule was \
   \expressed as an observable rewrite rule. See rulesinfo for more details \
   \about the rules." $
   ruleset ::: typed

ruleinfoS :: Service
ruleinfoS = makeService "meta.ruleinfo"
   "Information about a rule" $
   (id :: Rule (Context a) -> Rule (Context a)) ::: typed

rulesinfoS :: Service
rulesinfoS = makeService "meta.rulesinfo"
   "Returns a list of all rules of a particular exercise, with many details \
   \including Formal Mathematical Properties (FMPs) and example applications." $
   () ::: Tag "RulesInfo" Unit

strategyinfoS :: Service
strategyinfoS = makeService "meta.strategyinfo"
   "Returns the representation of the strategy of a particular exercise." $
   (toStrategy . strategy) ::: typed

exerciseinfoS :: Service
exerciseinfoS = makeService "meta.exerciseinfo"
   "Exercise information" $
   (id :: Exercise a -> Exercise a) ::: typed

stateinfoS :: Service
stateinfoS = makeService "meta.stateinfo"
   "State information" $
   (id :: State a -> State a) ::: typed

examplederivationsS :: Service
examplederivationsS = makeService "meta.examplederivations"
   "Show example derivations" $ exampleDerivations ::: typed

testreportS :: Service
testreportS = makeService "meta.testreport"
   "Show test report for an exercise." $
   (\stdgen -> runTestSuiteResult False . exerciseTestSuite stdgen) ::: typed