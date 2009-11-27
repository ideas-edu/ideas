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

import Common.Transformation
import qualified Common.Exercise as E
import Common.Utils (Some(..))
import Common.Exercise hiding (Exercise)
import Control.Monad.Error
import Service.ExerciseList (exercises)
import qualified Service.TypedAbstractService as S
import Service.FeedbackText hiding (ExerciseText)
import Service.Types 
import Service.ProblemDecomposition
import Data.List (sortBy)

data Service a = Service 
   { serviceName :: String
   , typedValue  :: TypedValue a
   }

------------------------------------------------------
-- Querying a service

serviceList :: [Service a]
serviceList =
   [ derivationS, allfirstsS, onefirstS, readyS
   , stepsremainingS, applicableS, applyS, generateS
   , submitS
   , onefirsttextS, findbuggyrulesS
   , submittextS, derivationtextS
   , problemdecompositionS
   , exerciselistS, rulelistS
   ]

getService :: Monad m => String -> m (Service a)
getService txt =
   case filter ((==txt) . serviceName) serviceList of
      [hd] -> return hd
      []   -> fail $ "No service " ++ txt
      _    -> fail $ "Ambiguous service " ++ txt

evalService :: Monad m => Evaluator m inp out a -> Service a -> inp -> m out
evalService f = eval f . typedValue
   
------------------------------------------------------
-- Basic services

derivationS :: Service a
derivationS = Service "derivation" $ 
   S.derivationNew ::: Maybe StrategyCfg :-> State :-> List (Tuple Rule Context)

allfirstsS :: Service a
allfirstsS = Service "allfirsts" $ 
   S.allfirsts ::: State :-> List (Triple Rule Location State)
        
onefirstS :: Service a
onefirstS = Service "onefirst" $ 
   S.onefirst ::: State :-> Elem (Triple Rule Location State)
  
readyS :: Service a
readyS = Service "ready" $ 
   S.ready ::: State :-> Bool

stepsremainingS :: Service a
stepsremainingS = Service "stepsremaining" $
   S.stepsremaining ::: State :-> Int

applicableS :: Service a
applicableS = Service "applicable" $ 
   S.applicable ::: Location :-> State :-> List Rule

applyS :: Service a
applyS = Service "apply" $ 
   S.apply ::: Rule :-> Location :-> State :-> State

generateS :: Service a
generateS = Service "generate" $ S.generate ::: 
   Exercise :-> Optional 5 Int :-> IO State

findbuggyrulesS :: Service a
findbuggyrulesS = Service "findbuggyrules" $ 
   S.findbuggyrules ::: State :-> Term :-> List Rule

submitS :: Service a
submitS = Service "submit" $ 
   S.submit ::: State :-> Term :-> Result

------------------------------------------------------
-- Services with a feedback component

onefirsttextS :: Service a
onefirsttextS = Service "onefirsttext" $ 
   onefirsttext ::: ExerciseText :-> State :-> Maybe String :-> Elem (Triple Bool String State)

submittextS :: Service a
submittextS = Service "submittext" $ 
   submittext ::: ExerciseText :-> State :-> String :-> Maybe String :-> Elem (Triple Bool String State)

derivationtextS :: Service a
derivationtextS = Service "derivationtext" $ 
   derivationtext ::: ExerciseText :-> State :-> Maybe String :-> List (Tuple String Context)

------------------------------------------------------
-- Problem decomposition service

problemdecompositionS :: Service a
problemdecompositionS = Service "problemdecomposition" $
   problemDecomposition ::: State :-> StrategyLoc :-> Maybe Term :-> DecompositionReply

------------------------------------------------------
-- Reflective services
   
exerciselistS :: Service a
exerciselistS = Service "exerciselist" $
   allExercises ::: List (Quadruple (Tag "domain" String) (Tag "identifier" String) (Tag "description" String) (Tag "status" String))

rulelistS :: Service a
rulelistS = Service "rulelist" $ 
   allRules ::: Exercise :-> List (Triple (Tag "name" String) (Tag "buggy" Bool) (Tag "rewriterule" Bool))
      
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