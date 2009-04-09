module Service.ServiceList (Service, getService, evalService) where

import Common.Context
import Common.Transformation
import qualified Common.Exercise as E
import Common.Utils (Some(..))
import Common.Exercise hiding (Exercise)
import Control.Monad.Error
import qualified Service.ExerciseList as S
import qualified Service.TypedAbstractService as S
import Service.FeedbackText
import Service.Types 
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
   , onefirsttextS, submittextS, derivationtextS
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
   S.derivation ::: State :-> List (Pair Rule Term)

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
generateS = Service "generate" $ (flip S.generate 5) ::: 
   Exercise :-> IO State

submitS :: Service a
submitS = Service "submit" $ (\a -> S.submit a . fromContext) :::
   State :-> Term :-> Result

------------------------------------------------------
-- Services with a feedback component

onefirsttextS :: Service a
onefirsttextS = Service "onefirsttext" $ 
   onefirsttext ::: State :-> Elem (Triple Bool String State)

submittextS :: Service a
submittextS = Service "submittext" $ (\a -> submittext a . fromContext) :::
   State :-> Term :-> Elem (Triple Bool String State)

derivationtextS :: Service a
derivationtextS = Service "derivationtext" $ 
   derivationtext ::: State :-> List (Pair String Term)
   
------------------------------------------------------
-- Reflective services
   
exerciselistS :: Service a
exerciselistS = Service "exerciselist" $
   allExercises ::: List (Quadruple (Tag "domain" String) (Tag "identifier" String) (Tag "description" String) (Tag "status" String))

rulelistS :: Service a
rulelistS = Service "rulelist" $ 
   allRules ::: Exercise :-> List (Triple (Tag "name" String) (Tag "buggy" Bool) (Tag "rewriterule" Bool))
      
allExercises :: [(String, String, String, String)]
allExercises  = map make $ sortBy cmp S.exerciseList
 where
   cmp e1 e2  = f e1 `compare` f e2
   f (Some e) = (domain e, identifier e)
   make (Some ex) = (domain ex, identifier ex, description ex, show (status ex))

allRules :: E.Exercise a -> [(String, Bool, Bool)]
allRules = map make . ruleset
 where  make r = (name r, isBuggyRule r, isRewriteRule r)