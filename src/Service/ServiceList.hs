{-# OPTIONS -XExistentialQuantification #-}
module Service.ServiceList (Service, getService, evalService) where

import Common.Context
import Common.Transformation
import qualified Common.Exercise as E
import Common.Utils (Some(..))
import Common.Exercise hiding (Exercise)
import Control.Monad.Error
import qualified Service.ExerciseList as List
import Service.TypedAbstractService hiding (exercise, State)
import Service.FeedbackText
import Service.Types 
import Data.List (sortBy)

data Service a = Service 
   { serviceName  :: String
   , typedService :: TypedService a
   }

data TypedService a = forall t . TypedService t (Type a t)

makeService :: String -> t -> Type a t -> Service a
makeService n f tp = Service
   { serviceName  = n
   , typedService = TypedService f tp
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
evalService f srv s = 
   case typedService srv of
      TypedService tv tp ->
         eval f tp tv s
   
------------------------------------------------------
-- Basic services

derivationS :: Service a
derivationS = makeService "derivation" derivation $
   State :-> List (Pair Rule Term)

allfirstsS :: Service a
allfirstsS = makeService "allfirsts" allfirsts $ 
   State :-> List (Triple Rule Location State)
        
onefirstS :: Service a
onefirstS = makeService "onefirst" onefirst $ 
   State :-> Elem (Triple Rule Location State)
  
readyS :: Service a
readyS = makeService "ready" ready $ 
   State :-> Bool

stepsremainingS :: Service a
stepsremainingS = makeService "stepsremaining" stepsremaining $
   State :-> Int

applicableS :: Service a
applicableS = makeService "applicable" applicable $  
   Location :-> State :-> List Rule

applyS :: Service a
applyS = makeService "apply" apply $ 
   Rule :-> Location :-> State :-> State

generateS :: Service a
generateS = makeService "generate" (flip generate 5) $ 
   Exercise :-> IO State

submitS :: Service a
submitS = makeService "submit" (\a -> submit a . fromContext) $
   State :-> Term :-> Result

------------------------------------------------------
-- Services with a feedback component

onefirsttextS :: Service a
onefirsttextS = makeService "onefirsttext" onefirsttext $ 
   State :-> Elem (Triple Bool String State)

submittextS :: Service a
submittextS = makeService "submittext" (\a -> submittext a . fromContext) $ 
   State :-> Term :-> Elem (Triple Bool String State)

derivationtextS :: Service a
derivationtextS = makeService "derivationtext" derivationtext $ 
   State :-> List (Pair String Term)
   
------------------------------------------------------
-- Reflective services
   
exerciselistS :: Service a
exerciselistS = makeService "exerciselist" allExercises $ 
   List (Pair (Pair String String) (Pair String String))

rulelistS :: Service a
rulelistS = makeService "rulelist" allRules $ 
   Exercise :-> List (Triple String Bool Bool)
      
allExercises :: [((String, String), (String, String))]
allExercises  = map make $ sortBy cmp List.exerciseList
 where
   cmp e1 e2  = f e1 `compare` f e2
   f (Some e) = (domain e, identifier e)
   make (Some ex) =
      ( (tag "domain"      (domain ex)
      , tag "identifier"  (identifier ex))
      , (tag "description" (description ex)
      , tag "status"      (show $ status ex))
      ) 

allRules :: E.Exercise a -> [(String, Bool, Bool)]
allRules = map make . ruleset
 where
   make r = 
      ( tag "name"        (name r)
      , tag "buggy"       (isBuggyRule r) 
      , tag "rewriterule" (isRewriteRule r)
      )
      
tag _ a = a