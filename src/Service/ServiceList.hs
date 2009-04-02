{-# OPTIONS -XExistentialQuantification #-}
module Service.ServiceList (Service, getService, evalService) where

import Common.Context
import Control.Monad.Error
import Service.TypedAbstractService hiding (exercise, State)
import Service.Types

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

{-
data ResultType t where
   IntRes  :: ResultType Int
   (:->)   :: ResultType t1 -> ResultType t2 -> ResultType (t1 -> t2)
   Conv    :: (t2 -> t1) -> ResultType t1 -> ResultType t2
   PairRes :: ResultType t1 -> ResultType t2 -> ResultType (t1, t2)

data EvalType s = EvalType 
   { evalArgument :: forall t . ResultType t -> s -> (t, s)
   , evalResult   :: forall t . ResultType t -> t -> s -> s
   , evalAppend   :: s -> s -> s
   }

evalString :: EvalType String
evalString = EvalType 
   { evalArgument = fromSpec
   , evalResult   = runSpec
   , evalAppend   = \x y -> x ++ "/" ++ y
   }

triple t1 t2 t3 = Conv (\(a, b, c) -> (a, (b, c))) (PairRes t1 (PairRes t2 t3))

fromGen :: EvalType s -> ResultType t -> s -> (t, s)
fromGen f (PairRes t1 t2) = \s -> 
   let (a, s1) = fromGen f t1 s
       (b, s2) = fromGen f t2 s1 
   in ((a, b), s2)
fromGen f t = evalArgument f t
   
fromSpec :: ResultType t -> String -> (t, String)
fromSpec IntRes = \s -> case words s of
   hd:tl -> (read hd, unwords tl)
   [] -> error "" 

runGen :: EvalType s -> ResultType t -> t -> s -> s
runGen f (Conv fun t) a s = runGen f t (fun a) s
runGen f (t1 :-> t2) fun s = let (a, s1) = fromGen f t1 s in runGen f t2 (fun a) s1
runGen f (PairRes t1 t2) (a, b) s = evalAppend f (runGen f t1 a s) (runGen f t2 b s)
runGen f t a s = evalResult f t a s

runSpec :: ResultType t -> t -> String -> String
runSpec IntRes n _ = show n

test = runGen evalString ((:->) IntRes ((:->) (PairRes IntRes IntRes) (triple IntRes IntRes IntRes))) (\x (y, z) -> (x*y*z, max x y, min y z)) "3 4 5"





execute :: Service a -> Converter s a -> s -> Either String s
execute (Service _ ts) conv = 
   case ts of 
      TypedService f t1 t2 ->
         liftM (fromType conv t2 . f) . toType conv t1







  
{-
fromTypeDefault :: Monoid s => (forall t . ServiceType a t -> t -> Maybe s) -> ServiceType a t -> t -> s
fromTypeDefault f serviceType tv = 
   case f serviceType tv of
      Just s  -> s
      Nothing ->
         case serviceType of
            PairType t1 t2 -> 
               let (a, b) = tv
               in fromTypeDefault f t1 a `mappend` fromTypeDefault f t2 b
            TripleType t1 t2 t3 -> 
               let (a, b, c) = tv
               in mconcat [fromTypeDefault f t1 a, fromTypeDefault f t2 b, fromTypeDefault f t3 c]
            ElemType t1 ->
               fromTypeDefault f t1 tv
            IOType t1 -> 
               fromTypeDefault f t1 (unsafePerformIO tv)
            IntType  -> fromTypeDefault f StringType (show tv)
            BoolType -> fromTypeDefault f StringType (show tv)
            LocationType ->  fromTypeDefault f StringType (show tv)
            RuleType -> fromTypeDefault f StringType (name tv) -}
   

-- instance Show (ServiceType a t) 
     


-------------------------------


-}

-------------------------------
{-
exerciselistS :: Service
exerciselistS = Service "exerciselist" $ makeExecutable f VoidType (ListType ExerciseType)
 where f = undefined

 -}