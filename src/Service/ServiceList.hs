{-# OPTIONS -XGADTs -XRank2Types #-}
module Service.ServiceList where

import Common.Transformation
import qualified Common.Transformation as Rule
import Common.Context
import Common.Utils (uncurry3)
import Common.Exercise
import Control.Monad.Error
import Service.TypedAbstractService hiding (exercise)

data Service a = Service 
   { serviceName  :: String
   , typedService :: TypedService a
   }

data TypedService a = forall arg res . TypedService (arg -> res) (ServiceType a arg) (ServiceType a res)

makeService :: String -> (a -> b) -> ServiceType s a -> ServiceType s b -> Service s
makeService n f t1 t2 = Service
   { serviceName  = n
   , typedService = TypedService f t1 t2
   }

execute :: Service a -> Converter s a -> s -> Either String s
execute (Service _ ts) conv = 
   case ts of 
      TypedService f t1 t2 ->
         liftM (fromType conv t2 . f) . toType conv t1

getService :: Monad m => String -> m (Service a)
getService txt =
   case filter ((==txt) . serviceName) serviceList of
      [hd] -> return hd
      []   -> fail $ "No service "   ++ txt
      _    -> fail $ "Ambiguous service " ++ txt

serviceList :: [Service a]
serviceList =
   [ derivationS
   , allfirstsS
   , onefirstS
   , readyS
   , stepsremainingS
   , applicableS
   , applyS
   , generateS
   , submitS
     --
   , onefirsttextS
   , submittextS
   , derivationtextS
     -- 
   ]

data Converter s a = Converter 
   { exercise :: Exercise a
   , toTerm   :: s -> Either String a
   , toType   :: forall t . ServiceType a t -> s -> Either String t
   , fromTerm :: a -> s
   , fromType :: forall t . ServiceType a t -> t -> s
   }

data ServiceType a t where
   StateType :: ServiceType a (State a)
   ListType  :: ServiceType a t -> ServiceType a [t]
   --VoidType  :: ServiceType a ()
   PairType  :: ServiceType a t1 -> ServiceType a t2 -> ServiceType a (t1, t2)
   TripleType :: ServiceType a t1 -> ServiceType a t2 -> ServiceType a t3 -> ServiceType a (t1, t2, t3)
   ExerciseType :: ServiceType a (Exercise a)
   RuleType  :: ServiceType a (Rule (Context a))
   BoolType  :: ServiceType a Bool
   IntType   :: ServiceType a Int
   StringType :: ServiceType a String
   TermType  :: ServiceType a (Context a)
   LocationType :: ServiceType a Location
   ElemType     :: ServiceType a t -> ServiceType a t -- quick fix
   IOType       :: ServiceType a t -> ServiceType a (IO t)
   ResultType   :: ServiceType a (Result a)
  
-- instance Show (ServiceType a t) 
     
derivationS :: Service a
derivationS = makeService "derivation" derivation StateType (ListType (PairType RuleType TermType))

allfirstsS :: Service a
allfirstsS = makeService "allfirsts" allfirsts StateType (ListType (TripleType RuleType LocationType StateType))
          
onefirstS :: Service a
onefirstS = makeService "onefirst" onefirst StateType (ElemType (TripleType RuleType LocationType StateType))

readyS :: Service a
readyS = makeService "ready" ready StateType BoolType

stepsremainingS :: Service a
stepsremainingS = makeService "stepsremaining" stepsremaining StateType IntType

applicableS :: Service a
applicableS = makeService "applicable" (uncurry applicable) (PairType LocationType StateType) (ListType RuleType)

applyS :: Service a
applyS = makeService "apply" (uncurry3 apply) (TripleType RuleType LocationType StateType) StateType

generateS :: Service a
generateS = makeService "generate" (flip generate 5) ExerciseType (IOType StateType)

submitS :: Service a
submitS = makeService "submit" f (PairType StateType TermType) ResultType
 where f (a, b) = submit a (fromContext b)

-------------------------------

onefirsttextS :: Service a
onefirsttextS = makeService "onefirsttext" onefirsttext StateType (ElemType (TripleType BoolType StringType StateType))

submittextS :: Service a
submittextS = makeService "submittext" f (PairType StateType TermType) (ElemType (TripleType BoolType StringType StateType))
 where f (a, b) = submittext a (fromContext b)

derivationtextS :: Service a
derivationtextS = makeService "derivationtext" derivationtext StateType (ListType (PairType StringType TermType))


-------------------------------
{-
exerciselistS :: Service
exerciselistS = Service "exerciselist" $ makeExecutable f VoidType (ListType ExerciseType)
 where f = undefined

 -}