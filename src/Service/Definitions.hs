{-# LANGUAGE GADTs, Rank2Types #-}
-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.Definitions 
   ( -- * Services
     Service, makeService, deprecate 
   , serviceName, serviceDescription, serviceDeprecated, serviceFunction
     -- * Types
   , Type(..), TypedValue(..), tuple2, tuple3, tuple4
   , isSynonym
   , resultType, resultTypeSynonym
   , diagnosisType, diagnosisTypeSynonym
   , rulesInfoType
   , replyType, replyTypeSynonym
   ) where

import Common.Context (Context)
import Common.Exercise (Exercise)
import Common.Navigator (Location)
import Common.Transformation (Rule)
import Common.Strategy (Strategy, StrategyLocation, StrategyConfiguration)
import Common.Utils (commaList)
import Control.Monad
import Data.Maybe
import Service.TypedAbstractService (State)
import Service.FeedbackText (ExerciseText)
import qualified Service.Diagnose as Diagnose
import qualified Service.Submit as Submit
import qualified Service.ProblemDecomposition as Decomposition

-----------------------------------------------------------------------------
-- Services

data Service = Service 
   { serviceName        :: String
   , serviceDescription :: String
   , serviceDeprecated  :: Bool
   , serviceFunction    :: forall a . TypedValue a
   }
   
makeService :: String -> String -> (forall a . TypedValue a) -> Service
makeService name descr f = Service name descr False f

deprecate :: Service -> Service
deprecate s = s { serviceDeprecated = True }



equal :: Type a t1 -> Type a t2 -> Maybe (t1 -> t2)
equal (a :|: b) (c :|: d) = liftM2 (\f g -> either (Left . f) (Right . g)) (equal a c) (equal b d)
equal (List a) (List b) = liftM map (equal a b)
equal Rule Rule = Just id
equal Unit Unit = Just id
equal (Pair a b) (Pair c d) = liftM2 (\f g (x, y) -> (f x, g y)) (equal a c) (equal b d)
equal State State = Just id
equal Bool Bool = Just id
equal (Iso _ f a) b = fmap (. f) (equal a b)
equal a (Iso f _ b) = fmap (f .) (equal a b)
equal _ _ = Nothing

infixr 5 :|:

-----------------------------------------------------------------------------
-- Types

infix  2 :::
infixr 3 :->

data TypedValue a = forall t . t ::: Type a t

tuple2 :: Type a t1 -> Type a t2 -> Type a (t1, t2)
tuple2 = Pair

tuple3 :: Type a t1 -> Type a t2 -> Type a t3 -> Type a (t1, t2, t3)
tuple3 t1 t2 t3 = Iso f g (Pair t1 (Pair t2 t3)) 
 where
   f (a, (b, c)) = (a, b, c)
   g (a, b, c)   = (a, (b, c))
   
tuple4 :: Type a t1 -> Type a t2 -> Type a t3 -> Type a t4 -> Type a (t1, t2, t3, t4)
tuple4 t1 t2 t3 t4 = Iso f g (Pair t1 (Pair t2 (Pair t3 t4))) 
 where
   f (a, (b, (c, d))) = (a, b, c, d)
   g (a, b, c, d)     = (a, (b, (c, d)))

data Type a t where
   -- Type isomorphisms (for defining type synonyms)
   Iso          :: (t1 -> t2) -> (t2 -> t1) -> Type a t1 -> Type a t2
   -- Function type
   (:->)        :: Type a t1 -> Type a t2 -> Type a (t1 -> t2)
   -- Special annotations
   Tag          :: String -> Type a t1 -> Type a t1
   Optional     :: t1 -> Type a t1 -> Type a t1
   Maybe        :: Type a t1 -> Type a (Maybe t1)
   Error        :: Type a t -> Type a (Either String t)
   -- Type constructors
   List         :: Type a t -> Type a [t]
   Pair         :: Type a t1 -> Type a t2 -> Type a (t1, t2)
   (:|:)        :: Type a t1 -> Type a t2 -> Type a (Either t1 t2)
   Unit         :: Type a ()
   Elem         :: Type a t -> Type a t -- quick fix
   IO           :: Type a t -> Type a (IO t)
   -- Exercise-specific types
   State        :: Type a (State a)
   Exercise     :: Type a (Exercise a)
   Strategy     :: Type a (Strategy (Context a))
   ExerciseText :: Type a (ExerciseText a)
   Rule         :: Type a (Rule (Context a))
   Term         :: Type a a
   Context      :: Type a (Context a)
   Location     :: Type a Location
   StrategyLoc  :: Type a StrategyLocation
   StrategyCfg  :: Type a StrategyConfiguration
   -- Basic types
   Bool         :: Type a Bool
   Int          :: Type a Int
   String       :: Type a String

instance Show (Type a t) where
   show (Iso _ _ t)    = show t
   show (t1 :-> t2)    = show t1 ++ " -> " ++ show t2 
   show t@(Pair _ _)   = showTuple t
   show (t1 :|: t2)    = show t1 ++ " | " ++ show t2
   show (Tag s t)      = s ++ "@(" ++ show t ++ ")"
   show (Optional _ t) = "(" ++ show t ++ ")?"
   show (Maybe t)      = "(" ++ show t ++ ")?"
   show (Error t)      = show t
   show (List t)       = "[" ++ show t ++ "]"
   show (Elem t)       = show t
   show (IO t)         = show t
   show t              = fromMaybe "unknown" (groundType t)
   
showTuple :: Type a t -> String
showTuple t = "(" ++ commaList (collect t) ++ ")"
 where
   collect :: Type a t -> [String]
   collect (Pair t1 t2) = collect t1 ++ collect t2
   collect (Iso _ _ t)  = collect t
   collect t            = [show t]
   
groundType :: Type a t -> Maybe String
groundType tp =
   case tp of 
      State        -> Just "State"
      Exercise     -> Just "Exercise"
      Strategy     -> Just "Strategy"
      ExerciseText -> Just "ExerciseText"
      Rule         -> Just "Rule"
      Term         -> Just "Term"
      Context      -> Just "Context"
      Unit         -> Just "()"
      Bool         -> Just "Bool"
      Int          -> Just "Int"
      String       -> Just "String"
      Location     -> Just "Location"
      StrategyLoc  -> Just "StrategyLocation"
      StrategyCfg  -> Just "StrategyConfiguration"
      _            -> Nothing
      
-----------------------------------------------------------------------------
-- Type Synonyms

data TypeSynonym a t = TS 
   { synonymName :: String 
   , useSynonym  :: Type a t
   , isSynonym   :: Monad m => TypedValue a -> m t
   }
   
typeSynonym :: String -> (t2 -> t) -> (t -> t2) -> Type a t2 -> TypeSynonym a t
typeSynonym name to from tp = TS
   { synonymName = name
   , useSynonym  = Tag name (Iso to from tp)
   , isSynonym   = maybe (fail name) return . match
        
   }
 where
   match (a ::: t0) = do
      (s, t) <- isTag t0
      guard (s == name)
      f <- equal t tp
      return (to (f a))

isTag :: Type a t -> Maybe (String, Type a t)
isTag (Tag s t) = Just (s, t)
isTag _         = Nothing

resultType :: Type a (Submit.Result a)
resultType = useSynonym resultTypeSynonym

resultTypeSynonym :: TypeSynonym a (Submit.Result a)
resultTypeSynonym = typeSynonym "Result" to from tp
 where
   to (Left rs) = Submit.Buggy rs
   to (Right (Left ())) = Submit.NotEquivalent
   to (Right (Right (Left (rs, s)))) = Submit.Ok rs s
   to (Right (Right (Right (Left (rs, s))))) = Submit.Detour rs s
   to (Right (Right (Right (Right s)))) = Submit.Unknown s

   from (Submit.Buggy rs)      = Left rs
   from (Submit.NotEquivalent) = Right (Left ())
   from (Submit.Ok rs s)       = Right (Right (Left (rs, s)))
   from (Submit.Detour rs s)   = Right (Right (Right (Left (rs, s))))
   from (Submit.Unknown s)     = Right (Right (Right (Right s))) 

   tp  =  List Rule 
      :|: Unit
      :|: Pair (List Rule) State
      :|: Pair (List Rule) State
      :|: State

diagnosisType :: Type a (Diagnose.Diagnosis a)
diagnosisType = useSynonym diagnosisTypeSynonym

diagnosisTypeSynonym :: TypeSynonym a (Diagnose.Diagnosis a)
diagnosisTypeSynonym = typeSynonym "Diagnosis" to from tp
 where
   to (Left r) = Diagnose.Buggy r
   to (Right (Left ())) = Diagnose.NotEquivalent
   to (Right (Right (Left (b, s)))) = Diagnose.Similar b s
   to (Right (Right (Right (Left (b, s, r))))) = Diagnose.Expected b s r
   to (Right (Right (Right (Right (Left (b, s, r)))))) = Diagnose.Detour b s r
   to (Right (Right (Right (Right (Right (b, s)))))) = Diagnose.Correct b s
   
   from (Diagnose.Buggy r)        = Left r
   from (Diagnose.NotEquivalent)  = Right (Left ())
   from (Diagnose.Similar b s)    = Right (Right (Left (b, s)))
   from (Diagnose.Expected b s r) = Right (Right (Right (Left (b, s, r))))
   from (Diagnose.Detour b s r)   = Right (Right (Right (Right (Left (b, s, r)))))
   from (Diagnose.Correct b s)    = Right (Right (Right (Right (Right (b, s)))))
   
   tp  =  Rule
      :|: Unit
      :|: Pair Bool State
      :|: tuple3 Bool State Rule
      :|: tuple3 Bool State Rule
      :|: Pair Bool State
      
rulesInfoType :: Type a ()
rulesInfoType = useSynonym (typeSynonym "RulesInfo" id id Unit) 

replyType :: Type a (Decomposition.Reply a)
replyType = useSynonym replyTypeSynonym

replyTypeSynonym :: TypeSynonym a (Decomposition.Reply a)
replyTypeSynonym = typeSynonym "DecompositionReply" to from tp
 where
   to (Left (a, b, c, d)) = 
      Decomposition.Ok (Decomposition.ReplyOk a b c d)
   to (Right (Left ((a, b, c), (d, e, f, g)))) =
      Decomposition.Incorrect (Decomposition.ReplyIncorrect a b c d e f g)
   to (Right (Right (a, b))) = 
      Decomposition.Error (Decomposition.ReplyError a b)
   
   from (Decomposition.Ok (Decomposition.ReplyOk a b c d)) = Left (a, b, c, d)
   from (Decomposition.Incorrect (Decomposition.ReplyIncorrect a b c d e f g)) =
      Right (Left ((a, b, c), (d, e, f, g)))
   from (Decomposition.Error (Decomposition.ReplyError a b)) = Right (Right (a, b))
   
   tp  =  tuple4 Exercise StrategyLoc String Int
      :|: Pair (tuple3 Exercise StrategyLoc Term) 
               (tuple4 derTp argsTp Int Bool)
      :|: Pair String String
   derTp  = List (Pair String Term)
   argsTp = List (Pair String String)