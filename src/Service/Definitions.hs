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
   ) where

import Common.Context (Context)
import Common.Exercise (Exercise)
import Common.Navigator (Location)
import Common.Transformation (Rule)
import Common.Strategy (Strategy, StrategyLocation, StrategyConfiguration)
import Common.Utils (commaList)
import Data.Maybe
import Service.TypedAbstractService (State)
import Service.Submit (Result)
import Service.Diagnose (Diagnosis)
import Service.FeedbackText (ExerciseText)
import Service.RulesInfo (RulesInfo)
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
   Elem         :: Type a t -> Type a t -- quick fix
   IO           :: Type a t -> Type a (IO t)
   -- Exercise-specific types
   State        :: Type a (State a)
   Exercise     :: Type a (Exercise a)
   Strategy     :: Type a (Strategy (Context a))
   ExerciseText :: Type a (ExerciseText a)
   Rule         :: Type a (Rule (Context a))
   RulesInfo    :: Type a (RulesInfo a)
   Term         :: Type a a
   Context      :: Type a (Context a)
   Result       :: Type a (Result a)
   Diagnosis    :: Type a (Diagnosis a)
   Location     :: Type a Location
   StrategyLoc  :: Type a StrategyLocation
   StrategyCfg  :: Type a StrategyConfiguration
   DecompositionReply :: Type a (Decomposition.Reply a)
   -- Basic types
   Bool         :: Type a Bool
   Int          :: Type a Int
   String       :: Type a String

instance Show (Type a t) where
   show (Iso _ _ t)    = show t
   show (t1 :-> t2)    = show t1 ++ " -> " ++ show t2 
   show t@(Pair _ _)   = showTuple t
   show (Tag _ t)      = show t
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
      RulesInfo    -> Just "RulesInfo"
      Term         -> Just "Term"
      Context      -> Just "Context"
      Result       -> Just "Result"
      Diagnosis    -> Just "Diagnosis"
      Bool         -> Just "Bool"
      Int          -> Just "Int"
      String       -> Just "String"
      Location     -> Just "Location"
      StrategyLoc  -> Just "StrategyLocation"
      StrategyCfg  -> Just "StrategyConfiguration"
      _            -> Nothing