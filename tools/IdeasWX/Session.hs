{-# OPTIONS -XRankNTypes #-}
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
module Session
   ( Some(..), Exercise(..), qualification, Status(..)
   , Session, makeSession, newTerm, suggestTerm, suggestTermFor, newExercise
   , thisExercise, thisExerciseFor, progressPair, undo, submitText
   , currentDescription, currentText, derivationText, readyText, hintText
   , stepText, nextStep, currentState, getDerivation, currentExerciseId
   ) where

import Common.Library hiding (current, showDerivation)
import Common.Utils
import Control.Monad
import Data.Maybe
import Observable hiding (Id)
import Service.BasicServices
import Service.Diagnose (restartIfNeeded)
import Service.ExercisePackage (ExercisePackage)
import Service.FeedbackScript.Parser
import Service.FeedbackScript.Run
import Service.FeedbackText
import Service.State
import Service.Submit
import qualified Service.ExercisePackage as Pkg

------------------------------------------------------------
-- Helper function

exerciseScript :: HasId a => a -> IO (Maybe Script)
exerciseScript a
   | take 1 (qualifiers a) == ["logic"] =
        liftM Just (parseScript (Just "scripts") "logic.txt")
   | otherwise =
        return Nothing

--------------------------------------------------
-- Sessions with logging

type Session = Control (Some SessionState)

data SessionState a = SessionState 
   { getPackage    :: ExercisePackage a
   , getDerivation :: Derivation () (State a)
   }

currentExerciseId :: Session -> IO Id
currentExerciseId ref = do
   (Some st) <- getValue ref
   return (getId (getPackage st))

withDerivation :: (forall a . Derivation () (State a) -> IO b) -> Session -> IO b
withDerivation f ref = do
   Some d <- getValue ref
   f (getDerivation d)

makeSession :: Some ExercisePackage -> IO Session
makeSession pkg = do
   ref <- createControl (error "reference not initialized")
   newExercise Medium pkg ref
   return ref

newExercise :: Difficulty -> Some ExercisePackage -> Session -> IO ()
newExercise dif (Some pkg) ref = do
   d <- startNewDerivation dif pkg
   setValue ref $ Some $ SessionState pkg d

thisExercise :: String -> Session -> IO ()
thisExercise txt ref = do
   Some ss <- getValue ref
   let pkg = getPackage ss
   case parser (Pkg.exercise pkg) txt of
      Left _  -> return ()
      Right a -> do
         let new = emptyDerivation $ emptyState pkg a
         setValue ref $ Some $ ss {getDerivation = new}

thisExerciseFor :: String -> Some ExercisePackage -> Session -> IO (Maybe String)
thisExerciseFor txt (Some pkg) ref =
   let ex = Pkg.exercise pkg in
   case parser ex txt of
      Left err  -> return (Just $ show err)
      Right a -> do
         let new = emptyDerivation $ makeState pkg (Just $ emptyPrefix $ strategy ex) (inContext ex a)
         setValue ref $ Some $ SessionState pkg new
         return Nothing         
    
newTerm :: Difficulty -> Session -> IO ()
newTerm dif ref = do
   Some ss <- getValue ref
   newExercise dif (Some (getPackage ss)) ref
   
suggestTerm :: Session -> IO String
suggestTerm ref = do
   Some ss <- getValue ref
   suggestTermFor Medium (Some (getPackage ss))

suggestTermFor :: Difficulty -> Some ExercisePackage -> IO String
suggestTermFor dif (Some pkg) = do
   let ex = Pkg.exercise pkg
   a  <- randomTerm dif ex
   return $ prettyPrinter ex a
       
undo :: Session -> IO ()
undo ref =
   changeValue ref $ \(Some ss) -> Some ss {getDerivation = withoutLast (getDerivation ss)}

submitText :: String -> Session -> IO String
submitText txt ref = do
   Some ss <- getValue ref
   ms <- exerciseScript (getPackage ss)
   let d = getDerivation ss
   case ms of
      -- Use exercise text module
      Just script -> do
         let (b, msg, st) = submittext script (currentState d) txt
             new = restartIfNeeded st
         when b $ setValue ref $ Some $ ss {getDerivation = extendDerivation new d}
         return msg
      -- Use default text
      _ ->
         case parser (exercise d) txt of
            Left err -> 
               return (show err)
            Right term ->
               case submit (currentState d) term of
                  Buggy rs -> 
                     return ("Incorrect: you used the buggy rule: " ++ show rs)
                  NotEquivalent -> 
                     return ("Incorrect")
                  Ok rs new 
                     | null rs -> 
                          return ("You have submitted the current term.")
                     | otherwise -> do
                          setValue ref $ Some ss {getDerivation = extendDerivation new d}
                          return ("Well done! You applied rule " ++ show rs)
                  Detour rs _ -> 
                     return ("You applied rule " ++ show rs ++ ". Although it is equivalent, please follow the strategy")
                  Unknown _ -> 
                     return ("Equivalent, but not a known rule. Please retry.")

currentText :: Session -> IO String
currentText = withDerivation $ \d -> do
   a <- fromContext $ current d
   return $ prettyPrinter (exercise d) a

currentDescription :: Session -> IO String
currentDescription = withDerivation $ \d -> 
   return $ description (exercise d)

derivationText :: Session -> IO String
derivationText = withDerivation $ \d -> 
   return $ showDerivation (prettyPrinter (exercise d)) d

progressPair :: Session -> IO (Int, Int)
progressPair = withDerivation $ \d -> 
   let x = derivationLength d
       y = either (const 0) id (stepsremaining (currentState d))
   in return (x, x+y)

readyText :: Session -> IO String
readyText = withDerivation $ \d -> 
   if ready (currentState d)
   then return "Congratulations: you have reached a solution!"
   else return "Sorry, you have not yet reached a solution"

hintOrStep :: Bool -> Session -> IO String
hintOrStep verbose ref = do
   Some ss <- getValue ref
   ms <- exerciseScript (getPackage ss)
   let d = getDerivation ss
       showRule r = fromMaybe ("rule " ++ showId r) $ do 
          script <- ms
          return (ruleToString (newEnvironment (currentState d)) script r)
   case allfirsts (currentState d) of
      Left msg ->
         return ("Error: " ++ msg)
      Right [] -> 
         return "Sorry, no hint available"
      Right ((r, _, s):_) ->
         return $ unlines $
            [ "Use " ++ showRule r
            ] ++
            [ "   with arguments " ++ commaList (map snd (fromJust args))
            | let args = expectedArguments r (current d), isJust args
            ] ++ if verbose then
            [ "   to rewrite the term into:"
            , prettyPrinter (exercise d) (stateTerm s)
            ] else []

hintText, stepText :: Session -> IO String
hintText = hintOrStep False
stepText = hintOrStep True

nextStep :: Session -> IO String
nextStep ref = do
   Some ss <- getValue ref
   let d = getDerivation ss
   case allfirsts (currentState d) of
      Left msg ->
         return ("Error: " ++ msg)
      Right [] -> 
         return "No more steps left to do"
      Right ((r, _, new):_) -> do
         ms <- exerciseScript new
         setValue ref $ Some $ ss { getDerivation = extendDerivation new d  }
         case ms of
            Just script ->
               return ("You have applied " ++ ruleToString (newEnvironment (currentState d)) script r ++ " correctly.")
            _ -> 
               return ("You have applied rule " ++ showId r ++ " correctly.")

--------------------------------------------------
-- Session state

exercise :: Derivation () (State a) -> Exercise a
exercise = Pkg.exercise . exercisePkg . currentState

--------------------------------------------------
-- Derivations


startNewDerivation :: Difficulty -> ExercisePackage a -> IO (Derivation () (State a))
startNewDerivation dif pkg = do
   a <- randomTerm dif (Pkg.exercise pkg)
   return $ emptyDerivation (emptyState pkg a)

extendDerivation :: a -> Derivation () a -> Derivation () a
extendDerivation x d = d `extend` ((), x)

current :: Derivation () (State a) -> Context a
current = stateContext . currentState

currentState :: Derivation () a -> a
currentState = last . terms

showDerivation :: (a -> String) -> Derivation () (State a) -> String
showDerivation f = show . biMap (const (ShowString "")) (f . stateTerm)