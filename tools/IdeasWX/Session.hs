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

import Common.Library
import Common.Utils
import Control.Monad
import Data.Maybe
import Observable hiding (Id)
import Service.BasicServices
import Service.Diagnose (restartIfNeeded)
import Service.FeedbackScript.Parser
import Service.FeedbackScript.Run
import Service.FeedbackText
import Service.State
import Service.Submit

------------------------------------------------------------
-- Helper function

exerciseScript :: HasId a => a -> IO (Maybe Script)
exerciseScript a
   | take 1 (qualifiers a) == ["logic"] =
        liftM Just (parseScript (Just "../scripts") "logic.txt")
   | otherwise =
        return Nothing

--------------------------------------------------
-- Sessions with logging

type Session = Control (Some SessionState)

data SessionState a = SessionState 
   { getDerivation :: Derivation () (State a)
   }

getExercise :: SessionState a -> Exercise a
getExercise = exercise . firstTerm . getDerivation

currentExerciseId :: Session -> IO Id
currentExerciseId ref = do
   (Some st) <- getValue ref
   return (getId (getExercise st))

makeSession :: Some Exercise -> IO Session
makeSession ex = do
   ref <- createControl (error "reference not initialized")
   newExercise Medium ex ref
   return ref

newExercise :: Difficulty -> Some Exercise -> Session -> IO ()
newExercise dif (Some ex) ref = do
   d <- startNewDerivation dif ex
   setValue ref $ Some $ SessionState d

thisExercise :: String -> Session -> IO ()
thisExercise txt ref = do
   Some ss <- getValue ref
   let ex = getExercise ss
   case parser ex txt of
      Left _  -> return ()
      Right a -> do
         let new = emptyDerivation $ emptyState ex a
         setValue ref $ Some $ ss {getDerivation = new}

thisExerciseFor :: String -> Some Exercise -> Session -> IO (Maybe String)
thisExerciseFor txt (Some ex) ref =
   case parser ex txt of
      Left err  -> return (Just $ show err)
      Right a -> do
         let new = emptyDerivation $ makeState ex (Just $ emptyPrefix $ strategy ex) (inContext ex a)
         setValue ref $ Some $ SessionState new
         return Nothing         
    
newTerm :: Difficulty -> Session -> IO ()
newTerm dif ref = do
   Some ss <- getValue ref
   newExercise dif (Some (getExercise ss)) ref
   
suggestTerm :: Session -> IO String
suggestTerm ref = do
   Some ss <- getValue ref
   suggestTermFor Medium (Some (getExercise ss))

suggestTermFor :: Difficulty -> Some Exercise -> IO String
suggestTermFor dif (Some ex) = do
   a  <- randomTerm dif ex
   return $ prettyPrinter ex a
       
undo :: Session -> IO ()
undo ref =
   changeValue ref $ \(Some ss) -> Some ss {getDerivation = withoutLast (getDerivation ss)}

submitText :: String -> Session -> IO String
submitText txt ref = do
   Some ss <- getValue ref
   ms <- exerciseScript (getExercise ss)
   let d = getDerivation ss
   case ms of
      -- Use exercise text module
      Just script -> do
         let (b, msg, st) = submittext script (currentState d) txt
             new = restartIfNeeded st
         when b $ setValue ref $ Some $ ss {getDerivation = extendDerivation new d}
         return (show msg)
      -- Use default text
      _ ->
         case parser (getExercise ss) txt of
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
currentText ref = do 
   Some ss <- getValue ref
   a <- fromContext $ currentContext $ getDerivation ss
   return $ prettyPrinter (getExercise ss) a

currentDescription :: Session -> IO String
currentDescription ref = do 
   Some ss <- getValue ref
   return $ description (getExercise ss)

derivationText :: Session -> IO String
derivationText ref = do 
   Some ss <- getValue ref
   return $ showDerivationWith (prettyPrinter (getExercise ss)) (getDerivation ss)

progressPair :: Session -> IO (Int, Int)
progressPair ref = do
   Some ss <- getValue ref
   let d = getDerivation ss 
       x = derivationLength d
       y = either (const 0) id (stepsremaining (currentState d))
   return (x, x+y)

readyText :: Session -> IO String
readyText ref = do
   Some ss <- getValue ref 
   if ready (currentState (getDerivation ss))
     then return "Congratulations: you have reached a solution!"
     else return "Sorry, you have not yet reached a solution"

hintOrStep :: Bool -> Session -> IO String
hintOrStep verbose ref = do
   Some ss <- getValue ref
   ms <- exerciseScript (getExercise ss)
   let d = getDerivation ss
       showRule r = fromMaybe ("rule " ++ showId r) $ do 
          script <- ms
          return (ruleToString (newEnvironment (currentState d)) script r)
   case allfirsts (currentState d) of
      Left msg ->
         return ("Error: " ++ msg)
      Right [] -> 
         return "Sorry, no hint available"
      Right ((r, _, _, s):_) ->
         return $ unlines $
            [ "Use " ++ showRule r
            ] ++
            [ "   with arguments " ++ commaList (map f (fromJust args))
            | let args = expectedArguments r (currentContext d), isJust args
            , let f (ArgValue descr a) = showArgument descr a
            ] ++ if verbose then
            [ "   to rewrite the term into:"
            , prettyPrinter (getExercise ss) (stateTerm s)
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
      Right ((r, _, _, new):_) -> do
         ms <- exerciseScript new
         setValue ref $ Some $ ss { getDerivation = extendDerivation new d  }
         case ms of
            Just script ->
               return ("You have applied " ++ ruleToString (newEnvironment (currentState d)) script r ++ " correctly.")
            _ -> 
               return ("You have applied rule " ++ showId r ++ " correctly.")

--------------------------------------------------
-- Derivations


startNewDerivation :: Difficulty -> Exercise a -> IO (Derivation () (State a))
startNewDerivation dif ex = do
   a <- randomTerm dif ex
   return $ emptyDerivation (emptyState ex a)

extendDerivation :: a -> Derivation () a -> Derivation () a
extendDerivation x d = d `extend` ((), x)

currentContext :: Derivation () (State a) -> Context a
currentContext = stateContext . currentState

currentState :: Derivation () a -> a
currentState = last . terms

showDerivationWith :: (a -> String) -> Derivation () (State a) -> String
showDerivationWith f = show . biMap (const (ShowString "")) (f . stateTerm)