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
   ( Some(..), Exercise(..), domain, Status(..)
   , Session, makeSession, newTerm, suggestTerm, suggestTermFor, newExercise
   , thisExercise, thisExerciseFor, progressPair, undo, submitText
   , currentDescription, currentText, derivationText, readyText, hintText
   , stepText, nextStep, ruleNames, currentState, getDerivation, currentCode
   ) where

import qualified Service.TypedAbstractService as TAS
import Service.Diagnose (restartIfNeeded)
import Service.Submit
import Service.FeedbackText
import Service.ExercisePackage (getExerciseText, ExercisePackage)
import qualified Service.ExercisePackage as Pkg
import Common.Context
import Common.Exercise hiding (showDerivation)
import Common.Strategy (emptyPrefix)
import Common.Transformation
import Common.Utils
import Control.Monad
import Data.List
import Data.Maybe
import Observable

--------------------------------------------------
-- Sessions with logging

type Session = Control (Some SessionState)

data SessionState a = SessionState 
   { getPackage    :: ExercisePackage a
   , getDerivation :: Derivation a
   }

currentCode :: Session -> IO ExerciseCode
currentCode ref = do
   (Some st) <- getValue ref
   return (exerciseCode (Pkg.exercise (getPackage st)))

withDerivation :: (forall a . Derivation a -> IO b) -> Session -> IO b
withDerivation f ref = do
   Some d <- getValue ref
   f (getDerivation d)

makeSession :: Some ExercisePackage -> IO Session
makeSession pkg = do
   ref <- createControl (error "reference not initialized")
   newExercise 5 pkg ref
   return ref

newExercise :: Int -> Some ExercisePackage -> Session -> IO ()
newExercise dif (Some pkg) ref = do
   d <- startNewDerivation dif (Pkg.exercise pkg)
   setValue ref $ Some $ SessionState pkg d

thisExercise :: String -> Session -> IO ()
thisExercise txt ref = do
   Some ss <- getValue ref
   let ex = exercise (getDerivation ss)
   case parser ex txt of
      Left _  -> return ()
      Right a -> do
         let new = makeDerivation $ TAS.State ex (Just $ emptyPrefix $ strategy ex) (inContext ex a)
         setValue ref $ Some $ ss {getDerivation = new}

thisExerciseFor :: String -> Some ExercisePackage -> Session -> IO (Maybe String)
thisExerciseFor txt (Some pkg) ref =
   let ex = Pkg.exercise pkg in
   case parser ex txt of
      Left err  -> return (Just $ show err)
      Right a -> do
         let new = makeDerivation $ TAS.State ex (Just $ emptyPrefix $ strategy ex) (inContext ex a)
         setValue ref $ Some $ SessionState pkg new
         return Nothing         
    
newTerm :: Int -> Session -> IO ()
newTerm dif ref = do
   Some ss <- getValue ref
   newExercise dif (Some (getPackage ss)) ref
   
suggestTerm :: Int -> Session -> IO String
suggestTerm dif ref = do
   Some ss <- getValue ref
   let ex = exercise (getDerivation ss)
   ca <- TAS.generate ex dif
   a  <- fromContext $ TAS.context ca
   return $ prettyPrinter ex a

suggestTermFor :: Int -> Some Exercise -> IO String
suggestTermFor dif (Some ex) = do
   ca <- TAS.generate ex dif
   a  <- fromContext $ TAS.context ca
   return $ prettyPrinter ex a
       
undo :: Session -> IO ()
undo ref =
   changeValue ref $ \(Some ss) -> Some ss {getDerivation = undoLast (getDerivation ss)}

submitText :: String -> Session -> IO String
submitText txt ref = do
   Some ss <- getValue ref
   let d = getDerivation ss
   case getExerciseText (getPackage ss) of
      -- Use exercise text module
      Just exText -> do
         let (b, msg, st) = submittext exText (currentState d) txt Nothing
             new = restartIfNeeded st
         when b $ setValue ref $ Some $ ss {getDerivation = extendDerivation new d}
         return msg
      -- Use default text
      Nothing ->
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
       y = fromMaybe 0 (TAS.stepsremaining (currentState d))
   in return (x, x+y)

readyText :: Session -> IO String
readyText = withDerivation $ \d -> 
   if TAS.ready (currentState d)
   then return "Congratulations: you have reached a solution!"
   else return "Sorry, you have not yet reached a solution"

hintOrStep :: Bool -> Session -> IO String
hintOrStep verbose ref = do
   Some ss <- getValue ref
   let d = getDerivation ss
       showRule r = fromMaybe ("rule " ++ name r) $ do 
          exText <- getExerciseText (getPackage ss)
          ruleText exText r
   case TAS.allfirsts (currentState d) of
      Left msg ->
         return ("Error: " ++ msg)
      Right [] -> 
         return "Sorry, no hint available"
      Right ((rule, _, s):_) ->
         return $ unlines $
            [ "Use " ++ showRule rule
            ] ++
            [ "   with arguments " ++ showList (fromJust args)
            | let args = expectedArguments rule (current d), isJust args
            , let showList xs = "(" ++ concat (intersperse "," xs) ++ ")"
            ] ++ if verbose then
            [ "   to rewrite the term into:"
            , prettyPrinter (exercise d) (TAS.term s)
            ] else []

hintText, stepText :: Session -> IO String
hintText = hintOrStep False
stepText = hintOrStep True

nextStep :: Session -> Int -> IO String
nextStep ref n = do
   Some ss <- getValue ref
   let d = getDerivation ss
   case TAS.allfirsts (currentState d) of
      Left msg ->
         return ("Error: " ++ msg)
      Right [] -> 
         return "No more steps left to do"
      Right ((rule, _, new):_) -> do
         setValue ref $ Some $ ss { getDerivation = extendDerivation new d  }
         case getExerciseText (getPackage ss) of
            Just exText ->
               return (appliedRule exText rule)
            Nothing -> 
               return ("You have applied rule " ++ name rule ++ " correctly.")

ruleNames :: Session -> IO [String]
ruleNames = withDerivation $ \d -> 
   return $ map name $ filter isMajorRule $ ruleset $ exercise d

{-
getRuleAtIndex :: Int -> Session -> IO (Some Rule)
getRuleAtIndex i = withDerivation $ \d -> do
   let rule = filter (not . isMinorRule) (ruleset (exercise d)) !! i
   return (Some rule)

applyRuleAtIndex :: Int -> Maybe Location -> [String] -> Session -> IO (String, Bool)
applyRuleAtIndex i mloc args ref = do
   Some d <- readIORef ref
   let a = exercise d
   let rule    = filter isMajorRule (ruleset a) !! i
       newRule = fromMaybe rule (useArguments args rule)
       loc     = fromMaybe (makeLocation []) mloc
       results = applyAll newRule (setLocation loc $ current d)
       answers = TAS.allfirsts (currentState d)
       check (r, _, s) = name r==name rule && any (similarity a (TAS.term s) . fromContext) results
       thisRule (r, _, _) = name r==name rule
   case safeHead (filter check answers) of
      Just (_, _, new) -> do
         writeIORef ref $ Some (extendDerivation new d)
         return ("Successfully applied rule " ++ name rule, True)
      _ | any thisRule answers && not (null args) -> 
         return ("Use rule " ++ name rule ++ " with different arguments:" ++ unlines (map (prettyPrinter a . fromContext) results), False)
      _ | any thisRule answers && null args ->
         return ("Apply rule " ++ name rule ++ " at a different location", False)
      _ ->
         return ("You selected rule " ++ name rule ++ ": try a different rule", False)
-}

--------------------------------------------------
-- Session state

exercise :: Derivation a -> Exercise a
exercise (D (s:_)) = TAS.exercise s
exercise _ = error "Session.exercise: empty list"

--------------------------------------------------
-- Derivations

newtype Derivation a = D [TAS.State a]

startNewDerivation :: Int -> Exercise a -> IO (Derivation a)
startNewDerivation dif ex = do 
   state <- TAS.generate ex dif
   return $ makeDerivation state

makeDerivation :: TAS.State a -> Derivation a
makeDerivation state = D [state]

undoLast :: Derivation a -> Derivation a
undoLast (D [x]) = D [x]
undoLast (D xs)  = D (drop 1 xs)

extendDerivation :: TAS.State a -> Derivation a -> Derivation a
extendDerivation x (D xs) = D (x:xs)

current :: Derivation a -> Context a
current (D (s:_)) = TAS.context s
current _ = error "Session.current: empty list"

currentState :: Derivation a -> TAS.State a
currentState (D xs) = head xs

showDerivation :: (a -> String) -> Derivation a -> String
showDerivation f (D xs) = unlines $ intersperse "   =>" $ reverse $ [ f (TAS.term s) | s <- xs ] 

derivationLength :: Derivation a -> Int
derivationLength (D xs) = length xs - 1