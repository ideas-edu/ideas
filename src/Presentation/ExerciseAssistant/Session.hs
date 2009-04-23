{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Session
   ( Some(..), Exercise(..), Status(..), exerciseCode
   , Session, makeSession, newTerm, suggestTerm, suggestTermFor, newExercise
   , thisExercise, thisExerciseFor, progressPair, undo, submitText
   , currentDescription, currentText, derivationText, readyText, hintText, stepText, nextStep, ruleNames
   , getRuleAtIndex, applyRuleAtIndex, subTermAtIndices
   ) where

import qualified Service.TypedAbstractService as TAS
import Service.FeedbackText (feedbackLogic)
import Common.Context
import Common.Exercise hiding (showDerivation)
import Text.Parsing (indicesToRange)
import Common.Strategy (emptyPrefix)
import Common.Transformation
import Common.Apply
import Common.Utils
import Domain.Logic.Exercises (dnfExercise) -- to be removed 
import Domain.Logic.FeedbackText            -- to be removed
import Data.List
import Data.IORef
import Data.Maybe

--------------------------------------------------
-- Sessions with logging

data Session = Session String (IORef (Some Derivation))

withState :: (forall a . Derivation a -> IO b) -> Session -> IO b
withState f (Session _ ref) = do
   Some d <- readIORef ref
   f d

makeSession :: Some Exercise -> IO Session
makeSession pa = do
   ref   <- newIORef (error "reference not initialized")
   let session = Session "" ref
   newExercise 5 pa session
   return session

newExercise :: Int -> Some Exercise -> Session -> IO ()
newExercise dif (Some a) (Session _ ref) = do
   d <- startNewDerivation dif a
   writeIORef ref $ Some d

thisExercise :: String -> Session -> IO (Maybe String)
thisExercise txt (Session _ ref) = do
   Some d <- readIORef ref
   let ex = exercise d
   case parser ex txt of
      Left err  -> return (Just $ show err)
      Right a -> do
         let new = makeDerivation $ TAS.State ex (Just $ emptyPrefix $ strategy ex) (inContext a)
         writeIORef ref $ Some new
         return Nothing

thisExerciseFor :: String -> Some Exercise -> Session -> IO (Maybe String)
thisExerciseFor txt (Some ex) (Session _ ref) =
   case parser ex txt of
      Left err  -> return (Just $ show err)
      Right a -> do
         let new = makeDerivation $ TAS.State ex (Just $ emptyPrefix $ strategy ex) (inContext a)
         writeIORef ref $ Some new
         return Nothing         
    
newTerm :: Int -> Session -> IO ()
newTerm dif session@(Session _ ref) = do
   Some d <- readIORef ref
   newExercise dif (Some (exercise d)) session
   
suggestTerm :: Int -> Session -> IO String
suggestTerm dif (Session _ ref) = do
   Some d <- readIORef ref
   let ex = exercise d
   a <- TAS.generate ex dif
   return $ prettyPrinter ex $ fromContext $ TAS.context a

suggestTermFor :: Int -> Some Exercise -> IO String
suggestTermFor dif (Some ex) = do
   a <- TAS.generate ex dif
   return $ prettyPrinter ex $ fromContext $ TAS.context a
       
undo :: Session -> IO ()
undo (Session _ ref) =
   modifyIORef ref $ \(Some d) -> Some (undoLast d)

submitText :: String -> Session -> IO (String, Bool)
submitText txt (Session _ ref) = do
   Some d <- readIORef ref
   if exerciseCode (exercise d) == exerciseCode dnfExercise 
      then submitTextLogic   txt ref 
      else submitTextGeneral txt ref
         
submitTextGeneral :: String -> IORef (Some Derivation) -> IO (String, Bool)
submitTextGeneral txt ref = do
   Some d <- readIORef ref
   case parser (exercise d) txt of
      Left err -> 
         return (show err, False)
      Right term ->
         case TAS.submit (currentState d) term of
            TAS.Buggy rs -> 
               return ("Incorrect: you used the buggy rule: " ++ show rs, False)
            TAS.NotEquivalent -> 
               return ("Incorrect", False)
            TAS.Ok rs new 
               | null rs -> 
                    return ("You have submitted the current term.", False)
               | otherwise -> do
                    writeIORef ref $ Some (extendDerivation new d)
                    return ("Well done! You applied rule " ++ show rs, True)
            TAS.Detour rs _ -> 
               return ("You applied rule " ++ show rs ++ ". Although it is equivalent, please follow the strategy", False)
            TAS.Unknown _ -> 
               return ("Equivalent, but not a known rule. Please retry.", False)

submitTextLogic :: String -> IORef (Some Derivation) -> IO (String, Bool)
submitTextLogic txt ref = do
   Some d <- readIORef ref
   case parser (exercise d) txt of
      Left err -> return (feedbackSyntaxError err, False)
      Right term -> do
         let old = currentState d
             result = TAS.submit old term
         case (feedbackLogic old result, TAS.getResultState result) of
            ((txt, True), Just n) -> do
               -- make sure that new has a prefix (because of possible detour)
               -- when resetting the prefix, also make sure that the context is refreshed
               let new = TAS.resetStateIfNeeded n
               writeIORef ref $ Some (extendDerivation new d)
               return (txt, True)
            ((txt, _), _) -> return (txt, False)
      
currentText :: Session -> IO String
currentText = withState $ \d -> 
   return $ prettyPrinter (exercise d) (fromContext $ current d)

currentDescription :: Session -> IO String
currentDescription = withState $ \d -> 
   return $ description (exercise d)

derivationText :: Session -> IO String
derivationText = withState $ \d -> 
   return $ showDerivation (prettyPrinter (exercise d)) d

progressPair :: Session -> IO (Int, Int)
progressPair = withState $ \d -> 
   let x = derivationLength d
       y = TAS.stepsremaining (currentState d)
   in return (x, x+y)

readyText :: Session -> IO String
readyText = withState $ \d -> 
   if TAS.ready (currentState d)
   then return "Congratulations: you have reached a solution!"
   else return "Sorry, you have not yet reached a solution"

hintOrStep :: Bool -> Session -> IO String
hintOrStep verbose = withState $ \d -> 
   case TAS.allfirsts (currentState d) of
      [] -> 
         return "Sorry, no hint available"
      (rule, _, s):_ ->
         return $ unlines $
            [ "Use " ++ fromMaybe ("rule " ++ name rule) (ruleText rule)
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

nextStep :: Session -> IO (String, Bool)
nextStep (Session _ ref) = do
   Some d <- readIORef ref
   case TAS.allfirsts (currentState d) of
      [] -> 
         return ("No more steps left to do", False)
      (rule, _, new):_ -> do
         writeIORef ref $ Some (extendDerivation new d)
         return (appliedRule rule, True)

ruleNames :: Session -> IO [String]
ruleNames = withState $ \d -> 
   return $ map name $ filter isMajorRule $ ruleset $ exercise d

getRuleAtIndex :: Int -> Session -> IO (Some Rule)
getRuleAtIndex i = withState $ \d -> do
   let rule = filter (not . isMinorRule) (ruleset (exercise d)) !! i
   return (Some rule)

applyRuleAtIndex :: Int -> Maybe Location -> [String] -> Session -> IO (String, Bool)
applyRuleAtIndex i mloc args (Session _ ref) = do
   Some d <- readIORef ref
   let a = exercise d
   let rule    = filter isMajorRule (ruleset a) !! i
       newRule = fromMaybe rule (useArguments args rule)
       loc     = fromMaybe (makeLocation []) mloc
       results = applyAll newRule (setLocation loc $ current d)
       answers = TAS.allfirsts (currentState d)
       check (r, _, s) = name r==name rule && any (equality a (TAS.term s) . fromContext) results
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

subTermAtIndices :: String -> Int -> Int -> Session -> IO (Maybe Location)
subTermAtIndices s i j = withState $ \d -> do
   let rng = indicesToRange s i j
   return (subTerm (exercise d) s rng)

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

exercise :: Derivation a -> Exercise a
exercise (D (s:_)) = TAS.exercise s
exercise _ = error "Session.exercise: empty list"

currentState :: Derivation a -> TAS.State a
currentState (D xs) = head xs

showDerivation :: (a -> String) -> Derivation a -> String
showDerivation f (D xs) = unlines $ intersperse "   =>" $ reverse $ [ f (TAS.term s) | s <- xs ] 

derivationLength :: Derivation a -> Int
derivationLength (D xs) = length xs - 1