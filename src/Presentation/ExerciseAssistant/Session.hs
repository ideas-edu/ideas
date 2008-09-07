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
   ( Domain(..), make, Some(..), Exercise(..)
   , Session, makeSession, newTerm, newExercise, progressPair, undo, submitText
   , currentText, derivationText, readyText, hintText, stepText, nextStep, ruleNames
   , getRuleAtIndex, applyRuleAtIndex, subTermAtIndices
   ) where

import qualified Service.TypedAbstractService as TAS
import Common.Context
import Common.Exercise (Exercise(..))
import Common.Parsing (indicesToRange)
import Common.Logging
import Common.Transformation
import Common.Apply
import Common.Utils
import Data.List
import Data.IORef
import Data.Maybe

newtype Domain a = Domain (Exercise a)

make :: Exercise a -> Some Domain
make ex = Some (Domain ex)

--------------------------------------------------
-- Sessions with logging

data Session = Session String (IORef (Some Derivation))

withState :: (forall a . Derivation a -> IO b) -> Session -> IO b
withState f (Session _ ref) = do
   Some d <- readIORef ref
   f d

makeSession :: Some Domain -> IO Session
makeSession pa = do
   logMessage "New session: "
   ref   <- newIORef (error "reference not initialized")
   let session = Session "" ref
   newExercise pa session
   return session

newExercise :: Some Domain -> Session -> IO ()
newExercise (Some (Domain a)) = logCurrent ("New (" ++ shortTitle a ++ ")") $ 
   \(Session _ ref) -> do
      d <- makeDerivation a
      writeIORef ref $ Some d
      
newTerm :: Session -> IO ()
newTerm session@(Session _ ref) = do
   Some d <- readIORef ref
   newExercise (Some (Domain (exercise d))) session
       
undo :: Session -> IO ()
undo = logCurrent "Undo" $ \(Session _ ref) ->
   modifyIORef ref $ \(Some d) -> Some (undoLast d)
 
submitText :: String -> Session -> IO (String, Bool)
submitText txt = logMsgWith fst ("Submit: " ++ txt) $ \(Session _ ref) -> do
   Some d <- readIORef ref
   case parser (exercise d) txt of
      Left err -> 
         return (err, False)
      Right term ->
         case TAS.submit (currentState d) (inContext term) of
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

currentText :: Session -> IO String
currentText = withState $ \d -> 
   return $ prettyPrinter (exercise d) (current d)

derivationText :: Session -> IO String
derivationText = withState $ \d -> 
   return $ showDerivation (prettyPrinter (exercise d)) d

progressPair :: Session -> IO (Int, Int)
progressPair = withState $ \d -> 
   let x = derivationLength d
       y = TAS.stepsremaining (currentState d)
   in return (x, x+y)

readyText :: Session -> IO String
readyText = logMsg "Ready" $ withState $ \d -> 
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
            [ "Use rule " ++ name rule
            ] ++
            [ "   with arguments " ++ showList (fromJust args)
            | let args = expectedArguments rule (current d), isJust args
            , let showList xs = "(" ++ concat (intersperse "," xs) ++ ")"
            ] ++ if verbose then
            [ "   to rewrite the term into:"
            , prettyPrinter (exercise d) (TAS.term s)
            ] else []

hintText, stepText :: Session -> IO String
hintText = logMsg "Hint" (hintOrStep False)
stepText = logMsg "Step" (hintOrStep True)

nextStep :: Session -> IO (String, Bool)
nextStep = logCurrent "Next" $ \(Session _ ref) -> do
   Some d <- readIORef ref
   case TAS.allfirsts (currentState d) of
      [] -> 
         return ("No more steps left to do", False)
      (rule, _, new):_ -> do
         writeIORef ref $ Some (extendDerivation new d)
         return ("Successfully applied rule " ++ name rule, True)

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
       check (r, _, s) = name r==name rule && any (equality a (TAS.term s)) results
       thisRule (r, _, _) = name r==name rule
   case safeHead (filter check answers) of
      Just (_, _, new) -> do
         writeIORef ref $ Some (extendDerivation new d)
         return ("Successfully applied rule " ++ name rule, True)
      _ | any thisRule answers && not (null args) -> 
         return ("Use rule " ++ name rule ++ " with different arguments:" ++ unlines (map (prettyPrinter a) results), False)
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

makeDerivation :: Exercise a -> IO (Derivation a)
makeDerivation ex = do 
   state <- TAS.generate ex 5
   return $ D [state]

undoLast :: Derivation a -> Derivation a
undoLast (D [x]) = D [x]
undoLast (D xs)  = D (drop 1 xs)

extendDerivation :: TAS.State a -> Derivation a -> Derivation a
extendDerivation x (D xs) = D (x:xs)

current :: Derivation a -> Context a
current (D (s:_)) = TAS.term s
current _ = error "Session.current: empty list"

exercise :: Derivation a -> Exercise a
exercise (D (s:_)) = TAS.exercise s
exercise _ = error "Session.exercise: empty list"

currentState :: Derivation a -> TAS.State a
currentState (D xs) = head xs

showDerivation :: (Context a -> String) -> Derivation a -> String
showDerivation f (D xs) = unlines $ intersperse "   =>" $ reverse $ [ f (TAS.term s) | s <- xs ] 

derivationLength :: Derivation a -> Int
derivationLength (D xs) = length xs - 1

--------------------------------------------------
-- Logging

logMsg :: String -> (Session -> IO String) -> Session -> IO String
logMsg = logMsgWith id

logCurrent :: String -> (Session -> IO a) -> Session -> IO a
logCurrent msg m session = do
   result <- m session
   term <- currentText session
   logMessage (msg ++ ": " ++ term)
   return result

logMsgWith :: (a -> String) -> String -> (Session -> IO a) -> Session -> IO a
logMsgWith f msg m session = do
   result <- m session
   logMessage (msg ++ ": " ++ f result)
   return result