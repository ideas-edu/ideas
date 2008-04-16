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

import Common.Context
import Common.Exercise
import Common.Parsing (indicesToRange)
import Common.Logging
import Common.Transformation
import Common.Strategy hiding (not, Step)
import Common.Apply
import Common.Utils
import Data.List
import Data.IORef
import Data.Maybe
import System.Time

newtype Domain a = Domain (Exercise (Context a))

make :: Exercise (Context a) -> Some Domain
make ex = Some (Domain ex)

--------------------------------------------------
-- Sessions with logging

data Session = Session String (IORef SessionState)

data SessionState = forall a . St (Exercise (Context a)) (Derivation (Context a))

withState :: (forall a . Exercise a -> Derivation a -> IO b) -> Session -> IO b
withState f (Session _ ref) = do
   St a d <- readIORef ref
   f a d

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
      term <- randomTerm a
      writeIORef ref $ St a (Start (emptyPrefix $ strategy a) term)

newTerm :: Session -> IO ()
newTerm session@(Session _ ref) = do
   St a _ <- readIORef ref
   newExercise (Some (Domain a)) session
        
undo :: Session -> IO ()
undo = logCurrent "Undo" $ \(Session _ ref) ->
   modifyIORef ref $ \st@(St a d) -> case d of 
      Start _ _    -> st
      Step d _ _ _ -> St a d

submitText :: String -> Session -> IO (String, Bool)
submitText txt = logMsgWith fst ("Submit: " ++ txt) $ \(Session _ ref) -> do
   St a d <- readIORef ref
   case feedback a (currentPrefix d) (current d) txt of
      SyntaxError doc -> 
         let msg = "Parse error:\n" ++ showDoc a doc
         in return (msg, False)
      Incorrect doc -> 
         let msg = showDoc a doc
         in return (msg, False)
      Correct doc Nothing ->
         return (showDoc a doc, False)
      Correct doc (Just (newPrefix, rule, new)) -> do
         -- let new = either (error "internal error") id $ parser a txt -- REWRITE !
         writeIORef ref $ St a (Step d rule newPrefix new)
         return (showDoc a doc, True)
   
currentText :: Session -> IO String
currentText = withState $ \a d -> 
   return $ prettyPrinter a (current d)

derivationText :: Session -> IO String
derivationText = withState $ \a d -> 
   return $ showDerivation (prettyPrinter a) d   

progressPair :: Session -> IO (Int, Int)
progressPair = withState $ \a d -> 
   let x = derivationLength d
       y = stepsRemaining (currentPrefix d) (current d)
   in return (x, x+y)
  
readyText :: Session -> IO String
readyText = logMsg "Ready" $ withState $ \a d -> 
   if finalProperty a (current d)
   then return "Congratulations: you have reached a solution!"
   else return "Sorry, you have not yet reached a solution"

hintText :: Session -> IO String
hintText = logMsg "Hint" $ withState $ \a d -> 
   case giveHint (currentPrefix d) (current d) of 
      Nothing -> 
         return "Sorry, no hint available" 
      Just (doc, rule) ->
         return $ showDoc a doc

stepText :: Session -> IO String
stepText = logMsg "Step" $ withState $ \a d -> 
   case giveStep (currentPrefix d) (current d) of
      Nothing -> 
         return "Sorry, no hint available"
      Just (doc, rule, newPrefix, before, after) ->
         return $ unlines $
            [ "Use rule " ++ name rule
            ] ++
            [ "   with arguments " ++ showList (fromJust args)
            | let args = expectedArguments rule before, isJust args
            , let showList xs = "(" ++ concat (intersperse "," xs) ++ ")"
            ] ++
            [ "   to rewrite the term into:"
            , prettyPrinter a after
            ]

nextStep :: Session -> IO (String, Bool)
nextStep = logCurrent "Next" $ \(Session _ ref) -> do
   St a d <- readIORef ref
   case giveStep (currentPrefix d) (current d) of
      Nothing -> 
         return ("No more steps left to do", False)
      Just (_, rule, newPrefix, _, new) -> do
         writeIORef ref $ St a (Step d rule newPrefix new)
         return ("Successfully applied rule " ++ name rule, True)

ruleNames :: Session -> IO [String]
ruleNames = withState $ \a d -> 
   return $ map name $ filter isMajorRule $ ruleset a

getRuleAtIndex :: Int -> Session -> IO (Some Rule)
getRuleAtIndex i = withState $ \a d -> do
   let rule = filter (not . isMinorRule) (ruleset a) !! i
   return (Some rule)

applyRuleAtIndex :: Int -> Maybe [Int] -> [String] -> Session -> IO (String, Bool)
applyRuleAtIndex i mloc args (Session _ ref) = do
   St a d <- readIORef ref
   let rule    = filter isMajorRule (ruleset a) !! i
       newRule = fromMaybe rule (useArguments args rule)
       loc     = fromMaybe [] mloc
       results = applyAll newRule (setLocation loc $ current d)
       answers = giveSteps (currentPrefix d) (current d)
       check    (_, r, _, _, new) = name r==name rule && any (equality a new) results
       thisRule (_, r, _, _, _)   = name r==name rule
   case safeHead (filter check answers) of
      Just (_, _, newPrefix, _, new) -> do
         writeIORef ref $ St a (Step d rule newPrefix new)
         return ("Successfully applied rule " ++ name rule, True)
      _ | any thisRule answers && not (null args) -> 
         return ("Use rule " ++ name rule ++ " with different arguments" ++ show (map (prettyPrinter a) results), False)
      _ | any thisRule answers && null args ->
         return ("Apply rule " ++ name rule ++ " at a different location", False)
      _ -> 
         return ("You selected rule " ++ name rule ++ ": try a different rule", False)

subTermAtIndices :: String -> Int -> Int -> Session -> IO (Maybe [Int])
subTermAtIndices s i j = withState $ \a d -> do
   let rng = indicesToRange s i j
   return (subTerm a s rng)

--------------------------------------------------
-- Derivations

data Derivation a = Start (Prefix a) a | Step (Derivation a) (Rule a) (Prefix a) a -- snoc list for fast access to current term

current :: Derivation a -> a
current (Start _ a)    = a
current (Step _ _ _ a) = a

initial :: Derivation a -> a
initial (Start _ a)    = a
initial (Step d _ _ _) = initial d

currentPrefix :: Derivation a -> Prefix a
currentPrefix (Start p _)    = p
currentPrefix (Step _ _ p _) = p

-- | to do: make this function efficient (accumulating parameter)
terms :: Derivation a -> [a]
terms (Start _ a)    = [a]
terms (Step d _ _ a) = terms d ++ [a]

showDerivation :: (a -> String) -> Derivation a -> String
showDerivation f (Start _ a)    = f a
showDerivation f (Step d r _ a) = showDerivation f d ++ "\n   => [" ++ name r ++ "]\n" ++ f a

derivationLength :: Derivation a -> Int
derivationLength (Start _ _)    = 0
derivationLength (Step d _ _ _) = 1 + derivationLength d

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