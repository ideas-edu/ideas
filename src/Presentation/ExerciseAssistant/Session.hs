{-# OPTIONS -fglasgow-exts #-}
module Session 
   ( Some(..), Exercise(..)
   , Session, makeSession, newTerm, newExercise, progressPair, undo, submitText
   , currentText, derivationText, readyText, hintText, stepText, nextStep, ruleNames
   , getRuleAtIndex, applyRuleAtIndex
   ) where

import Common.Exercise
import Common.Logging
import Common.Transformation
import Common.Apply
import Common.Utils
import Data.IORef
import Data.Maybe
import System.Time

--------------------------------------------------
-- Sessions with logging

data Session = Session String (IORef SessionState)

data SessionState = forall a . St (Exercise a) (Derivation a)

withState :: (forall a . Exercise a -> Derivation a -> IO b) -> Session -> IO b
withState f (Session _ ref) = do
   St a d <- readIORef ref
   f a d

makeSession :: Some Exercise -> IO Session
makeSession pa = do
   logMessage "New session: "
   ref   <- newIORef (error "reference not initialized")
   let session = Session "" ref
   newExercise pa session
   return session

newExercise :: Some Exercise -> Session -> IO ()
newExercise (Some a) = logCurrent ("New (" ++ shortTitle a ++ ")") $ 
   \(Session _ ref) -> do
      term <- randomTerm a
      writeIORef ref $ St a (Start term)

newTerm :: Session -> IO ()
newTerm session@(Session _ ref) = do
   St a _ <- readIORef ref
   newExercise (Some a) session
        
undo :: Session -> IO ()
undo = logCurrent "Undo" $ \(Session _ ref) ->
   modifyIORef ref $ \st@(St a d) -> case d of 
      Start _    -> st
      Step d _ _ -> St a d

submitText :: String -> Session -> IO (String, Bool)
submitText txt = logMsgWith fst ("Submit: " ++ txt) $ \(Session _ ref) -> do
   St a d <- readIORef ref
   case feedback a (terms d) txt of
      SyntaxError doc msug -> 
         let msg = "Parse error:\n" ++ showDoc a doc ++ maybe "" (\x -> "\nDid you mean " ++ prettyPrinter a x) msug
         in return (msg, False)
      Incorrect doc msug -> 
         let msg = showDoc a doc ++ maybe "" (\x -> "\nDid you mean " ++ prettyPrinter a x) msug
         in return (msg, False)
      Correct doc Nothing ->
         return (showDoc a doc, False)
      Correct doc (Just rule) -> do
         let new = either (error "internal error") id $ parser a txt -- REWRITE !
         writeIORef ref $ St a (Step d rule new)
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
       y = stepsRemainingA a (terms d)
   in return (x, x+y)
  
readyText :: Session -> IO String
readyText = logMsg "Ready" $ withState $ \a d -> 
   if finalProperty a (current d)
   then return "Congratulations: you have reached a solution!"
   else return "Sorry, you have not yet reached a solution"

hintText :: Session -> IO String
hintText = logMsg "Hint" $ withState $ \a d -> 
   case giveHint a (terms d) of 
      Nothing -> 
         return "Sorry, no hint available" 
      Just (doc, rule) ->
         return $ showDoc a doc

stepText :: Session -> IO String
stepText = logMsg "Step" $ withState $ \a d -> 
   case giveStep a (terms d) of
      Nothing -> 
         return "Sorry, no hint available"
      Just (doc, rule, before, after) ->
         return $ unlines 
            [ "Use rule " ++ name rule
            , "   to rewrite the term into:"
            , prettyPrinter a after
            ]

nextStep :: Session -> IO (String, Bool)
nextStep = logCurrent "Next" $ \(Session _ ref) -> do
   St a d <- readIORef ref
   case giveStep a (terms d) of
      Nothing -> 
         return ("No more steps left to do", False)
      Just (_, rule, _, new) -> do
         writeIORef ref $ St a (Step d rule new)
         return ("Successfully applied rule " ++ name rule, True)

ruleNames :: Session -> IO [String]
ruleNames = withState $ \a d -> 
   return $ map name $ filter (not . isMinorRule) $ ruleset a

getRuleAtIndex :: Int -> Session -> IO (Some Rule)
getRuleAtIndex i = withState $ \a d -> do
   let rule = filter (not . isMinorRule) (ruleset a) !! i
   return (Some rule)

applyRuleAtIndex :: Int -> [String] -> Session -> IO (String, Bool)
applyRuleAtIndex i args (Session _ ref) = do
   St a d <- readIORef ref
   let rule = filter (not . isMinorRule) (ruleset a) !! i
   case safeHead (useArguments args rule (current d)) of
      Nothing  -> return ("Could not apply rule " ++ name rule, False)
      Just new -> do 
         writeIORef ref $ St a (Step d rule new)
         return ("Successfully applied rule " ++ name rule, True)
   
{-
askForArguments :: (forall a . Rule a -> a -> IO ()) -> Int -> Session -> IO (String, Bool)
askForArguments ask i (Session _ ref) = do
   St a d <- readIORef ref
   let rule = filter (not . isMinorRule) (ruleset a) !! i
   ma <- if   hasArguments rule 
         then ask rule (current d) 
         else return () -- apply rule (current d))
   {-case ma of
      Just new -> do
         writeIORef ref $ St a (Step d rule new)
         return ("Successfully applied rule " ++ name rule, True) -}
   return ("Impossible to apply rule " ++ name rule, False) -}

--------------------------------------------------
-- Derivations

data Derivation a = Start a | Step (Derivation a) (Rule a) a -- snoc list for fast access to current term

current :: Derivation a -> a
current (Start a)    = a
current (Step _ _ a) = a

initial :: Derivation a -> a
initial (Start a)    = a
initial (Step d _ _) = initial d

-- | to do: make this function efficient (accumulating parameter)
terms :: Derivation a -> [a]
terms (Start a) = [a]
terms (Step d _ a) = terms d ++ [a]

showDerivation :: (a -> String) -> Derivation a -> String
showDerivation f (Start a)    = f a
showDerivation f (Step d r a) = showDerivation f d ++ "\n   => [" ++ name r ++ "]\n" ++ f a

derivationLength :: Derivation a -> Int
derivationLength (Start _)    = 0
derivationLength (Step d _ _) = 1 + derivationLength d

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