{-# OPTIONS -fglasgow-exts #-}
module Session 
   ( PackedAssignment(..), Assignment(..)
   , Session, makeSession, newTerm, newAssignment, progressPair, undo, submitText
   , currentText, derivationText, readyText, hintText, stepText, applyStep
   ) where

import Common.Assignment
import Common.Logging
import Common.Transformation
import Data.IORef
import Data.Maybe
import System.Time

--------------------------------------------------
-- Sessions with logging

data Session = Session String (IORef SessionState)

data SessionState = forall a . St (Assignment a) (Derivation a)

withState :: (forall a . Assignment a -> Derivation a -> IO b) -> Session -> IO b
withState f (Session _ ref) = do
   St a d <- readIORef ref
   f a d

makeSession :: PackedAssignment -> IO Session
makeSession pa = do
   logMessage "New session: "
   ref   <- newIORef (error "reference not initialized")
   let session = Session "" ref
   newAssignment pa session
   return session

newAssignment :: PackedAssignment -> Session -> IO ()
newAssignment (Pack a) = logCurrent ("New (" ++ shortTitle a ++ ")") $ 
   \(Session _ ref) -> do
      term <- randomTerm a
      writeIORef ref $ St a (Start term)

newTerm :: Session -> IO ()
newTerm session@(Session _ ref) = do
   St a _ <- readIORef ref
   newAssignment (Pack a) session
        
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

applyStep :: Session -> IO (String, Bool)
applyStep = logCurrent "Apply" $ \(Session _ ref) -> do
   St a d <- readIORef ref
   case giveStep a (terms d) of
      Nothing -> 
         return ("No more steps left to do", False)
      Just (_, rule, _, new) -> do
         writeIORef ref $ St a (Step d rule new)
         return ("Successfully applied rule " ++ name rule, True)

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