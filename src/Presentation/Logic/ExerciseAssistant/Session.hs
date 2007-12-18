{-# OPTIONS -fglasgow-exts #-}
module Session 
   ( PackedAssignment(..), Assignment(..)
   , Session, makeSession, newTerm, newAssignment, progressPair, undo, submitText
   , currentText, derivationText, readyText, hintText, stepText
   ) where

import Common.Assignment
import Common.Transformation
import Data.IORef
import Data.Maybe
import System.Time

data Session = Session String (IORef SessionState)

data SessionState = forall a . St (Assignment a) (Derivation a)

data Derivation a = Start a | Step (Derivation a) (Rule a) a -- snoc list for fast access to current term

withState :: (forall a . Assignment a -> Derivation a -> IO b) -> Session -> IO b
withState f (Session _ ref) = do
   St a d <- readIORef ref
   f a d

current :: Derivation a -> a
current (Start a)    = a
current (Step _ _ a) = a

initial :: Derivation a -> a
initial (Start a)    = a
initial (Step d _ _) = initial d

showDerivation :: (a -> String) -> Derivation a -> String
showDerivation f (Start a)    = f a
showDerivation f (Step d r a) = showDerivation f d ++ "\n   => [" ++ name r ++ "]\n" ++ f a

derivationLength :: Derivation a -> Int
derivationLength (Start _)    = 0
derivationLength (Step d _ _) = 1 + derivationLength d

makeSession :: PackedAssignment -> IO Session
makeSession (Pack a) = do
   clock <- getClockTime
   term  <- randomTerm a
   ref   <- newIORef $ St a (Start term)
   return (Session (show clock) ref)

newAssignment :: PackedAssignment -> Session -> IO ()
newAssignment (Pack a) (Session _ ref) = do
   term <- randomTerm a
   writeIORef ref $ St a (Start term)

newTerm :: Session -> IO ()
newTerm session@(Session _ ref) = do
   St a _ <- readIORef ref
   newAssignment (Pack a) session
        
undo :: Session -> IO ()
undo (Session _ ref) = 
   modifyIORef ref $ \st@(St a d) -> case d of 
      Start _    -> st
      Step d _ _ -> St a d

submitText :: String -> Session -> IO (String, Bool)
submitText txt (Session _ ref) = do
   St a d <- readIORef ref
   case feedback a (current d) txt of
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
       y = stepsRemaining a (current d)
   in return (x, x+y)
  
readyText :: Session -> IO String
readyText = withState $ \a d -> 
   if finalProperty a (current d)
   then return "Congratulations: you have reached a solution!"
   else return "Sorry, you have not yet reached a solution"

hintText :: Session -> IO String
hintText = withState $ \a d -> 
   let (doc, rule) =  giveHint a (current d)
   in return $ "Use rule " ++ showDoc a doc

stepText :: Session -> IO String
stepText = withState $ \a d -> 
   let (doc, before, after) =  giveStep a (current d)
   in return $ unlines 
         [ "Use rule " ++ showDoc a doc ++ " to rewrite the term"
         , prettyPrinter a before 
         , "into"
         , prettyPrinter a after
         ]

{-
--------------------------------------------------
-- Sessions with logging

data Session a = S 
   { sessionID         :: SessionID
   , sessionAssignment :: IORef (Assignment a)
   , sessionDerivation :: IORef (Derivation a)
   }

type SessionID = String

newSession :: Assignment a -> IO (Session a)
newSession a = do
   clock <- getClockTime
   term  <- randomTerm a
   r1    <- newIORef a
   r2    <- newIORef (Start term)
   let session = S (show clock) r1 r2
   logString session $ "NewSession" ++ prettyPrinter a term
   return session
   
newTermSession :: Session a -> IO ()
newTermSession session = do
   a    <- readIORef (sessionAssignment session)
   term <- randomTerm a
   logString session $ "NewTerm: " ++ prettyPrinter a term
   writeIORef (sessionDerivation session) (Start term)

giveHintSession   :: Session a -> IO String
giveHintSession session = do
   a <- readIORef (sessionAssignment session)
   b <- currentTerm session
   let x@(x1,x2) = giveHint a b
   logString session $ "GiveHint: " ++ prettyPrinter a b ++ showDoc a x1 ++ show x2
   return (showDoc a x1)

giveStepSession   :: Session a -> IO String
giveStepSession session = do
   a <- readIORef (sessionAssignment session)
   b <- currentTerm session
   let x@(x1,x2,x3) = giveStep a b
   logString session $ "GiveStep: " ++ prettyPrinter a b ++ showDoc a x1 ++ prettyPrinter a x2 ++ prettyPrinter a x3
   return $ "Use " ++ showDoc a x1 ++ "\nto rewrite subterm\n" ++ 
            prettyPrinter a x2 ++ "\nresulting in\n" ++
            prettyPrinter a x3

feedbackSession   :: Session a -> String -> IO (Feedback a)
feedbackSession session txt = do
   a <- readIORef (sessionAssignment session)
   b <- currentTerm session
   let x = feedback a b txt
   logString session $ "Feedback: " ++ prettyPrinter a b ++ txt ++ showFeedback a x
   return x

logString :: Session a -> String -> IO ()
logString session s = rec 
 where
   msg = (sessionID session ++ ": " ++ s ++ "\n")
   rec = appendFile "logfile" msg 
            `catch` \_ -> rec

showFeedback :: Assignment a -> Feedback a -> String
showFeedback a feedback =
   case feedback of
      SyntaxError d ma -> 
         "SyntaxError " ++ showDoc a d ++ showMaybe (prettyPrinter a) ma
      Incorrect d ma ->
         "Incorrect " ++ showDoc a d ++ showMaybe (prettyPrinter a) ma
      Correct d mr ->
         "Correct " ++ showDoc a d ++ showMaybe name mr
               
showMaybe :: (a -> String) -> Maybe a -> String
showMaybe f = maybe "Nothing" (\a -> "Just " ++ f a)

currentTerm :: Session a -> IO a
currentTerm session = readIORef (sessionDerivation session) >>= f
 where
   f (Start a)    = return a
   f (Step _ _ a) = return a

initialTerm :: Session a -> IO a
initialTerm session = readIORef (sessionDerivation session) >>= f
 where
   f (Start a)    = return a
   f (Step d _ _) = f d

getDerivationText :: Session a -> IO String
getDerivationText session = do
   a <- readIORef (sessionAssignment session)
   d <- readIORef (sessionDerivation session)
   return (showDerivation (prettyPrinter a) d)

derivationStep :: Session a -> Rule a -> a -> IO ()
derivationStep session r term =
   modifyIORef (sessionDerivation session) (\d -> Step d r term)

derivationUndo :: Session a -> IO ()
derivationUndo session =
   modifyIORef (sessionDerivation session) f
 where
   f d@(Start _)  = d
   f (Step d _ _) = d -}