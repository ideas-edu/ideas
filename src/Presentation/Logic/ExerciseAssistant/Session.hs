module Session 
   (module Session, module Common.Assignment) where

import Common.Assignment
import Common.Transformation
import Data.IORef
import System.Time

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

giveHintSession   :: Session a -> IO (Doc a, Rule a)
giveHintSession session = do
   a <- readIORef (sessionAssignment session)
   b <- currentTerm session
   let x@(x1,x2) = giveHint a b
   logString session $ "GiveHint: " ++ prettyPrinter a b ++ showDoc a x1 ++ show x2
   return x

giveStepSession   :: Session a -> IO (Doc a, a, a)
giveStepSession session = do
   a <- readIORef (sessionAssignment session)
   b <- currentTerm session
   let x@(x1,x2,x3) = giveStep a b
   logString session $ "GiveStep: " ++ prettyPrinter a b ++ showDoc a x1 ++ prettyPrinter a x2 ++ prettyPrinter a x3
   return x

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

data Derivation a = Start a | Step (Derivation a) (Rule a) a -- snoc list for fast access to current term

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

showDerivation :: (a -> String) -> Derivation a -> String
showDerivation f (Start a)    = f a
showDerivation f (Step d r a) = showDerivation f d ++ "\n   => [" ++ name r ++ "]\n" ++ f a

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
   f (Step d _ _) = d