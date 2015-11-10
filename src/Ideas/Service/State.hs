{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- The information maintained for a learner trying to complete a
-- derivation.
--
-----------------------------------------------------------------------------

module Ideas.Service.State
   ( -- * Exercise state
     State, startState, makeState, makeNoState, emptyStateContext, emptyState
   , exercise, statePrefix, stateContext, stateTerm
   , stateUser, stateSession, stateStartTerm, restart
   , withoutPrefix, stateLabels, suitable, finished, firsts, microsteps
   ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Ideas.Common.Library hiding (suitable, ready, (:~>))
import Ideas.Common.Strategy.Prefix
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Symbol
import System.Random

data State a = State
   { exercise       :: Exercise a
   , statePrefix    :: Prefix (Context a)
   , stateContext   :: Context a
   , stateUser      :: Maybe String
   , stateSession   :: Maybe String
   , stateStartTerm :: Maybe String
   }

instance Show (State a) where
   show s = unlines $ "State {" : map ("   "++) xs ++ ["}"]
    where
      xs = [ "exercise  = " ++ showId s
           , "prefix    = " ++ show (statePrefix s)
           , "term      = " ++ prettyPrinterContext (exercise s) (stateContext s)
           , "user      = " ++ show (stateUser s)
           , "session   = " ++ show (stateSession s)
           , "startterm = " ++ show (stateStartTerm s)
           ]

instance HasId (State a) where
   getId = getId . exercise
   changeId f s = s { exercise = changeId f (exercise s) }

instance HasEnvironment (State a) where
   environment = environment . stateContext
   setEnvironment env s =
      s { stateContext = setEnvironment env (stateContext s) }

instance Firsts (State a) where
   type Elem (State a) = (Rule (Context a), Context a, Environment)

   ready  = ready . majorPrefix . statePrefix
   firsts = firstsWith (majorPrefix . statePrefix)

microsteps :: State a -> [((Rule (Context a), Context a, Environment), State a)]
microsteps = firstsWith statePrefix

firstsWith :: (State a -> Prefix (Context a)) -> State a -> [((Rule (Context a), Context a, Environment), State a)]
firstsWith getPrefix st = map f (firstsOrdered cmp (getPrefix st))
 where
   cmp = ruleOrdering (exercise st)
   f ((r, a, env), pr) = ((r, a, env), st {statePrefix = pr, stateContext = a})

stateTerm :: State a -> a
stateTerm = fromMaybe (error "invalid term") . fromContext . stateContext

-----------------------------------------------------------

makeState :: Exercise a -> Prefix (Context a) -> Context a -> State a
makeState ex prf ctx = State ex prf ctx Nothing Nothing Nothing

-- State without a prefix
makeNoState :: Exercise a -> Context a -> State a
makeNoState = flip makeState noPrefix

emptyStateContext :: Exercise a -> Context a -> State a
emptyStateContext ex ca =
   let pr = emptyPrefix (strategy ex) ca
   in makeState ex pr ca

emptyState :: Exercise a -> a -> State a
emptyState ex = emptyStateContext ex . inContext ex

startState :: Exercise a -> Maybe String -> a -> IO (State a)
startState ex userId a = do
   sid <- newSessionId
   return (emptyStateContext ex (inContext ex a))
      { stateUser      = userId
      , stateSession   = Just sid
      , stateStartTerm = Just (prettyPrinter ex a)
      }

-- Restart the strategy: make sure that the new state has a prefix
-- When resetting the prefix, also make sure that the context is refreshed
restart :: State a -> State a
restart state
   | canBeRestarted ex = state
        { stateContext = ctx
        , statePrefix  = emptyPrefix (strategy ex) ctx
        }
   | otherwise = state
 where
   ex  = exercise state
   ctx = inContext ex (stateTerm state)

withoutPrefix :: State a -> Bool
withoutPrefix = null . prefixPaths . statePrefix

suitable :: State a -> Bool
suitable st = isSuitable (exercise st) (stateTerm st)

finished :: State a -> Bool
finished st = isReady (exercise st) (stateTerm st)

stateLabels :: State a -> [[Id]]
stateLabels st = map make (prefixPaths (statePrefix st))
 where
   ex = exercise st
   make path =
      let (xs, _) = replayPath path (strategy ex) (stateContext st)
      in nub (mapMaybe isEnterRule xs) \\ mapMaybe isExitRule xs

-- | Produces a 80 bit random number, represented as 20 hexadecimal digits
newSessionId :: IO String
newSessionId = replicateM 20 $ do
   n <- randomRIO (0 :: Int, 15)
   return (hex n)
 where
   hex :: Int -> Char
   hex n | n < 10    = chr (n+48)
         | otherwise = chr (n+87)