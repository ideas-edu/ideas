{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
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
--  $Id$

module Ideas.Service.State
   ( -- * Exercise state
     State, makeState, makeNoState, emptyStateContext, emptyState
   , exercise, statePrefix, stateContext, stateTerm
   , stateUser, stateSession, stateStartTerm
   , withoutPrefix, stateLabels, suitable, finished, firsts, microsteps
   ) where

import Data.Function
import Data.List
import Data.Maybe
import Ideas.Common.Library hiding (suitable, ready, (:~>))
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence

data State a = State
   { exercise       :: Exercise a
   , statePrefix    :: Prefix (Context a)
   , stateContext   :: Context a
   , stateUser      :: Maybe String
   , stateSession   :: Maybe String -- min 40 bits (10 hex)
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
   type Elem (State a) = (Step (Context a), Context a)

   firsts st = firstsOrdered cmp st
    where
      cmp = ruleOrdering (exercise st) `on` (stepRule . fst)

   menu st = fmap f (menu (majorPrefix (statePrefix st)))
    where
      f Done = Done
      f (info :~> p) = info :~> st {statePrefix = p, stateContext = snd info}

microsteps :: State a -> [((Step (Context a), Context a), State a)]
microsteps st = concatMap f (bests (menu (statePrefix st)))
 where
   f Done         = []
   f (info :~> p) = [(info, st {statePrefix = p, stateContext = snd info})]

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
      in nub [l | Enter l <- xs] \\ [l | Exit l <- xs]