{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Manages links to information
--
-----------------------------------------------------------------------------

module Ideas.Encoding.LinkManager
   ( LinkManager(..), makeLinkManager
   , stateToXML
   -- , pathLevel, (</>)
     -- links to services and exercises
   , linkToIndex, linkToExercises, linkToServices, linkToService
     -- links to exercise information
   , linkToExercise, linkToStrategy, linkToRules, linkToExamples
   , linkToDerivations, linkToRule, linkToRandomExample, linkToTestReport
     -- links to state information (dynamic)
   , linkToState, linkToFirsts, linkToApplications, linkToDerivation
   , linkToMicrosteps
   , escapeInURL
   ) where

import Data.Either
import Ideas.Common.Library
import Ideas.Encoding.EncoderXML
import Ideas.Encoding.Options
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Text.HTML
import Ideas.Text.XML
import Ideas.Utils.Decoding

data LinkManager = LinkManager
   { urlForCSS           :: String -> String
   , urlForImage         :: String -> String
   , urlForRequest       :: String
     -- links to services and exercises
   , urlForIndex         :: String
   , urlForExercises     :: String
   , urlForServices      :: String
   , urlForService       :: Service -> String
     -- links to exercise information
   , urlForExercise      :: forall a . Exercise a -> String
   , urlForStrategy      :: forall a . Exercise a -> String
   , urlForRules         :: forall a . Exercise a -> String
   , urlForConstraints   :: forall a . Exercise a -> String
   , urlForExamples      :: forall a . Exercise a -> String
   , urlForDerivations   :: forall a . Exercise a -> String
   , urlForRule          :: forall a . Exercise a -> Rule (Context a) -> String
   , urlForTestReport    :: forall a . Exercise a -> String
     -- dynamic exercise information
   , urlForRandomExample :: forall a . Exercise a -> Difficulty -> String
     -- dynamic state information
   , urlForState         :: forall a . State a -> String
   , urlForFirsts        :: forall a . State a -> String
   , urlForApplications  :: forall a . State a -> String
   , urlForDerivation    :: forall a . State a -> String
   , urlForMicrosteps    :: forall a . State a -> String
   }

---------------------------------------------------------------------
-- links to services and exercises

linkToIndex :: LinkManager -> HTMLBuilder -> HTMLBuilder
linkToIndex = linkWith urlForIndex

linkToExercises :: LinkManager -> HTMLBuilder -> HTMLBuilder
linkToExercises = linkWith urlForExercises

linkToServices :: LinkManager -> HTMLBuilder -> HTMLBuilder
linkToServices = linkWith urlForServices

linkToService :: LinkManager -> Service -> HTMLBuilder -> HTMLBuilder
linkToService = linkWith . urlForService

---------------------------------------------------------------------
-- links to exercise information

linkToExercise :: LinkManager -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToExercise = linkWith . urlForExercise

linkToStrategy :: LinkManager -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToStrategy = linkWith . urlForStrategy

linkToRules :: LinkManager -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToRules = linkWith . urlForRules

linkToExamples :: LinkManager -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToExamples = linkWith . urlForExamples

linkToDerivations :: LinkManager -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToDerivations = linkWith . urlForDerivations

linkToRule :: LinkManager -> Exercise a -> Rule (Context a) -> HTMLBuilder -> HTMLBuilder
linkToRule lm = linkWith . urlForRule lm

linkToTestReport :: LinkManager -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToTestReport = linkWith . urlForTestReport

---------------------------------------------------------------------
-- dynamic exercise information

linkToRandomExample :: LinkManager -> Exercise a -> Difficulty -> HTMLBuilder -> HTMLBuilder
linkToRandomExample lm = linkWith . urlForRandomExample lm

---------------------------------------------------------------------
-- links to state information (dynamic)

linkToState :: LinkManager -> State a -> HTMLBuilder -> HTMLBuilder
linkToState = linkWith . urlForState

linkToFirsts :: LinkManager -> State a -> HTMLBuilder -> HTMLBuilder
linkToFirsts = linkWith . urlForFirsts

linkToMicrosteps :: LinkManager -> State a -> HTMLBuilder -> HTMLBuilder
linkToMicrosteps = linkWith . urlForMicrosteps

linkToApplications :: LinkManager -> State a -> HTMLBuilder -> HTMLBuilder
linkToApplications = linkWith . urlForApplications

linkToDerivation :: LinkManager -> State a -> HTMLBuilder -> HTMLBuilder
linkToDerivation = linkWith . urlForDerivation

---------------------------------------------------------------------
-- Dynamic links

makeLinkManager :: String -> String -> LinkManager
makeLinkManager base cgiBinary = LinkManager
   { urlForRequest   = prefix
   , urlForCSS       = \s -> base ++ "css/" ++ s
   , urlForImage     = \s -> base ++ "images/" ++ s
   , urlForIndex     = url $ simpleRequest "index"
   , urlForExercises = url $ simpleRequest "exerciselist"
   , urlForServices  = url $ simpleRequest "servicelist"
   , urlForService   =
        url . makeRequest "serviceinfo" . tag "location" . text
   , urlForExercise    = url . exerciseRequest "exerciseinfo"
   , urlForStrategy    = url . exerciseRequest "strategyinfo"
   , urlForRules       = url . exerciseRequest "rulelist"
   , urlForConstraints = url . exerciseRequest "constraintlist"
   , urlForTestReport  = url . exerciseRequest "testreport"
   , urlForExamples    = url . exerciseRequest "examples"
   , urlForDerivations = url . exerciseRequest "examplederivations"
   , urlForRule = \ex r ->
        url $ exerciseRequestWith "ruleinfo" ex $
           tag "ruleid" $ text r
   , urlForRandomExample = \ex d ->
        url $ exerciseRequestWith "generate" ex $
           "difficulty" .=. show d
   , urlForState        = url . stateRequest "stateinfo"
   , urlForFirsts       = url . stateRequest "allfirsts"
   , urlForApplications = url . stateRequest "allapplications"
   , urlForDerivation   = url . stateRequest "derivation"
   , urlForMicrosteps   = url . stateRequest "microsteps"
   }
 where
   prefix  = cgiBinary ++ "?input="
   url req = prefix ++ compactXML req

simpleRequest :: String -> XML
simpleRequest s = makeRequest s mempty

makeRequest :: String -> XMLBuilder -> XML
makeRequest s rest = makeXML "request" $
   ("service"  .=. s) <>
   ("encoding" .=. "html") <>
   rest

exerciseRequest :: String -> Exercise a -> XML
exerciseRequest s ex = makeRequest s ("exerciseid" .=. showId ex)

exerciseRequestWith :: String -> Exercise a -> XMLBuilder -> XML
exerciseRequestWith s ex rest =
   makeRequest s (("exerciseid" .=. showId ex) <> rest)

stateRequest :: String -> State a -> XML
stateRequest s st =
   exerciseRequestWith s (exercise st) (stateToXML st)

-- assume nothing goest wrong
stateToXML :: State a -> XMLBuilder
stateToXML st = fromRight (error "LinkManager: Invalid state") $ 
   runEncoder (encodeState st) (exercise st, optionHtml mempty)

linkWith :: (a -> String) -> a -> HTMLBuilder -> HTMLBuilder
linkWith f = link . escapeInURL . f

escapeInURL :: String -> String
escapeInURL = concatMap f
 where
   f '+' = "%2B"
   f '>' = "%3E"
   f '&' = "%26"
   f '%' = "%25"
   f '#' = "%23"
   f ';' = "%3B"
   f c   = [c]