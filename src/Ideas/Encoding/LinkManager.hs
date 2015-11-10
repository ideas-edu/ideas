{-# LANGUAGE RankNTypes #-}
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
-- Manages links to information
--
-----------------------------------------------------------------------------

module Ideas.Encoding.LinkManager
   ( LinkManager(..)
   , dynamicLinks, stateToXML
   , staticLinks, linksUp, pathLevel, (</>)
     -- links to services and exercises
   , linkToIndex, linkToExercises, linkToServices, linkToService
     -- links to exercise information
   , linkToExercise, linkToStrategy, linkToRules, linkToExamples
   , linkToDerivations, linkToRule, linkToRandomExample, linkToTestReport
     -- links to state information (dynamic)
   , linkToState, linkToFirsts, linkToApplications, linkToDerivation
   , linkToMicrosteps
   ) where

import Data.Maybe
import Ideas.Common.Library
import Ideas.Encoding.Encoder
import Ideas.Encoding.EncoderXML
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Text.HTML
import Ideas.Text.XML

data LinkManager = LinkManager
   { urlForCSS           :: String -> String
   , urlForImage         :: String -> String
   , urlForRequest       :: String
   , isStatic            :: Bool
     -- links to services and exercises
   , urlForIndex         :: String
   , urlForExercises     :: String
   , urlForServices      :: String
   , urlForService       :: Service -> String
     -- links to exercise information
   , urlForExercise      :: forall a . Exercise a -> String
   , urlForStrategy      :: forall a . Exercise a -> String
   , urlForRules         :: forall a . Exercise a -> String
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

dynamicLinks :: String -> LinkManager
dynamicLinks cgiBinary = LinkManager
   { isStatic        = False
   , urlForRequest   = prefix
   , urlForCSS       = ("http://ideas.cs.uu.nl/css/" ++)
   , urlForImage     = ("http://ideas.cs.uu.nl/images/" ++)
   , urlForIndex     = url $ simpleRequest "index"
   , urlForExercises = url $ simpleRequest "exerciselist"
   , urlForServices  = url $ simpleRequest "servicelist"
   , urlForService   =
        url . makeRequest "serviceinfo" . tag "location" . text
   , urlForExercise    = url . exerciseRequest "exerciseinfo"
   , urlForStrategy    = url . exerciseRequest "strategyinfo"
   , urlForRules       = url . exerciseRequest "rulelist"
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
stateRequest s state =
   exerciseRequestWith s (exercise state) (stateToXML state)

-- assume nothing goest wrong
stateToXML :: State a -> XMLBuilder
stateToXML st = fromMaybe (error "LinkManager: Invalid state") $
   run encodeState (simpleOptions (exercise st)) st

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

---------------------------------------------------------------------
-- Static links

staticLinks :: LinkManager
staticLinks = LinkManager
   { isStatic        = True
   , urlForCSS       = id
   , urlForImage     = id
   , urlForRequest   = ""
   , -- links to services and exercises
     urlForIndex     = "index.html"
   , urlForExercises = "exercises.html"
   , urlForServices  = "services.html"
   , urlForService   = \srv -> "services" </> idToFilePath srv
     -- links to exercise information
   , urlForExercise    = idToFilePath
   , urlForStrategy    = idToFilePathWith "-strategy.html"
   , urlForRules       = idToFilePathWith "-rules.html"
   , urlForExamples    = idToFilePathWith "-examples.html"
   , urlForDerivations = idToFilePathWith "-derivations.html"
   , urlForRule        = \ex r -> idToFilePathWith ("/" ++ showId r ++ ".html") ex
   , urlForTestReport  = idToFilePathWith "-testreport.html"
     -- dynamic exercise information
   , urlForRandomExample = \_ _ -> ""
     -- dynamic state information
   , urlForState        = const ""
   , urlForFirsts       = const ""
   , urlForApplications = const ""
   , urlForDerivation   = const ""
   , urlForMicrosteps   = const ""
   }

linksUp :: Int -> LinkManager -> LinkManager
linksUp n lm = lm
   { isStatic        = isStatic lm
     -- links to services and exercises
   , urlForIndex     = f0 urlForIndex
   , urlForExercises = f0 urlForExercises
   , urlForServices  = f0 urlForServices
   , urlForService   = f1 urlForService
     -- links to exercise information
   , urlForExercise    = f1 urlForExercise
   , urlForStrategy    = f1 urlForStrategy
   , urlForRules       = f1 urlForRules
   , urlForExamples    = f1 urlForExamples
   , urlForDerivations = f1 urlForDerivations
   , urlForRule        = f2 urlForRule
     -- dynamic exercise information
   , urlForRandomExample = f2 urlForRandomExample
     -- dynamic state information
   , urlForState        = f1 urlForState
   , urlForFirsts       = f1 urlForFirsts
   , urlForApplications = f1 urlForApplications
   , urlForDerivation   = f1 urlForDerivation
   , urlForMicrosteps   = f1 urlForMicrosteps
   }
 where
   f0 g   = pathUp n $ g lm
   f1 g   = pathUp n . g lm
   f2 g x = pathUp n . g lm x

pathUp :: Int -> FilePath -> FilePath
pathUp n file = concat (replicate n "../") ++ file

pathLevel :: FilePath -> Int
pathLevel = length . filter (=='/')

idToFilePath :: HasId a => a -> FilePath
idToFilePath = idToFilePathWith ".html"

idToFilePathWith :: HasId a => String -> a -> FilePath
idToFilePathWith suffix a = foldr (</>) (unqualified a ++ suffix) (qualifiers a)

(</>) :: String -> FilePath -> FilePath
x </> y = x ++ "/" ++ y