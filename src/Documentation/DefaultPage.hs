-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Documentation.DefaultPage where

import Common.Context
import Common.Exercise
import Common.Transformation
import Control.Monad
import Service.DomainReasoner
import Service.Types
import System.Directory
import System.FilePath
import Text.HTML
import qualified Text.XML as XML
import Data.Char

generatePage :: String -> String -> HTMLBuilder -> DomainReasoner ()
generatePage = generatePageAt 0

generatePageAt :: Int -> String -> String -> HTMLBuilder -> DomainReasoner ()
generatePageAt n dir txt body = do
   version <- getFullVersion
   let filename = dir ++ "/" ++ txt
       dirpart  = takeDirectory filename
       doc      = defaultPage version (findTitle body) n body
   liftIO $ do
      putStrLn $ "Generating " ++ filename
      unless (null dirpart) (createDirectoryIfMissing True dirpart)
      writeFile filename (showHTML doc)

defaultPage :: String -> String -> Int -> HTMLBuilder -> HTML
defaultPage version title level builder = 
   htmlPage title (Just (up level ++ "ideas.css")) $ do
      header level
      builder
      footer version

header :: Int -> HTMLBuilder
header level = center $ do
   let f m = text "[" >> space >> m >> space >> text "]"
   f $ link (up level ++ exerciseOverviewPageFile) $ text "Exercises"
   replicateM_ 5 space
   f $ link (up level ++ "services.html")  $ text "Services"
   replicateM_ 5 space
   f $ link (up level ++ "tests.html")  $ text "Tests"
   replicateM_ 5 space
   f $ link (up level ++ "coverage/hpc_index.html")  $ text "Coverage"
   replicateM_ 5 space
   f $ link (up level ++ "api/index.html")  $ text "API"
   hr

footer :: String -> HTMLBuilder
footer version = do 
   hr 
   italic $ text $ "Automatically generated from sources: " ++ version

up :: Int -> String
up = concat . flip replicate "../"

findTitle :: HTMLBuilder -> String
findTitle = maybe "" XML.getData . XML.findChild "h1" . XML.makeXML "page"

------------------------------------------------------------
-- Paths and files

ruleImagePath :: Exercise a -> String
ruleImagePath ex = "exercises/" ++ f (domain (exerciseCode ex)) ++ "/" ++ f (description ex) ++ "/"
 where f = filter isAlphaNum . map toLower

exercisePagePath :: ExerciseCode -> String
exercisePagePath code = "exercises/" ++ domain code ++ "/"

servicePagePath :: String
servicePagePath = "services/" 

ruleImageFile :: Exercise a -> Rule (Context a) -> String
ruleImageFile ex r = ruleImagePath ex ++ "rule" ++ showId r ++ ".png"

ruleImageFileHere :: Exercise a -> Rule (Context a) -> String
ruleImageFileHere ex r = 
   filter (not . isSpace) (identifier (exerciseCode ex)) 
   ++ "/rule" ++ filter isAlphaNum (showId r) ++ ".png"

exerciseOverviewPageFile :: String
exerciseOverviewPageFile = "exercises.html"

exerciseOverviewAllPageFile :: String
exerciseOverviewAllPageFile = "exercises-all.html"

serviceOverviewPageFile :: String
serviceOverviewPageFile = "services.html"

exercisePageFile :: ExerciseCode -> String
exercisePageFile code = 
   exercisePagePath code 
   ++ filter (not . isSpace) (identifier code) 
   ++ ".html"

exerciseStrategyFile :: ExerciseCode -> String
exerciseStrategyFile code = 
   exercisePagePath code
   ++ filter (not . isSpace) (identifier code)
   ++ "-strategy.html"

exerciseRulesFile :: ExerciseCode -> String
exerciseRulesFile code = 
   exercisePagePath code
   ++ filter (not . isSpace) (identifier code)
   ++ "-rules.html"

exerciseDerivationsFile :: ExerciseCode -> String
exerciseDerivationsFile code = 
   exercisePagePath code
   ++ filter (not . isSpace) (identifier code)
   ++ "-derivations.html"

servicePageFile :: Service -> String
servicePageFile srv = servicePagePath ++ serviceName srv ++ ".html"

testsPageFile :: String
testsPageFile = "tests.html"

------------------------------------------------------------
-- Utility functions

showBool :: Bool -> String 
showBool b = if b then "yes" else "no"