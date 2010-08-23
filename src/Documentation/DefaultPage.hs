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

filePathId :: HasId a => a -> FilePath
filePathId a = foldr (\x y -> x ++ "/" ++ y) (unqualified a) (qualifiers a)

------------------------------------------------------------
-- Paths and files

exerciseOverviewPageFile, exerciseOverviewAllPageFile, 
   serviceOverviewPageFile, testsPageFile :: String

exerciseOverviewPageFile    = "exercises.html"
exerciseOverviewAllPageFile = "exercises-all.html"
serviceOverviewPageFile     = "services.html"
testsPageFile               = "tests.html"

exercisePageFile, exerciseDerivationsFile, exerciseStrategyFile,
   exerciseRulesFile, exerciseDiagnosisFile :: HasId a => a -> FilePath
exercisePageFile        a = filePathId a ++ ".html"
exerciseDerivationsFile a = filePathId a ++ "-derivations.html"
exerciseStrategyFile    a = filePathId a ++ "-strategy.html"
exerciseRulesFile       a = filePathId a ++ "-rules.html"
exerciseDiagnosisFile   a = filePathId a ++ "-diagnosis.html"

ruleImageFile :: Exercise a -> Rule (Context a) -> String
ruleImageFile ex r = unqualified ex ++ unqualified r ++ ".png"

servicePageFile :: Service -> String
servicePageFile srv = "services/" ++ filePathId srv ++ ".html"

diagnosisExampleFile :: Id -> String
diagnosisExampleFile a = filePathId a ++ "-examples.txt"

------------------------------------------------------------
-- Utility functions

showBool :: Bool -> String 
showBool b = if b then "yes" else "no"