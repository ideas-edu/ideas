-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
import Service.ServiceList
import Service.Revision
import System.Environment
import System.Directory
import System.FilePath
import Text.HTML
import Data.Char

generatePage :: String -> HTML -> IO ()
generatePage txt doc = do
   dir <- targetDirectory
   let filename = dir ++ "/" ++ txt
       dirpart  = takeDirectory filename
   putStrLn $ "Generating " ++ filename
   unless (null dirpart) (createDirectoryIfMissing True dirpart)
   writeFile filename (showHTML doc)

defaultPage :: String -> Int -> HTMLBuilder -> HTML
defaultPage title level builder = htmlPage title (Just (up level ++ "ideas.css")) $ do
   header level
   builder
   footer 

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

footer :: HTMLBuilder
footer = do 
   hr 
   italic $ text $ "Automatically generated from sources: version " ++
      version ++ "  (revision " ++ show revision ++ ", " ++ lastChanged ++ ")"

up :: Int -> String
up = concat . flip replicate "../"

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
ruleImageFile ex r = ruleImagePath ex ++ "rule" ++ name r ++ ".png"

ruleImageFileHere :: Exercise a -> Rule (Context a) -> String
ruleImageFileHere ex r = 
   filter (not . isSpace) (identifier (exerciseCode ex)) 
   ++ "/rule" ++ filter isAlphaNum (name r) ++ ".png"

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

servicePageFile :: Service a -> String
servicePageFile srv = servicePagePath ++ serviceName srv ++ ".html"

------------------------------------------------------------
-- Utility functions

showBool :: Bool -> String 
showBool b = if b then "yes" else "no"

targetDirectory :: IO String
targetDirectory = do
   args <- getArgs
   case args of
      [dir] -> return dir
      _     -> return "docs"