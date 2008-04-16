-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Main (main) where

import Common.Exercise
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Utils (Some(..))
import OpenMath.StrategyTable
import OpenMath.ObjectParser
import OpenMath.Request
import Data.Char
import Data.List
import Data.Maybe
import System.Environment

main :: IO ()
main = do
   args <- getArgs
   case args of 
      []       -> make Nothing
      [target] -> make (Just target)
      _        -> putStrLn "Usage: wikipages (optional: target directory)"

make :: (Maybe String) -> IO ()
make target =
   mapM_ (makeWikiFile $ fromMaybe "." target) strategyTable

makeWikiFile :: String -> StrategyEntry -> IO ()
makeWikiFile target (Entry nr (Some (ExprExercise a)) xs examples) = do
   code <- insertCode a xs
   let filename = target ++ "/" ++ targetFileName a
   putStrLn $ "Writing to " ++ show filename
   writeFile filename $ unlines $ intersperse "" 
      [ makeTitle nr a
      , backRefText
      , section "Examples"
      , makeExamples (shortTitle a) examples
      , section "Strategy code"
      , code
      , visitRepoText
      , section "Strategy locations"
      , makeLocationTable a
      , generatedText
      ]

backRefText   = "Back to the [[FeedbackService|main page]]."
visitRepoText = "Visit the repository [http://ideas.cs.uu.nl/trac/browser/Feedback/trunk] for the full code base."
generatedText = "The locations have been generated automatically from the strategy code."

targetFileName :: Exercise a -> String
targetFileName a =
   filter isAlphaNum (shortTitle a) ++ ".txt"

makeTitle :: String -> Exercise a -> String
makeTitle nr a = title $ "Strategy " ++ nr ++ ": " ++ shortTitle a

makeLocationTable :: Exercise a -> String
makeLocationTable a = table ["Location", "Label or rule"] (map f $ reportLocations $ strategy a)
 where f (loc, s) = [show loc, s]

reportLocations :: LabeledStrategy a -> [(StrategyLocation, String)]
reportLocations = map f . strategyLocations
 where 
   f (loc, e) = (loc, either forStrategy forRule e)
   forStrategy s
      | all isMinorRule (rulesInStrategy s) = strategyName s ++ " (skipped)"
      | otherwise                           = strategyName s
   forRule r
      | hasArguments r = name r ++ " (parameterized rule)"
      | otherwise      = name r ++ " (rule)"

strategyFileName = "src/Domain/LinearAlgebra/Strategies.hs"

insertCode :: Exercise a -> [String] -> IO String
insertCode a xs = do
   program <- readFile strategyFileName
   let functions = map snd (reportLocations (strategy a)) ++ xs
       blocks    = makeBlocks (lines program)
       check x   = any (~= x) functions 
       pieces    = map (unlines . snd) $ filter (check . fst) blocks
   return (unlines pieces)

(~=) :: String -> String -> Bool
x ~= y = f x == f y
 where f = map toUpper . filter isAlphaNum

makeBlocks :: [String] -> [(String, [String])]
makeBlocks [] = []
makeBlocks (x:xs)
   | null fn   = makeBlocks xs
   | otherwise = let (ys, zs) = break check xs
                     check []       = True
                     check xs@(x:_) = not (isSpace x) && takeWhile isAlpha xs /= fn
                     list = map (' ':) $ filter (not . all isSpace) (x:ys)
                 in (fn, list) : makeBlocks zs
 where
   fn = takeWhile isAlphaNum x

makeExamples :: String -> [Expr] -> String
makeExamples name = unlines . map (f . oneliner . ppRequest . makeRequest name)
 where f s = "* Example [" ++ defaultURL True ++ escapeForWiki s ++ "]"

makeRequest :: String -> Expr -> Request
makeRequest name term = Request 
   { req_Strategy = name
   , req_Location = []
   , req_Term     = term
   , req_Context  = Nothing
   , req_Answer   = Nothing
   }

---------------------------------------------------------------
-- Wiki formatting

title :: String -> String
title s = "==" ++ s ++ "=="

section :: String -> String
section s = "-------------\n===" ++ s ++ "==="

table :: [String] -> [[String]] -> String
table xs xss = unlines $ 
   "{| border=\"1\"" :
   map ("! "++) xs ++
   concat (map (("|-":) . map ("| "++)) xss) ++
   ["|}"]
   
escapeForWiki :: String -> String
escapeForWiki = concatMap f 
 where
   f ' ' = "%20" -- dec: 32
   f '"' = "%22" -- dec: 34
   f '<' = "%3c" -- dec: 60
   f '>' = "%3e" -- dec: 62
   f '[' = "%5b" -- dec: 91
   f ']' = "%5d" -- dec: 93
   f c   = [c]
