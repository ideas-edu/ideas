module Main (main) where

import Common.Assignment
import Common.Strategy (reportLocations)
import OpenMath.StrategyTable
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
      _        -> putStrLn "Usage: MakeWikiPages (optional: target directory)"

make :: (Maybe String) -> IO ()
make target =
   mapM_ (makeWikiFile $ fromMaybe "." target) strategyTable

makeWikiFile :: String -> StrategyEntry -> IO ()
makeWikiFile target (Entry nr (ExprAssignment a) xs) = do
   code <- insertCode a xs
   let filename = target ++ "/" ++ targetFileName a
   putStrLn $ "Writing to " ++ show filename
   writeFile filename $ unlines $ intersperse "" 
      [ makeTitle nr a
      , backRefText
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

targetFileName :: Assignment a -> String
targetFileName a =
   filter isAlphaNum (shortTitle a) ++ ".txt"


makeTitle :: String -> Assignment a -> String
makeTitle nr a = title $ "Strategy " ++ nr ++ ": " ++ shortTitle a

makeLocationTable :: Assignment a -> String
makeLocationTable a = table ["Location", "Label or rule"] (map f $ reportLocations $ strategy a)
 where f (loc, s) = [show loc, s]
 
strategyFileName = "src/Domain/LinearAlgebra/Strategies.hs"

insertCode :: Assignment a -> [String] -> IO String
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