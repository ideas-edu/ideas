module OpenMath.StrategyToWiki where

import Common.Assignment
import Common.Strategy (reportLocations)
import Domain.LinearAlgebra (reduceMatrixAssignment)
import OpenMath.StrategyTable
import Data.Char
import Data.List
import System.Environment

main :: IO ()
main = do
   args <- getArgs
   case args of 
      target:xs | not (null xs) -> mapM_ (make target) xs
      _ -> putStrLn "Usage: StrategyToWiki targetDir [list of strategy numbers]"
   
make :: String -> String -> IO ()
make target s = 
   case [ triple | triple@(t, _, _) <- strategyTable, s==t ] of
      [(nr, a, xs)] -> makeWikiFile target nr a xs
      _ -> putStrLn $ "Invalid strategy: " ++ s

makeWikiFile :: String -> String -> Assignment a -> [String] -> IO ()
makeWikiFile target nr a xs = do
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
makeLocationTable a = table ["Location", "Label or rule"] (reportLocations $ strategy a)

strategyFileName = "src/Domain/LinearAlgebra/Strategies.hs"

insertCode :: Assignment a -> [String] -> IO String
insertCode a xs = do
   program <- readFile strategyFileName
   let functions = map (!!1) (reportLocations (strategy a)) ++ xs
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