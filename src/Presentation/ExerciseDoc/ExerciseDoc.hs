module Main (main) where

import Common.Exercise
import Common.Rewriting
import Common.Transformation
import Common.Utils
import Control.Monad
import Service.ExerciseList
import Data.Char
import Data.List
import System.Environment
import System.Time

main :: IO ()
main = do
   dir <- targetDirectory
   flip mapM_ exerciseList $ \(Some ex) -> do
      let rules = concatMap getRewriteRules (ruleset ex)
      unless (null rules) $ do
         doc <- makeDocument ex
         let filename = dir ++ "/" ++ show (exerciseCode ex) ++ ".lhs"
         putStrLn $ "Creating " ++ filename
         writeFile filename doc

targetDirectory :: IO String
targetDirectory = do
   args <- getArgs
   case args of
      []    -> return "."
      dir:_ -> return dir

rulesToTeX :: Exercise a -> String
rulesToTeX ex = unlines . map f . concatMap getRewriteRules . ruleset $ ex
 where
   f (Some r, sound) = 
      "RewriteRule " ++ filter isAlpha (ruleName r) ++ " (" ++ showRuleSpec sound r ++ ")"
   
------------------------------------------------------

makeDocument :: Exercise a -> IO String
makeDocument ex = do
   time <- getClockTime
   return $ 
      texHeader (Just $ domain ex ++ ".fmt") ++ 
      texBody (Just $ show time) (texSectionRules ex)

------------------------------------------------------

texHeader :: Maybe String -> String
texHeader fmt = unlines
   [ "\\documentclass{article}"
   , ""
   , "%include lhs2TeX.fmt"
   , "%format RewriteRule (a) (b) = a\": \"b"
   , "%format :~> = \"\\:\\leadsto\\:\""
   , "%format :/~> = \"\\:\\not\\leadsto\\:\""
   , maybe "" ("%include "++) fmt
   , "" 
   , "\\newcommand{\\rewriterule}[2]{#1: #2}"
   , "\\newcommand{\\rulename}[1]{\\mbox{\\sc #1}}"
   ]
   
texBody :: Maybe String -> String -> String
texBody date content = unlines
   [ "\\begin{document}"
   , maybe "" (\s -> "\\hfill@(generated on " ++ s ++ ")@") date
   , content
   , "\\end{document}"
   ]
   
texSectionRules :: Exercise a -> String
texSectionRules ex = unlines 
   [ "\\section*{Rewrite Rules}"
   , formats
   , "\\begin{code}"
   , filter (/= '"') $ rulesToTeX ex
   , "\\end{code}"
   ]
 where
   rules   = concatMap getRewriteRules (ruleset ex)
   names   = let f (Some r, _) = ruleName r 
             in nub (map f rules)
   formats = unlines (map make names)
   make  s = "%format " ++ s ++ " = \"\\rulename{" ++ s ++ "}\"" 