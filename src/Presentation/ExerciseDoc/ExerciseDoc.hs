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
      -- Exercise document
      let rules = concatMap getRewriteRules (ruleset ex)
      unless (null rules) $ do
         doc <- makeDocument ex
         let filename = dir ++ "/" ++ show (exerciseCode ex) ++ ".lhs"
         putStrLn $ "Creating " ++ filename
         writeFile filename doc
      -- individual rules
      flip mapM_ (ruleset ex) $ \r ->
         case makeSingleRule (domain ex ++ ".fmt") r of
            Nothing  -> return ()
            Just txt -> do
               let filename = dir ++ "/" ++ show (exerciseCode ex) ++ "." ++ name r ++ ".lhs"
               putStrLn $ "Creating " ++ filename
               writeFile filename txt

targetDirectory :: IO String
targetDirectory = do
   args <- getArgs
   case args of
      []    -> return "."
      dir:_ -> return dir

exerciseRulesToTeX :: Exercise a -> String
exerciseRulesToTeX ex = unlines . map ruleToTeX . concatMap getRewriteRules . ruleset $ ex

ruleToTeX :: (Some RewriteRule, Bool) -> String
ruleToTeX (Some r, sound) = 
   "RewriteRule " ++ filter isAlpha (ruleName r) ++ " (" ++ showRuleSpec sound r ++ ")"

   
------------------------------------------------------

makeSingleRule :: String -> Rule a -> Maybe String
makeSingleRule dom r 
   | null (getRewriteRules r) = Nothing
   | otherwise = Just $ texHeader (Just dom) ++ texBody Nothing content
 where
   content = unlines $
      [ "\\pagestyle{empty}"
      , "\\begin{code}"
      ] ++
      map (filter (/= '"') . ruleToTeX) (getRewriteRules r) ++
      [ "\\end{code}"
      ]


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
   , filter (/= '"') $ exerciseRulesToTeX ex
   , "\\end{code}"
   ]
 where
   rules   = concatMap getRewriteRules (ruleset ex)
   names   = let f (Some r, _) = ruleName r 
             in nub (map f rules)
   formats = unlines (map make names)
   make  s = "%format " ++ s ++ " = \"\\rulename{" ++ s ++ "}\"" 