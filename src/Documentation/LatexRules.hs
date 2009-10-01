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
module Documentation.LatexRules (main) where

import Common.Exercise
import Common.Rewriting
import Common.Transformation
import Common.Utils
import Control.Monad
import Service.ExerciseList
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.Time

main :: IO ()
main = do
   dir <- targetDirectory
   forM_ exerciseList $ \(Some ex) -> do
      let code = exerciseCode ex
          path = dir ++ "/" ++ domain code ++ "/" ++ filter (/= ' ') (identifier code)
      -- Exercise document
      let rules = concatMap getRewriteRules (ruleset ex)
      unless (null rules) $ do
         createDirectoryIfMissing True path
         doc <- makeDocument ex
         let filename = path ++ "/overview.lhs"
         putStrLn $ "Creating " ++ filename
         writeFile filename doc
      -- individual rules
      forM_ (ruleset ex) $ \r ->
         case makeSingleRule (domain code ++ "/" ++ domain code ++ ".fmt") r of
            Nothing  -> return ()
            Just txt -> do
               let filename = path ++ "/rule" ++ name r ++ ".lhs"
               putStrLn $ "Creating " ++ filename
               writeFile filename txt

targetDirectory :: IO String
targetDirectory = do
   args <- getArgs
   case args of
      []    -> return "."
      dir:_ -> return dir

{- 
exerciseRulesToTeX :: Exercise a -> String
exerciseRulesToTeX ex = unlines . map ruleToTeX . concatMap getRewriteRules . ruleset $ ex
-}

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
   let code = exerciseCode ex
   time <- getClockTime
   return $ 
      texHeader (Just $ domain code ++ "/" ++ domain code ++ ".fmt") ++ 
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
   , content
   , maybe "" (\s -> "\\par\\vspace*{5mm}\\noindent\\footnotesize{@(generated on " ++ s ++ ")@}") date
   , "\\end{document}"
   ]
   
texSectionRules :: Exercise a -> String
texSectionRules ex = unlines 
   [ "\\section{Rewrite rules}"
   , formats
   , makeGroup Nothing
   , unlines $ map (makeGroup . Just) groups
   ]
 where
   rules   = concatMap getRewriteRules (ruleset ex)
   groups  = nub (concatMap ruleGroups (ruleset ex))
   names   = let f (Some r, _) = ruleName r 
             in nub (map f rules)
   formats = unlines (map make names)
   make  s = "%format " ++ s ++ " = \"\\rulename{" ++ s ++ "}\"" 
   
   makeGroup :: Maybe String -> String
   makeGroup mgroup = unlines 
      [ maybe "" (\s -> "\\subsection{" ++ s ++ "}") mgroup
      , "\\begin{code}"
      , unlines $ map (filter (/= '"') . ruleToTeX) xs
      , "\\end{code}"
      ]
    where
      p x = maybe (null $ ruleGroups x) (`elem` ruleGroups x) mgroup
      xs  = concatMap getRewriteRules $ filter p $ ruleset ex