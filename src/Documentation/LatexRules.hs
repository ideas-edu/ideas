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
module Documentation.LatexRules (makeLatexRules) where

import Common.Exercise
import Common.Rewriting
import Common.Transformation
import Common.Utils
import Control.Monad
import Service.ExerciseList
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.Time

makeLatexRules :: String -> IO ()
makeLatexRules dir =
   forM_ exercises $ \(Some ex) -> do
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
               let filename = path ++ "/rule" ++ filter isAlphaNum (name r) ++ ".lhs"
               putStrLn $ "Creating " ++ filename
               writeFile filename txt
{- 
exerciseRulesToTeX :: Exercise a -> String
exerciseRulesToTeX ex = unlines . map ruleToTeX . concatMap getRewriteRules . ruleset $ ex
-}

ruleToTeX :: (Some RewriteRule, Bool) -> Maybe String
ruleToTeX (Some r, sound) = do
   txt <- showRewriteRule sound r
   return $ "RewriteRule " ++ withoutDigits (ruleName r) 
                           ++ " (" ++ txt ++ ")"

   
------------------------------------------------------

makeSingleRule :: String -> Rule a -> Maybe String
makeSingleRule dom r 
   | null (getRewriteRules r) = Nothing
   | otherwise = Just $ texHeader (Just dom) ++ texBody Nothing content
 where
   content = unlines $
      [ "\\pagestyle{empty}"
      , formatRuleName (name r)
      , "\\begin{code}"
      ] ++
      map (filter (/= '"') . fromMaybe "" . ruleToTeX) (getRewriteRules r) ++
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
   , "%format RewriteRule (a) (b) = \"\\rewriterule{\"a\"}{\"b\"}\""
   , "%format ~> = \"\\:\\leadsto\\:\""
   , "%format /~> = \"\\:\\not\\leadsto\\:\""
   , maybe "" ("%include "++) fmt
   , "" 
   , "\\newcommand{\\rewriterule}[2]{#1:\\quad #2}"
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
   formats = unlines (map formatRuleName names)
   
   makeGroup :: Maybe String -> String
   makeGroup mgroup = unlines 
      [ maybe "" (\s -> "\\subsection{" ++ s ++ "}") mgroup
      , "\\begin{code}"
      , unlines $ map (filter (/= '"')) xs
      , "\\end{code}"
      ]
    where
      p x = maybe (null $ ruleGroups x) (`elem` ruleGroups x) mgroup
      xs  = mapMaybe ruleToTeX $ concatMap getRewriteRules $ filter p $ ruleset ex
      
formatRuleName :: String -> String
formatRuleName s = "%format " ++ withoutDigits s ++ " = \"\\rulename{" ++ s ++ "}\""

withoutDigits :: String -> String
withoutDigits = concatMap f 
 where
   f c | isAlpha c = [c]
       | isDigit c = "QX" ++ [chr (ord c + 49)]
       | otherwise = []