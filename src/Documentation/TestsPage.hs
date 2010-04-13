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
module Documentation.TestsPage (main) where

import Control.Monad
import Data.Char
import Data.List
import Documentation.DefaultPage
import System.Environment
import Text.HTML
import qualified Text.XML as XML

main :: IO ()
main = do
   args <- getArgs
   case args of
      [fileIn, fileOut] -> do
         input <- readFile fileIn
         generatePage "docs" (up 1 ++ fileOut) (testsPage input)
      _ -> fail "Invalid invocation"
   
testsPage :: String -> HTML
testsPage input = defaultPage "Tests" 0 $ do 
   h1 "Tests"
   let (hs, bs) = unzip (map format (lines input))
   bold (text "Failures: ") 
   text $ show $ length $ filter not bs
   brs hs
 where
   format :: String -> (HTMLBuilder, Bool)
   format s
      | any (`elem` ws) ["failed", "error", "error:", "falsifiable"] =
           (errorLine (ttText s), False)
      | "* " `isPrefixOf` s =
           (h2 (drop 2 s), True)
      | "** " `isPrefixOf` s =
           (br >> bold (text (drop 3 s)), True)
      | "*** " `isPrefixOf` s =
           (br >> bold (text (drop 4 s)), True)
      | otherwise = 
           (fromString s, True)
    where
      ws = map (map toLower . filter isAlpha) (words s)
      
      
brs :: [HTMLBuilder] -> HTMLBuilder
brs = mapM_ (>> br)

fromString :: String -> HTMLBuilder
fromString = f []
 where
   f acc []     = ttText (reverse acc)
   f acc list@(x:xs) 
      | "+++" `isPrefixOf` list = do
           f acc [] 
           unless (null acc) (spaces 3)
           okLine (ttText (drop 3 list))
      | "*** Gave up!" `isPrefixOf` list = do
           f acc []
           unless (null acc) (spaces 3)
           ttText (drop 3 list)
      | otherwise = f (x:acc) xs

errorLine :: HTMLBuilder -> HTMLBuilder
errorLine b = XML.element "font" $ do
   "color" XML..=. "red"
   bold b
   
okLine :: HTMLBuilder -> HTMLBuilder
okLine b = XML.element "font" $ do
   "color" XML..=. "gray"
   b