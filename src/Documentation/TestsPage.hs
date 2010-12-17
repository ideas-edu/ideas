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
module Documentation.TestsPage (makeTestsPage) where

import Common.TestSuite
import Documentation.DefaultPage
import Documentation.SelfCheck
import Service.DomainReasoner
import Text.HTML
import qualified Text.XML as XML

makeTestsPage :: String -> String -> DomainReasoner ()
makeTestsPage docDir testDir = do
   checks <- selfCheck testDir
   result <- liftIO (runTestSuiteResult checks)
   generatePage docDir testsPageFile (testsPage result)

testsPage :: TestSuiteResult -> HTMLBuilder
testsPage result = do 
   h1 "Summary"
   preText (makeSummary result)
   h1 "Tests"
   makeTestLogWith formatHTML result
   
formatHTML :: FormatLog HTMLBuilder
formatHTML = FormatLog
   { formatRoot = \_ -> id
   , formatSuite = \loc s _ _ a -> 
        showHeader loc s >> a 
   , formatSuccesses = \xs -> 
        let f (_, n) = if n==1 then "." else "(passed " ++ show n ++ " tests)"
        in mapM_ (\s -> ttText s >> br) (breakLine (concatMap f xs))
   , formatFailure = \s msg -> colorRed $ do
        bold (ttText ("Error" ++ putLabel s))
        tt space
        ttText msg
        br
   , formatWarning = \s msg -> colorOrange $ do
        ttText ("Warning" ++ putLabel s)
        tt space
        ttText msg
        br
   }
 where 
   putLabel s = if null s then ":" else " (" ++ s ++ "):"

breakLine :: String -> [String]
breakLine xs
   | null xs   = []
   | otherwise = ys : breakLine zs
 where
   (ys, zs) = splitAt 80 xs

showHeader :: [Int] -> String -> HTMLBuilder
showHeader [a]   s = h2 (show a ++ ". " ++ s)
showHeader [a,b] s = h3 (show a ++ "." ++ show b ++ ". " ++ s)
showHeader _     s = para (bold (text s))

colorRed :: HTMLBuilder -> HTMLBuilder
colorRed body = XML.element "font" $ do
   "color" XML..=. "red"
   body
   
colorOrange :: HTMLBuilder -> HTMLBuilder
colorOrange body = XML.element "font" $ do
   "color" XML..=. "#FE9A2E"
   body