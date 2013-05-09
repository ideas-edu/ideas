-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Ideas.Documentation.TestsPage (makeTestsPage) where

import Ideas.Common.Utils.TestSuite
import Control.Monad
import Ideas.Documentation.DefaultPage
import Ideas.Documentation.SelfCheck
import Ideas.Service.DomainReasoner
import Ideas.Text.HTML

makeTestsPage :: DomainReasoner -> String -> String -> IO ()
makeTestsPage dr docDir testDir = do
   result <- runTestSuiteResult (selfCheck dr testDir)
   generatePage dr docDir testsPageFile (testsPage result)

testsPage :: TestSuiteResult -> HTMLBuilder
testsPage result = do
   h1 "Summary"
   preText (makeSummary result)
   h1 "Tests"
   formatResult [] result

formatResult :: [Int] -> TestSuiteResult -> HTMLBuilder
formatResult loc result = do
   ttText (show result)
   br
   forM_ (topMessages result) $ \m ->
      spanClass (if isError m then "error" else "warning")
      (ttText (show m) >> br)
   let subs = zip [1::Int ..] (subResults result)
   forM_ subs $ \(i, (s, a)) -> do
      let newloc = loc ++ [i]
      showHeader newloc s
      formatResult newloc a

showHeader :: [Int] -> String -> HTMLBuilder
showHeader [a]   s = h2 (show a ++ ". " ++ s)
showHeader [a,b] s = h3 (show a ++ "." ++ show b ++ ". " ++ s)
showHeader _     s = para (bold (text s))