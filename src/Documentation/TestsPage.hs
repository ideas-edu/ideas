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

makeTestsPage :: String -> String -> DomainReasoner ()
makeTestsPage docDir testDir = do
   suite  <- selfCheck testDir
   result <- liftIO (runTestSuiteResult suite)
   generatePage docDir testsPageFile (testsPage result)

testsPage :: TestSuiteResult -> HTMLBuilder
testsPage result = do 
   h1 "Summary"
   preText (makeSummary result)
   h1 "Tests"
   preText (makeTestLog result)