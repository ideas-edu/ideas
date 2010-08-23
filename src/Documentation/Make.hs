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
module Documentation.Make (DocItem(..), makeDocumentation) where

import Common.TestSuite
import Common.Utils (Some(..))
import Data.Maybe
import Service.DomainReasoner
import Documentation.SelfCheck
import Documentation.ExercisePage
import Documentation.TestsPage
import Documentation.ServicePage
import Documentation.OverviewPages

data DocItem = Pages | SelfCheck | BlackBox (Maybe String)
   deriving Eq

makeDocumentation :: String -> String -> DocItem -> DomainReasoner ()
makeDocumentation docDir testDir item =
   case item of
      Pages -> do 
         report "Generating overview pages"
         makeOverviewExercises docDir
         makeOverviewServices  docDir
         report "Generating exercise pages"
         getPackages >>= mapM_ (\(Some pkg) -> makeExercisePage docDir pkg)
         report "Generating service pages"
         getServices >>= mapM_ (\s          -> makeServicePage docDir s)
         report "Running tests"
         makeTestsPage docDir testDir
      SelfCheck -> do
         suite <- selfCheck testDir
         result <- liftIO (runTestSuiteResult suite)
         liftIO (printSummary result)
      BlackBox mdir -> do
         suite  <- blackBoxTests (fromMaybe testDir mdir)
         result <- liftIO (runTestSuiteResult suite)
         liftIO (printSummary result)
         
report :: String -> DomainReasoner ()
report s = liftIO $ do
   let line = replicate 75 '-'
   putStrLn line
   putStrLn ("--- " ++ s)
   putStrLn line