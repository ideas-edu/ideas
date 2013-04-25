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
module Ideas.Documentation.Make (DocItem(..), makeDocumentation) where

import Ideas.Common.Utils (Some(..))
import Ideas.Common.Utils.TestSuite
import Control.Monad
import Data.Maybe
import Ideas.Documentation.ExercisePage
import Ideas.Documentation.OverviewPages
import Ideas.Documentation.RulePage
import Ideas.Documentation.SelfCheck
import Ideas.Documentation.ServicePage
import Ideas.Documentation.TestsPage
import Ideas.Documentation.ViewPage
import Ideas.Service.DomainReasoner

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
         exs <- getExercises
         forM_ exs $ \(Some ex) ->
            makeExercisePage docDir ex
         report "Generating view pages"
         makeViewPages docDir
         report "Generating rule pages"
         makeRulePages docDir
         report "Generating service pages"
         getServices >>= mapM_ (makeServicePage docDir)
         report "Running tests"
         makeTestsPage docDir testDir
         {- report "Status hashtable"
         let file = docDir ++ "/hashtable.out"
         liftIO $ do
            putStrLn $ "Generating " ++ show file
            tableStatus >>= writeFile file -}
      SelfCheck -> do
         checks <- selfCheck testDir
         result <- liftIO (runTestSuiteResult checks)
         liftIO (printSummary result)
      BlackBox mdir -> do
         run    <- runWithCurrent
         checks <- liftIO $ blackBoxTests run (fromMaybe testDir mdir)
         result <- liftIO $ runTestSuiteResult checks
         liftIO (printSummary result)

report :: String -> DomainReasoner ()
report s = liftIO $ do
   let line = replicate 75 '-'
   putStrLn line
   putStrLn ("--- " ++ s)
   putStrLn line