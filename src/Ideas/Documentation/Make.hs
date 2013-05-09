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

makeDocumentation :: DomainReasoner -> String -> String -> DocItem -> IO ()
makeDocumentation dr docDir testDir item =
   case item of
      Pages -> do
         report "Generating overview pages"
         makeOverviewExercises dr docDir
         makeOverviewServices  dr docDir
         report "Generating exercise pages"
         forM_ (exercises dr) $ \(Some ex) ->
            makeExercisePage dr docDir ex
         report "Generating view pages"
         makeViewPages dr docDir
         report "Generating rule pages"
         makeRulePages dr docDir
         report "Generating service pages"
         mapM_ (makeServicePage dr docDir) (services dr)
         report "Running tests"
         makeTestsPage dr docDir testDir
         {- report "Status hashtable"
         let file = docDir ++ "/hashtable.out"
         liftIO $ do
            putStrLn $ "Generating " ++ show file
            tableStatus >>= writeFile file -}
      SelfCheck -> do
         result <- runTestSuiteResult (selfCheck dr testDir)
         liftIO (printSummary result)
      BlackBox mdir -> do
         checks <- blackBoxTests dr (fromMaybe testDir mdir)
         result <- runTestSuiteResult checks
         liftIO (printSummary result)

report :: String -> IO ()
report s = do
   let line = replicate 75 '-'
   putStrLn line
   putStrLn ("--- " ++ s)
   putStrLn line