-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Manages links to information
--
-----------------------------------------------------------------------------
module Ideas.Main.Documentation (makeDocumentation) where

import Control.Monad
import Ideas.Common.Library
import Ideas.Common.Utils
import Ideas.Encoding.EncoderHTML
import Ideas.Encoding.LinkManager
import Ideas.Service.BasicServices
import Ideas.Service.DomainReasoner
import Ideas.Service.Types
import Ideas.Text.HTML
import System.Directory
import System.FilePath (takeDirectory)

makeDocumentation :: DomainReasoner -> String -> IO ()
makeDocumentation dr dir = do
   putStrLn "Generating index pages"
   makeIndex urlForIndex     (dr ::: typed)
   makeIndex urlForExercises (exercises dr ::: typed)
   makeIndex urlForServices  (services dr ::: typed)
   putStrLn "Generating service pages"
   forM_ (services dr) $ \srv ->
      makeIndex (`urlForService` srv) (srv ::: typed)
   putStrLn "Generating exercise pages"
   forM_ (exercises dr) $ \(Some ex) -> do
      makeEx ex urlForExercise    (ex ::: typed)
      makeEx ex urlForStrategy    (toStrategy (strategy ex) ::: typed)
      makeEx ex urlForRules       (ruleset ex ::: typed)
      makeEx ex urlForExamples    (map (second (inContext ex)) (examples ex) ::: typed)
      makeEx ex urlForDerivations (exampleDerivations ex ::: typed)
      forM_ (ruleset ex) $ \r ->
         make ex (urlForRule lm ex r) (r ::: typed)
 where
   lm = staticLinks
   makeIndex f = make emptyExercise (f lm)
   makeEx ex f = make ex (f lm ex)
   make ex url tv = safeWrite (dir </> url) $
      showHTML $ htmlEncoder (linksUp (pathLevel url) lm) dr ex tv

safeWrite :: FilePath -> String -> IO ()
safeWrite filename txt = do
   let dirpart = takeDirectory filename
   unless (null dirpart) (createDirectoryIfMissing True dirpart)
   putStrLn $ "- " ++ filename
   writeFile filename txt