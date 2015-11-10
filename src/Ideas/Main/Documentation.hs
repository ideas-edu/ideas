-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
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
import Ideas.Encoding.Encoder (run, simpleOptions)
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
   makeIndex urlForIndex     (dr ::: tDomainReasoner)
   makeIndex urlForExercises (exercises dr ::: tList tSomeExercise)
   makeIndex urlForServices  (services dr ::: tList tService)
   putStrLn "Generating service pages"
   forM_ (services dr) $ \srv ->
      makeIndex (`urlForService` srv) (srv ::: tService)
   putStrLn "Generating exercise pages"
   forM_ (exercises dr) $ \(Some ex) -> do
      makeEx ex urlForExercise    (ex ::: tExercise)
      makeEx ex urlForStrategy    (toStrategy (strategy ex) ::: tStrategy)
      makeEx ex urlForRules       (ruleset ex ::: tList tRule)
      makeEx ex urlForExamples    (map (second (inContext ex)) (examples ex) ::: tList (tPair tDifficulty tContext))
      makeEx ex urlForDerivations (exampleDerivations ex ::: tError (tList (tDerivation (tPair tRule tEnvironment) tContext)))
      forM_ (ruleset ex) $ \r ->
         make ex (urlForRule lm ex r) (r ::: tRule)
 where
   lm = staticLinks
   makeIndex f = make emptyExercise (f lm)
   makeEx ex f = make ex (f lm ex)
   make ex url tv = do
      let enc = htmlEncoderAt (pathLevel url) dr
      html <- run enc (simpleOptions ex) tv
      safeWrite (dir </> url) (showHTML html)

safeWrite :: FilePath -> String -> IO ()
safeWrite filename txt = do
   let dirpart = takeDirectory filename
   unless (null dirpart) (createDirectoryIfMissing True dirpart)
   putStrLn $ "- " ++ filename
   writeFile filename txt