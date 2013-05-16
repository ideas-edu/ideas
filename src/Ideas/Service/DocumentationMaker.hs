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
-- Manages links to information
--
-----------------------------------------------------------------------------
module Ideas.Service.DocumentationMaker (makeDocumentation) where

import Control.Monad
import Ideas.Common.Library
import System.Directory
import System.FilePath
import Ideas.Common.Utils
import Ideas.Service.EncoderHTML
import Ideas.Service.DomainReasoner
import Ideas.Service.LinkManager
import Ideas.Service.ServiceList (RuleShortInfo(..), exampleDeriv)
import Ideas.Service.Types
import Main

go = makeDocumentation ideasMath

makeDocumentation :: DomainReasoner -> IO ()
makeDocumentation dr = do
   putStrLn "Generating index pages"
   make urlForIndex emptyExercise (dr ::: typed)
   make urlForExercises emptyExercise (exercises dr ::: typed)
   make urlForServices emptyExercise (services dr ::: typed)
   putStrLn "Generating service pages"
   forM_ (services dr) $ \srv -> 
      make (\lm -> urlForService lm srv) emptyExercise (srv ::: typed)
   putStrLn "Generating exercise pages"
   forM_ (exercises dr) $ \(Some ex) -> do
      make (\lm -> urlForExercise lm ex) ex (ex ::: typed)
      make (\lm -> urlForStrategy lm ex) ex (toStrategy (strategy ex) ::: typed)
      make (\lm -> urlForRules lm ex) ex (map RuleShortInfo (ruleset ex) ::: typed)
      make (\lm -> urlForExamples lm ex) ex (map (second (inContext ex)) (examples ex) ::: typed)
      make (\lm -> urlForDerivations lm ex) ex (exampleDeriv ex ::: typed)
      forM_ (ruleset ex) $ \r -> do
          make (\lm -> urlForRule lm ex r) ex (r ::: typed)
 where
   lm = staticLinks
   make url ex tv = safeWrite ("newdoc/" ++ url lm) $ 
      show $ htmlEncoder (linksUp (pathLevel $ url lm) lm) dr ex tv 
  
safeWrite :: FilePath -> String -> IO ()
safeWrite filename txt = do
   let dirpart = takeDirectory filename
   unless (null dirpart) (createDirectoryIfMissing True dirpart)
   putStrLn $ "- " ++ filename
   writeFile filename txt