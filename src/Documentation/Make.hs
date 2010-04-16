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
module Documentation.Make 
   ( Documentation(..), makeDocumentation
   ) where

import Control.Monad
import Common.Utils (Some(..))
import Service.ServiceList
import Service.ExercisePackage
import Documentation.SelfCheck
import Documentation.LatexRules
import Documentation.ExercisePage
import Documentation.ServicePage
import Documentation.OverviewPages 

data Documentation = Pages String | LatexRules String | SelfCheck String
   deriving Eq

makeDocumentation :: String -> [Some ExercisePackage] -> Documentation -> IO ()
makeDocumentation version list doc =
   case doc of
      Pages dir -> do 
         makeOverviewExercises version dir exList
         makeOverviewServices version dir srvList
         forM_ list $ \(Some pkg) -> makeExercisePage version dir pkg
         mapM_ (makeServicePage version dir) srvList
      SelfCheck dir -> 
         performSelfCheck dir list
      LatexRules dir -> 
         forM_ exList $ \(Some ex) -> makeLatexRules dir ex
 where
   exList  = map (\(Some pkg) -> Some (exercise pkg)) list
   srvList = exerciselistS list : serviceList