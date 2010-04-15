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

import Common.Utils (Some(..))
import Service.ExerciseList
import Service.ServiceList
import Documentation.SelfCheck
import Documentation.LatexRules
import Documentation.ExercisePage
import Documentation.ServicePage
import Documentation.OverviewPages 

data Documentation = Pages String | LatexRules String | SelfCheck String
   deriving Eq

makeDocumentation :: [Documentation] -> IO ()
makeDocumentation = mapM_ $ \doc -> 
   case doc of
      Pages      dir -> makePages dir
      LatexRules dir -> makeLatexRules dir
      SelfCheck  dir -> performSelfCheck dir

makePages :: String -> IO ()
makePages dir = do 
   makeExerciseOverviewPages dir
   makeServiceOverviewPage dir
   mapM_ (\(Some pkg) -> makeExercisePage dir pkg) packages
   mapM_ (makeServicePage dir) serviceList