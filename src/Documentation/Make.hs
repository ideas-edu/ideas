-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Documentation.Make (makeDocumentation) where

import Common.Utils (Some(..))
import Control.Monad
import Service.ExerciseList
import Service.Options
import Service.ServiceList
import Documentation.Checks
import Documentation.LatexRules
import Documentation.ExercisePage
import Documentation.ServicePage
import Documentation.OverviewPages 

makeDocumentation :: [Flag] -> IO ()
makeDocumentation flags = do
   -- make pages
   unless (null [ () | MakePages _ <- flags ]) makePages
   -- make rules
   unless (null [ () | MakeRules _ <- flags ]) makeLatexRules
   -- perform a self-check
   unless (null [ () | SelfCheck _ <- flags ]) execute

makePages :: IO ()
makePages = do 
   makeExerciseOverviewPages
   makeServiceOverviewPage
   mapM_ (\(Some pkg) -> makeExercisePage pkg) packages
   mapM_ makeServicePage serviceList 