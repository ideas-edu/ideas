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
module Main (main) where

import Common.Utils (Some(..))
import Service.ExerciseList
import Service.ServiceList
import Documentation.ExercisePage
import Documentation.ServicePage
import Documentation.OverviewPages 

main :: IO ()
main = do 
   makeExerciseOverviewPage
   makeServiceOverviewPage
   mapM_ (\(Some pkg) -> makeExercisePage pkg) packages
   mapM_ makeServicePage serviceList 