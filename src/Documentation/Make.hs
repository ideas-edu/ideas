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
module Documentation.Make (makeDocumentation) where

import Common.Utils (Some(..))
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
   case [ s | MakePages s <- flags ] of
      [dir] -> makePages dir
      _     -> return ()
   -- make rules
   case [ s | MakeRules s <- flags ] of
      [dir] -> makeLatexRules dir
      _     -> return ()
   -- perform a self-check
   case [ () | SelfCheck _ <- flags ] of
      [] -> return ()
      _  -> execute

makePages :: String -> IO ()
makePages dir = do 
   makeExerciseOverviewPages dir
   makeServiceOverviewPage dir
   mapM_ (\(Some pkg) -> makeExercisePage dir pkg) packages
   mapM_ (makeServicePage dir) serviceList