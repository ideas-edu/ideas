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
module Documentation.Make (DocItem(..), makeDocumentation) where

import Control.Monad
import Common.Utils (Some(..))
import Service.DomainReasoner
import Documentation.SelfCheck
import Documentation.LatexRules
import Documentation.ExercisePage
import Documentation.ServicePage
import Documentation.OverviewPages

data DocItem = Pages String | LatexRules String | SelfCheck String
   deriving Eq

makeDocumentation :: DocItem -> DomainReasoner ()
makeDocumentation doc =
   case doc of
      Pages dir -> do 
         makeOverviewExercises dir
         makeOverviewServices  dir
         getPackages >>= mapM_ (\(Some pkg) -> makeExercisePage dir pkg)
         getServices >>= mapM_ (\(Some s)   -> makeServicePage dir s)
      SelfCheck dir -> 
         performSelfCheck dir
      LatexRules dir ->
         let f (Some ex) = makeLatexRules dir ex
         in getExercises >>= lift . mapM_ f