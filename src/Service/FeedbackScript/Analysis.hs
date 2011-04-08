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
-- Analysis of a feedbackscript
--
-----------------------------------------------------------------------------
module Service.FeedbackScript.Analysis (withScripts) where

import Common.Exercise
import Common.Transformation
import Common.Utils (Some(..))
import Data.List
import Service.DomainReasoner
import Service.ExercisePackage
import Service.FeedbackScript.Syntax
import Service.FeedbackScript.Run 

withScripts :: [String] -> DomainReasoner ()
withScripts = mapM_ $ \s -> do
   Some pkg <- findPackage (newId s)
   liftIO $ print (generateScript (exercise pkg))

generateScript :: Exercise a -> Script
generateScript ex = makeScript $
   Supports [getId ex] :
   [ feedbackDecl s mempty | s <- feedbackIds ] ++
   [ textForIdDecl r (makeText (description r)) | r <- nrs ] ++
   [ textForIdDecl r (makeText (description r)) | r <- brs ]
 where
   (brs, nrs) = partition isBuggyRule (ruleset ex)