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
-----------------------------------------------------------------------------
module Ideas.Documentation.ViewPage (makeViewPages) where

import Ideas.Common.Id
import Ideas.Common.View
import Control.Monad
import Data.List
import Data.Maybe
import Ideas.Documentation.DefaultPage
import Ideas.Documentation.ExampleFile
import Ideas.Documentation.ExercisePage
import Prelude hiding ((^))
import Ideas.Service.DomainReasoner
import Ideas.Text.HTML

makeViewPages :: String -> DomainReasoner ()
makeViewPages dir = do
   views <- liftM (sortBy compareId) getViews
   generatePage dir viewsOverviewPageFile (makeOverviewPage views)
   forM_ views $ \v -> do
      let exFile = dir ++ "/" ++ diagnosisExampleFile (getId v)
      xs <- liftIO $ liftM items (readExampleFile exFile)
               `catch` \_ -> return []
      generatePageAt 1 dir (viewPageFile v) (viewPage xs v)

makeOverviewPage :: HasId a => [a] -> HTMLBuilder
makeOverviewPage xs = do
   h1 "Views"
   table True (top : map make xs)
 where
   top = map text ["id", "description"]
   make x = [link (viewPageFile x) (text (showId x)), text (description x)]

viewPage :: [Item] -> ViewPackage -> HTMLBuilder
viewPage list (ViewPackage f v) = do
   idboxHTML "view" (getId v)
   unless (null list) $ do
      h2 "Examples"
      table True (top : content)
 where
   top = map text
      ["term", "representation", "canonical", "description"]

   content = map present . reorder . concatMap make $ list

   make (Ready t _ descr) =
      case f t of
         Just a ->
            [(True, t, match v a, canonical v a, descr)]
         Nothing ->
            [(False, t, Nothing, Nothing, descr)]
   make _ = []

   reorder [] = []
   reorder (x:xs) = x : ys ++ reorder zs
    where
      (ys, zs) | isJust (g x) = partition p xs
               | otherwise    = ([], xs)
      p a = g a == g x
      g (_, _, _, c, _) = c

   present (ok, t, b, c, descr) =
      let mark = if ok then id else spanClass "error"
      in map (mark . text) [t, maybe "-" show b, maybe "-" show c, descr]