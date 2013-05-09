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
module Ideas.Documentation.OverviewPages
   ( makeOverviewExercises, makeOverviewServices
   ) where

import Ideas.Common.Exercise
import Ideas.Common.Id
import Ideas.Common.Utils (Some(..))
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Ideas.Documentation.DefaultPage
import Ideas.Service.DomainReasoner
import Ideas.Service.Types
import Ideas.Text.HTML

makeOverviewExercises :: DomainReasoner -> String -> IO ()
makeOverviewExercises dr dir = do
   generatePage dr dir exerciseOverviewPageFile $
      exerciseOverviewPage False $ exercises dr
   generatePage dr dir exerciseOverviewAllPageFile $
      exerciseOverviewPage True $ exercises dr

makeOverviewServices :: DomainReasoner -> String -> IO ()
makeOverviewServices dr dir =
   generatePage dr dir serviceOverviewPageFile $ 
      serviceOverviewPage $ services dr

exerciseOverviewPage :: Bool -> [Some Exercise] -> HTMLBuilder
exerciseOverviewPage showAll list = do
   h1 title

   forM_ (zip [1::Int ..] (grouping list)) $ \(i, (dom, xs)) -> do
      h2 (show i ++ ". " ++ dom)
      table False (map makeRow xs)

   unless showAll $ para $ do
      text "Show"
      space
      link exerciseOverviewAllPageFile $
         text "all exercises"
      text ", including the ones under development"
 where
   title | showAll   = "All exercises"
         | otherwise = "Exercises"

   makeRow (Some ex) =
      [ link (exercisePageFile code) $ ttText (show code)
      , do spaces 10
           f (status ex)
           spaces 10
      , text $ description ex
      ]
    where
      code = getId ex
      f st = italic $ text ("(" ++ map toLower (show st) ++ ")")

   grouping = map g . groupBy eq . sortBy cmp . filter p
    where
      cmp (Some a) (Some b) = compareId (exerciseId a) (exerciseId b)
      eq a b      = f a == f b
      f (Some ex) = listToMaybe (qualifiers (exerciseId ex))
      g xs        = (fromMaybe "" (f (head xs)), xs)
      p (Some ex) = showAll || isPublic ex

serviceOverviewPage :: [Service] -> HTMLBuilder
serviceOverviewPage list = do
   h1 "Services"
   let (xs, ys) = partition serviceDeprecated (sortBy compareId list)
       make s   = link (servicePageFile s) (ttText (showId s))
   ul $ map make ys
   unless (null xs) $ do
      h2 "Deprecated"
      ul $ map make xs