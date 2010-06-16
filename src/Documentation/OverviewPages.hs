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
module Documentation.OverviewPages 
   ( makeOverviewExercises, makeOverviewServices
   ) where

import Documentation.DefaultPage
import Data.Char
import Data.List
import Control.Monad
import Common.Utils (Some(..))
import Common.Exercise
import Service.DomainReasoner
import Service.Types
import Text.HTML

makeOverviewExercises :: String -> DomainReasoner ()
makeOverviewExercises dir = do
   list <- getExercises
   generatePage dir exerciseOverviewPageFile $ 
      exerciseOverviewPage False list
   generatePage dir exerciseOverviewAllPageFile $ 
      exerciseOverviewPage True list

makeOverviewServices :: String -> DomainReasoner ()
makeOverviewServices dir = do
   list <- getServices
   generatePage dir serviceOverviewPageFile (serviceOverviewPage list)

exerciseOverviewPage :: Bool -> [Some Exercise] -> HTMLBuilder
exerciseOverviewPage showAll list = do
   h1 title
   
   unless showAll $ para $ do
      text "Show"
      space
      link exerciseOverviewAllPageFile $ 
         text "all exercises"
      text ", including the ones under development"
      
   forM_ (zip [1..] (grouping list)) $ \(i, (dom, xs)) -> do
      h2 (show i ++ ". " ++ dom)
      noBorderTable (map makeRow xs) 
 where
   title | showAll   = "All exercises"
         | otherwise = "Exercises"
 
   makeRow (Some ex) = 
      [ do tt bullet >> space
           link (exercisePageFile code) $ ttText (show code)
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
      cmp (Some a) (Some b) = exerciseId a `compare` exerciseId b
      eq a b      = f a == f b
      f (Some ex) = qualification (exerciseId ex)
      g xs = (f (head xs), xs)
      p (Some ex) = showAll || isPublic ex

serviceOverviewPage :: [Service] -> HTMLBuilder
serviceOverviewPage list = do
   h1 "Services"
   let sorted = sortBy (\x y -> serviceName x `compare` serviceName y) list
   ul $ flip map sorted $ \s -> do
      link (servicePageFile s) (ttText (serviceName s))
      when (serviceDeprecated s) $
         space >> text "(deprecated)"