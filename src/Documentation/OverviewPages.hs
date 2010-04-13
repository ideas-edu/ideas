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
module Documentation.OverviewPages 
   ( makeExerciseOverviewPages
   , makeServiceOverviewPage 
   ) where

import Documentation.DefaultPage
import Data.Char
import Data.List
import Control.Monad
import Common.Utils (Some(..))
import Common.Exercise
import Service.ExerciseList
import Service.ServiceList
import Text.HTML

makeExerciseOverviewPages :: String -> IO ()
makeExerciseOverviewPages dir = do
   generatePage dir exerciseOverviewPageFile    (exerciseOverviewPage False)
   generatePage dir exerciseOverviewAllPageFile (exerciseOverviewPage True)

makeServiceOverviewPage :: String -> IO ()
makeServiceOverviewPage dir = 
   generatePage dir serviceOverviewPageFile serviceOverviewPage

exerciseOverviewPage :: Bool -> HTML
exerciseOverviewPage showAll = defaultPage title 0 $ do
   h1 title
   
   unless showAll $ para $ do
      text "Show"
      space
      link exerciseOverviewAllPageFile $ 
         text "all exercises"
      text ", including the ones under development"
      
   forM_ (zip [1..] (groupedList showAll)) $ \(i, (dom, xs)) -> do
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
      code = exerciseCode ex
      f st = italic $ text ("(" ++ map toLower (show st) ++ ")")

groupedList :: Bool -> [(String, [Some Exercise])]
groupedList showAll = process exercises
 where
   process = map g . groupBy eq . sortBy cmp . filter p
 
   cmp (Some a) (Some b) = exerciseCode a `compare` exerciseCode b
   eq a b      = f a == f b
   f (Some ex) = domain (exerciseCode ex)
   g xs = (f (head xs), xs)
   p (Some ex) = showAll || isPublic ex

serviceOverviewPage :: HTML
serviceOverviewPage = defaultPage "Services" 0 $ do
   h1 "Services"
   let list = sortBy (\x y -> serviceName x `compare` serviceName y) serviceList
   ul $ flip map list $ \s -> do
      link (servicePageFile s) (ttText (serviceName s))
      when (serviceDeprecated s) $
         space >> text "(deprecated)"