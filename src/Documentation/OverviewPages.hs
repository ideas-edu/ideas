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
   ( makeExerciseOverviewPage
   , makeServiceOverviewPage 
   ) where

import Documentation.DefaultPage
import Data.List
import Control.Monad
import Common.Utils (Some(..))
import Common.Exercise
import Service.ExerciseList
import Service.ServiceList
import Text.HTML

makeExerciseOverviewPage :: IO ()
makeExerciseOverviewPage = 
   generatePage exerciseOverviewPageFile exerciseOverviewPage

makeServiceOverviewPage :: IO ()
makeServiceOverviewPage = 
   generatePage serviceOverviewPageFile serviceOverviewPage

exerciseOverviewPage :: HTML
exerciseOverviewPage = defaultPage "Exercises" 0 $ do
   h1 "Exercises"
   forM_ (zip [1..] groupedList) $ \(i, (dom, xs)) -> do
      h2 (show i ++ ". " ++ dom)
      ul $ flip map xs $ \(Some ex) -> do
         let code = exerciseCode ex
         link (exercisePageFile code) $ ttText (show code ++ ":")
         space
         text $ description ex

groupedList :: [(String, [Some Exercise])]
groupedList = map g (groupBy eq (sortBy cmp exercises))
 where
   cmp (Some a) (Some b) = exerciseCode a `compare` exerciseCode b
   eq a b      = f a == f b
   f (Some ex) = domain (exerciseCode ex)
   g xs = (f (head xs), xs)

serviceOverviewPage :: HTML
serviceOverviewPage = defaultPage "Services" 0 $ do
   h1 "Services"
   let list = sortBy (\x y -> serviceName x `compare` serviceName y) serviceList
   ul $ flip map list $ \s -> do
      link (servicePageFile s) (ttText (serviceName s))
      when (serviceDeprecated s) $
         space >> text "(deprecated)"