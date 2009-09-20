module Documentation.OverviewPages 
   ( makeExerciseOverviewPage, makeServiceOverviewPage ) where

import Documentation.DefaultPage
import Data.List
import Control.Monad
import Common.Utils (Some(..))
import Common.Exercise
import Service.ExerciseList
import Service.ServiceList
import Text.HTML

makeExerciseOverviewPage :: IO ()
makeExerciseOverviewPage = generatePage exerciseOverviewPageFile exerciseOverviewPage

makeServiceOverviewPage :: IO ()
makeServiceOverviewPage = generatePage serviceOverviewPageFile serviceOverviewPage

exerciseOverviewPage :: HTML
exerciseOverviewPage = defaultPage "Exercises" 0 $ do
   let groups = groupBy (\x y -> f x == f y) 
              $ sortBy (\x y -> g x `compare` g y) exerciseList
       f (Some ex) = domain ex
       g (Some ex) = show (exerciseCode ex)
   h1 "Exercises"
   forM_ (zip [1..] groups) $ \(i, xs@(hd:_)) -> do
      h2 (show i ++ ". " ++ f hd)
      ul $ flip map xs $ \(Some ex) -> do
         link (exercisePageFile ex) $ ttText (show (exerciseCode ex))
         space
         text $ "(" ++ description ex ++ ")"

serviceOverviewPage :: HTML
serviceOverviewPage = defaultPage "Services" 0 $ do
   h1 "Services"
   let list = sortBy (\x y -> serviceName x `compare` serviceName y) serviceList
   ul $ flip map list $ \s -> 
      link (servicePageFile s) (ttText (serviceName s))