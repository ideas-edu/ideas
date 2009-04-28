module Documentation.Make (main) where

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
   mapM_ (\(Some ex) -> makeExercisePage ex) exerciseList
   mapM_ makeServicePage serviceList 