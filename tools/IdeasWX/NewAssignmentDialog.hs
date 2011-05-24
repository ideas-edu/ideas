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
module NewAssignmentDialog (newAssignmentDialog, defaultAssignment) where

import Graphics.UI.WX
import Session
import Observable
import Control.Monad
import Data.List
import Data.Maybe
import ExerciseInfoPanel
import Common.Exercise
import Common.Id
import Main.IDEAS
import Service.DomainReasoner

defaultAssignment :: IO (Some Exercise)
defaultAssignment = useIDEAS (liftM head getExercises)

newAssignmentDialog :: Window a -> Session -> IO ()
newAssignmentDialog w session = do
   -- exercises and domains
   exs <- useIDEAS getExercises
   let domains = sort $ nub 
          [ qualification ex | Some ex <- exs ]
   -- create frame
   f <- dialog w [text := "New Assignment", bgcolor := white] 
   -- current exercise
   exid <- currentExerciseId session
   -- Left Panel
   leftPanel <- panel f []
   randomButton     <- button leftPanel [text := "Random"]
   cancelButton     <- button leftPanel [text := "Cancel"]
   goButton         <- button leftPanel [text := "Go"]
   difficultySlider <- hslider leftPanel False (fromEnum VeryEasy) (fromEnum VeryDifficult) 
      [selection := fromEnum Medium]
   ownTextView      <- textCtrl leftPanel [bgcolor := myGrey]
      
   -- Right Panel
   rightPanel <- panel f []
   domainBox  <- radioBox rightPanel Vertical domains []
   exerciseList <- singleListBox rightPanel []
   experimentalBox <- checkBox rightPanel [text := "Include private", checked := True]
   set rightPanel [layout := column 10 
      [ hstretch $ label "Domain selection", hfill $ widget domainBox
      , hstretch $ vspace 10
      , hstretch $ label "Exercise selection", fill $ widget exerciseList
      , hstretch $ widget experimentalBox]]
   
   set domainBox [selection := fromMaybe 0 (findIndex (==qualification exid) domains) ]
   
   set f [ layout := margin 20 $ row 30 [fill $ widget leftPanel, fill $ widget rightPanel]
         , size := sz 600 450]
   
   let getExerciseList = do
          i <- get domainBox selection 
          b <- get experimentalBox checked
          return (selectExercises b (domains !! i) exs)
       currentExercise = do
          i  <- get exerciseList selection
          xs <- getExerciseList
          return $ if i>=0 && length xs > i then Just (xs !! i) else Nothing
       fillExerciseList b = do
          xs <- getExerciseList
          let ys = [ description ex | Some ex <- xs ]
              isCode (Some ex) = exerciseId ex == exid
              mi = if b then findIndex isCode xs else Nothing
          set exerciseList [items := ys, selection := fromMaybe 0 mi]
          fillOwnText
       fillOwnText = do 
          mex <- currentExercise
          case mex of
             Nothing  -> return ()
             Just (Some ex) -> do
                dif <- get difficultySlider selection
                txt <- suggestTermFor (toEnum dif) (Some ex)
                set ownTextView [text := txt]
   fillExerciseList True
   
   ref <- currentExercise >>= createControl
   pi  <- exerciseInfoPanel leftPanel ref
   notifyObservers ref
   
   set leftPanel [layout := column 10 
      [ fill (widget pi)
      , hstretch $ label "Difficulty (very easy - very difficult)", row 10 [hfill $ widget difficultySlider, widget randomButton]
      , hstretch $ label "Enter your own assignment (or press Random button)", fill $ widget ownTextView
      , row 10  [widget cancelButton, hglue, widget goButton]]]
   
    
   set domainBox       [on select  := fillExerciseList False >> currentExercise >>= setValue ref]
   set experimentalBox [on command := fillExerciseList False >> currentExercise >>= setValue ref]
   set exerciseList    [on select  := fillOwnText >> currentExercise >>= setValue ref]
   set randomButton    [on command := fillOwnText]
   
   -- event handler for go button
   let goCommand stop = do
          txt <- get ownTextView text
          mex <- currentExercise
          case mex of
             Nothing  -> errorDialog f "Error" "No exercise selected"
             Just ex -> do  
                merr <- thisExerciseFor txt ex session
                case merr of
                   Nothing  -> stop Nothing
                   Just err -> errorDialog f "Error" ("Parse error: " ++ err)

   showModal f $ \stop -> do 
      set cancelButton [on command := stop Nothing]
      set goButton     [on command := goCommand stop]
   return () 
   
selectExercises :: Bool -> String -> [Some Exercise] -> [Some Exercise]
selectExercises b d = filter $ \(Some ex) ->  
   qualification ex == d && (b || isPublic ex)

myGrey = rgb 230 230 230