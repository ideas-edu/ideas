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
import Service.ExerciseList
import Service.ExercisePackage

packageList :: [Some ExercisePackage]
packageList = packages

domains :: [String]
domains = sort $ nub 
   [ domain (exerciseCode (exercise e)) | Some e <- packageList ]

defaultAssignment :: Some ExercisePackage
defaultAssignment = head packages

newAssignmentDialog :: Window a -> Session -> IO ()
newAssignmentDialog w session = do
   -- create frame
   f <- dialog w [text := "New Assignment", bgcolor := white] 
   -- current exercise
   code <- currentCode session
   -- Left Panel
   leftPanel <- panel f []
   randomButton     <- button leftPanel [text := "Random"]
   cancelButton     <- button leftPanel [text := "Cancel"]
   goButton         <- button leftPanel [text := "Go"]
   difficultySlider <- hslider leftPanel True 1 10 [selection := 5]
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
   
   set domainBox [selection := fromMaybe 0 (findIndex (==domain code) domains) ]
   
   set f [ layout := margin 20 $ row 30 [fill $ widget leftPanel, fill $ widget rightPanel]
         , size := sz 600 450]
   
   let getPackageList = do
          i <- get domainBox selection 
          b <- get experimentalBox checked
          return (getPackages b (domains !! i))
       currentPackage = do
          i  <- get exerciseList selection
          xs <- getPackageList
          return $ if i>=0 && length xs > i then Just (xs !! i) else Nothing
       fillPackageList b = do
          xs <- getPackageList
          let ys = [ description (exercise pkg) | Some pkg <- xs ]
              isCode (Some pkg) = exerciseCode (exercise pkg) == code
              mi = if b then findIndex isCode xs else Nothing
          set exerciseList [items := ys, selection := fromMaybe 0 mi]
          fillOwnText
       fillOwnText = do 
          mpkg <- currentPackage
          case mpkg of
             Nothing  -> return ()
             Just (Some pkg) -> do
                dif <- get difficultySlider selection
                txt <- suggestTermFor dif (Some (exercise pkg))
                set ownTextView [text := txt]
   fillPackageList True
   
   ref <- currentPackage >>= createControl
   pi  <- exerciseInfoPanel leftPanel ref
   notifyObservers ref
   
   set leftPanel [layout := column 10 
      [ fill (widget pi)
      , hstretch $ label "Difficulty", row 10 [hfill $ widget difficultySlider, widget randomButton]
      , hstretch $ label "Enter your own assignment (or press Random button)", fill $ widget ownTextView
      , row 10  [widget cancelButton, hglue, widget goButton]]]
   
    
   set domainBox       [on select  := fillPackageList False >> currentPackage >>= setValue ref]
   set experimentalBox [on command := fillPackageList False >> currentPackage >>= setValue ref]
   set exerciseList    [on select  := fillOwnText >> currentPackage >>= setValue ref]
   set randomButton    [on command := fillOwnText]
   
   -- event handler for go button
   let goCommand stop = do
          txt  <- get ownTextView text
          mpkg <- currentPackage
          case mpkg of
             Nothing  -> errorDialog f "Error" "No exercise selected"
             Just pkg -> do  
                merr <- thisExerciseFor txt pkg session
                case merr of
                   Nothing  -> stop Nothing
                   Just err -> errorDialog f "Error" ("Parse error: " ++ err)

   showModal f $ \stop -> do 
      set cancelButton [on command := stop Nothing]
      set goButton     [on command := goCommand stop]
   return () 
   
getPackages :: Bool -> String -> [Some ExercisePackage]
getPackages b d = filter p packageList
 where 
    p (Some pkg) = 
       let ex = exercise pkg
       in domain (exerciseCode ex) == d && (b || isPublic ex)

myGrey = rgb 230 230 230