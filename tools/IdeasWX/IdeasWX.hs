{-# OPTIONS -fglasgow-exts #-}
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
module Main where

import Configuration
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.List
import System.Directory
import Session
import qualified Service.ExerciseList as SE
import Service.Options (versionText)
--import Domain.Programming.Exercises (heliumExercise, isortExercise)

packageList = {- Some heliumExercise : Some isortExercise : -} SE.packages

domains :: [String]
domains = sort $ nub [ domain (exerciseCode (SE.exercise e)) | Some e <- packageList ]

title :: String
title = "IdeasWX Exercise Assistant: " ++ versionText

main :: IO ()
main = start exerciseFrame

exerciseFrame :: IO ()
exerciseFrame = do 
   cfg <- readConfig

   f <- frame [text := title, bgcolor := white]
   
   -- Left Panel
   leftPanel <- panel f []
   
   -- Domain Panel
   domainPanel  <- panel leftPanel []
   domainText   <- staticText leftPanel []
   newButton    <- button domainPanel [text := "New exercise"]
   changeButton <- button domainPanel [text := "Change exercise"]
   set domainPanel [layout := row 10 [hfill $ widget domainText, hglue, widget newButton, widget changeButton]]
   
   -- Button Panel
   bp <- panel leftPanel []
   backButton      <- button bp [text := "Back"]
   readyButton     <- button bp [text := "Ready"]
   submitButton    <- button bp [text := "Submit"]
   hintButton      <- button bp [text := "Hint"]
   stepButton      <- button bp [text := "Step"]
   autoButton      <- button bp [text := "Auto step"]
   -- workedoutButton <- button bp [text := "Worked-out exerc"]
   
   -- ruleBox        <- comboBox bp [] 
   set bp [layout := grid 10 10
      [ [widget backButton, widget readyButton, widget submitButton] 
      , [widget hintButton, widget stepButton, widget autoButton {-, widget workedoutButton-}]
      ]]
   
   assignmentView <- textCtrl leftPanel [bgcolor := myGrey]
   entryView      <- textCtrl leftPanel [bgcolor := myGrey]
   
   set leftPanel [layout := column 10
      [ hfill $ widget domainPanel, hstretch $ label "Assignment", fill $ widget assignmentView
      , hstretch $ label "Enter modified term", fill $ widget entryView
      , row 0 [hglue, widget bp, hglue]]]
   
   -- Right Panel
   rightPanel     <- panel f []
   derivationView <- textCtrl rightPanel [bgcolor := myGrey] 
   feedbackView   <- textCtrl rightPanel [bgcolor := myGrey]
   progressBar    <- hgauge rightPanel 100 []
   progressLabel  <- staticText rightPanel []
   imagePanel     <- panel rightPanel [bgcolor := white]
   mfull <- searchPath "ounl.jpg"
   case mfull of 
      Just full -> do imageOUNL <- imageCreateFromFile full
                      set imagePanel [on paint := \dc _ -> drawImage dc imageOUNL (pt 0 0) [], size := sz 230 30]
      Nothing   -> return ()
   set rightPanel [layout := column 10 
      [ hstretch $ label "Derivation", fill $ widget derivationView
      , row 10 [hfill $ widget progressBar, fill $ widget progressLabel]
      , hstretch $ label "Feedback", fill $ widget feedbackView
      , row 0 [hglue, widget imagePanel]]]
   
   set f [ layout := margin 20 $ row 30 [fill $ widget leftPanel, fill $ widget rightPanel] 
         , size := sz 800 600]

   mapM_ (flip textCtrlSetEditable False) [assignmentView, derivationView, feedbackView]

   -- initialize exercise
   session <- makeSession (head packageList)

   let updateAll = do
          descr <- currentDescription session
          set domainText [text := descr]
          txt <- currentText session
          set assignmentView [text := txt]
          set entryView [text := txt]
          der <- derivationText session
          set derivationView [text := der]
          (x, y) <- progressPair session
          set progressLabel [text := show x ++ "/" ++ show y]
          set progressBar [selection := if y==0 then 100 else (100*x) `div` y] 

   set feedbackView [text := "Welcome to the Exercise Assistant!"]
   
   updateAll
   assignmentFrame <- newAssignmentFrame session updateAll
   
   -- bind events
   set f [on closing := do
      windowDestroy assignmentFrame
      windowDestroy f
      return ()]
 
   set newButton [on command := do
      txt <- suggestTerm 5 session -- TODO: fix difficulty!!
      ms  <- thisExercise txt session
      case ms of 
         Just _  -> return () 
         Nothing -> updateAll]
 
   set changeButton [on command := do
      windowMakeModal assignmentFrame True
      set assignmentFrame [visible := True]]
     
   set readyButton [on command := do
      txt <- readyText session
      set feedbackView [text := txt]]
 
   set hintButton [on command := do
      txt <- hintText session
      set feedbackView [text := txt]]
          
   set stepButton [on command := do
      txt <- stepText session
      set feedbackView [text := txt]]
       
   set autoButton [on command := do
      (txt, ok) <- nextStep session
      set feedbackView [text := txt]
      when ok updateAll]
 
   set backButton [on command := do
      txt1 <- get entryView text
      txt2 <- get assignmentView text
      if txt1 == txt2 
         then undo session >> updateAll
         else set entryView [text := txt2]]
 
   set submitButton [on command := do
      cur       <- get entryView text
      (txt, ok) <- submitText cur session
      set feedbackView [text := txt]
      when ok updateAll]

newAssignmentFrame :: Session -> IO () -> IO (Frame ())
newAssignmentFrame session onExit = do
   f <- frame [text := "New Assignment", bgcolor := white, visible := False] 
   -- windowMakeModal f True
   
   -- Left Panel
   leftPanel <- panel f []
   randomButton     <- button leftPanel [text := "Random"]
   cancelButton     <- button leftPanel [text := "Cancel"]
   goButton         <- button leftPanel [text := "Go"]
   difficultySlider <- hslider leftPanel True 1 10 [selection := 5]
   ownTextView      <- textCtrl leftPanel [bgcolor := myGrey]
   
   set leftPanel [layout := column 10 
      [ hstretch $ label "Difficulty", row 10 [hfill $ widget difficultySlider, widget randomButton]
      , hstretch $ vspace 30
      , hstretch $ label "Enter your own assignment (or press Random button)", fill $ widget ownTextView
      , row 10  [widget cancelButton, hglue, widget goButton]]]
      
   -- Right Panel
   rightPanel <- panel f []
   domainBox  <- radioBox rightPanel Vertical domains []
   exerciseList <- singleListBox rightPanel []
   experimentalBox <- checkBox rightPanel [text := "Include experimental", checked := True]
   set rightPanel [layout := column 10 
      [ hstretch $ label "Domain selection", hfill $ widget domainBox
      , hstretch $ vspace 10
      , hstretch $ label "Exercise selection", fill $ widget exerciseList
      , hstretch $ widget experimentalBox]]
   
   set f [ layout := margin 20 $ row 30 [fill $ widget leftPanel, fill $ widget rightPanel]
         , size := sz 600 400]
   
   let getPackageList = do
          i <- get domainBox selection 
          b <- get experimentalBox checked
          return (getPackages b (domains !! i))
       currentPackage = do
          i  <- get exerciseList selection
          xs <- getPackageList
          return $ if i>=0 && length xs > i then Just (xs !! i) else Nothing
       fillPackageList = do
          xs <- getPackageList
          let ys = [ description (SE.exercise pkg) | Some pkg <- xs ]
          set exerciseList [items := ys, selection := 0]
          fillOwnText
       fillOwnText = do
          mPkg <- currentPackage
          case mPkg of
             Just (Some pkg) -> do
                dif <- get difficultySlider selection
                txt <- suggestTermFor dif (Some (SE.exercise pkg))
                set ownTextView [text := txt]
             Nothing -> return ()
   fillPackageList
    
   set domainBox       [on select  := fillPackageList]
   set experimentalBox [on command := fillPackageList]
   set exerciseList    [on select  := fillOwnText]
   set randomButton    [on command := fillOwnText]
   set cancelButton    [on command := close f]
   
   set f [on closing := do 
      windowMakeModal f False 
      -- windowDestroy f
      set f [visible := False]
      onExit]
   
   set goButton [on command := do
      txt  <- get ownTextView text
      mEx  <- currentPackage
      case mEx of 
         Just ex -> do
            merr <- thisExerciseFor txt ex session
            case merr of
               Nothing -> do
                  close f
               Just err -> do
                  errorDialog f "Error" ("Parse error: " ++ err)
         _ -> errorDialog f "Error" "First select an exercise"]

   return f

getPackages :: Bool -> String -> [Some SE.ExercisePackage]
getPackages b d = filter p packageList
 where 
    p (Some pkg) = 
       let ex = SE.exercise pkg
       in domain (exerciseCode ex) == d && (b || status ex == Stable) 

searchPath :: String -> IO (Maybe String)
searchPath filename = rec paths
 where
   paths  = [".", "bin"]
   rec [] = return Nothing
   rec (p:ps) = do
      let full = p ++ "/" ++ filename
      b <- doesFileExist full
      if b then return (Just full) else rec ps

lightBlue = rgb 235 244 255
myGrey = rgb 230 230 230