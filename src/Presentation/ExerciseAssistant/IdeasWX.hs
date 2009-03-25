-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
{-# OPTIONS -fglasgow-exts #-}
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore

--import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Glade
--import System.Glib.GObject (GObjectClass)
-- import Data.Maybe
import Data.List
import System.Directory
import Session
--import SupplyArguments
--import Common.Transformation
--import Control.Monad
--import Data.IORef
import Service.ExerciseList
import Service.Options (versionText)

domains :: [String]
domains = sort $ nub [ domain e | Some e <- exerciseList ]

title :: String
title = "Exercise Assistant: " ++ versionText

main :: IO ()
main = start exerciseFrame

exerciseFrame :: IO ()
exerciseFrame = do 
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
   set bp [layout := column 10
      [ row 10 [widget backButton, hglue, widget readyButton, widget submitButton] 
      , row 10 [widget hintButton, widget stepButton, widget autoButton {-, widget workedoutButton-}]
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
   session <- makeSession (head exerciseList)
       
   {- let fillRuleBox = do
          -- names <- ruleNames session
          -- set ruleBox [items := names]
          return () -}

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
   
   {-
   onChanged ruleBox $ do
      (iterBegin, iterEnd) <- textBufferGetSelectionBounds entryBuffer
      posBegin <- textIterGetOffset iterBegin
      posEnd   <- textIterGetOffset iterEnd
      cur      <- get entryBuffer textBufferText
      mloc     <- subTermAtIndices cur posBegin posEnd session
      mi       <- comboBoxGetActive ruleBox

      when (isJust mi) $ do
         comboBoxSetActive ruleBox (-1)
         Some rule <- getRuleAtIndex (fromJust mi) session
         case (mloc, hasArguments rule) of
            (Nothing, _) | posBegin /= posEnd ->
               textBufferSetText feedbackBuffer "Invalid selection: not a subterm"
                 
            (_, False) -> do
               (txt, ok) <- applyRuleAtIndex (fromJust mi) mloc [] session
               textBufferSetText feedbackBuffer txt
               when ok updateAll
                    
            _ -> do
               ref <- newIORef Nothing 
               w   <- argumentWindow ref rule
               onDestroy w $ do
                  result <- readIORef ref
                  case result of 
                     Nothing -> return ()
                     Just list -> do
                        (txt, ok) <- applyRuleAtIndex (fromJust mi) mloc list session
                        textBufferSetText feedbackBuffer txt
                        when ok updateAll
               return () -}

newAssignmentFrame :: Session -> IO () -> IO (Frame ())
newAssignmentFrame session onExit = do
   f <- frame [text := "New Assignment", bgcolor := white] 
   windowMakeModal f True
   
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
   
   let getExerciseList = do
          i <- get domainBox selection 
          b <- get experimentalBox checked
          return (getExercises b (domains !! i))
       currentExercise = do
          i  <- get exerciseList selection
          xs <- getExerciseList
          return $ if i>=0 && length xs > i then Just (xs !! i) else Nothing
       fillExerciseList = do
          xs <- getExerciseList
          let ys = [ description e | Some e <- xs ]
          set exerciseList [items := ys, selection := 0]
          fillOwnText
       fillOwnText = do
          mEx <- currentExercise
          case mEx of
             Just se -> do
                dif <- get difficultySlider selection
                txt <- suggestTermFor dif se
                set ownTextView [text := txt]
             Nothing -> return ()
   fillExerciseList
    
   set domainBox       [on select  := fillExerciseList]
   set experimentalBox [on command := fillExerciseList]
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
      mEx  <- currentExercise
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

getExercises :: Bool -> String -> [Some Exercise]
getExercises b d = filter p exerciseList
 where p (Some e) = domain e == d && (b || status e == Stable) 

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