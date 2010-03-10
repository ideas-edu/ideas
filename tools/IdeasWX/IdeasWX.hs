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
module Main (main) where

-- import Configuration
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.List
import System.Directory
import Session
import Service.ExerciseList
import Service.ExercisePackage
import Service.Options (versionText)
import Observable
import About
import Data.Maybe
import qualified Service.TypedAbstractService as TAS
import Common.Strategy (prefixToSteps)
--import Domain.Programming.Exercises (heliumExercise, isortExercise)

packageList = {- Some heliumExercise : Some isortExercise : -} packages

domains :: [String]
domains = sort $ nub [ domain (exerciseCode (exercise e)) | Some e <- packageList ]

title :: String
title = "IdeasWX Exercise Assistant"

main :: IO ()
main = start exerciseFrame

exerciseFrame :: IO ()
exerciseFrame = do 
   -- cfg <- readConfig
   session <- makeSession (head packageList)

   f <- frame [text := title, bgcolor := white]
   bmp <- iconCreateFromFile "ideas.ico" sizeNull
   topLevelWindowSetIcon f bmp
   
   -- Menu bar
   mfile  <- menuPane [text := "&File"]
   mexit  <- menuItem mfile [text := "&Exit", help := "Quit the application"] 
   mview  <- menuPane [text := "&View"]
   mdebug <- menuItem mview [text := "&Debug frame", help := "Open a frame for debugging"]
   mhelp  <- menuPane [text := "&Help"]
   mabout <- menuItem mhelp [text := "&About", help := "About Ideas"]
   set f [ menuBar := [mfile, mview, mhelp]
         , on (menu mexit)  := close f
         , on (menu mabout) := aboutIdeas f 
         , on (menu mdebug) := debugFrame session
         ] 
         
   -- Status bar
   field1 <- statusField []
   field2 <- statusField [statusWidth := 200, text := versionText]
   set f [statusBar := [field1,field2]] 
   
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
                      set imagePanel [on paint := \dc _ -> drawImage dc imageOUNL (pt 0 0) [], size := sz 233 86]
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
   
   let updateDescription = do
          descr <- currentDescription session
          set domainText [text := descr]
          
   let updateCurrent = do 
          txt <- currentText session
          set assignmentView [text := txt]
          set entryView [text := txt]
          
   let updateDerivation = do
          der <- derivationText session
          set derivationView [text := der]

   let updateProgress = do 
          (x, y) <- progressPair session
          set progressLabel [text := show x ++ "/" ++ show y]
          set progressBar [selection := if y==0 then 100 else (100*x) `div` y] 

   set feedbackView [text := "Welcome to the Exercise Assistant!"]
   
   addObserver_ session updateDescription
   addObserver_ session updateCurrent
   addObserver_ session updateDerivation
   addObserver_ session updateProgress
   notifyObservers session
  
   assignmentFrame <- newAssignmentFrame session
   
   -- bind events
   set f [on closing := do
      windowDestroy assignmentFrame
      windowDestroy f
      return ()]
 
   set newButton [on command := do
      txt <- suggestTerm 5 session -- TODO: fix difficulty!!
      thisExercise txt session ]
 
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
      txt <- nextStep session 0
      set feedbackView [text := txt] ]
 
   set backButton [on command := do
      txt1 <- get entryView text
      txt2 <- get assignmentView text
      if txt1 == txt2 
         then undo session
         else set entryView [text := txt2]]
 
   set submitButton [on command := do
      cur <- get entryView text
      txt <- submitText cur session
      set feedbackView [text := txt] ]

   focusOn f

newAssignmentFrame :: Session -> IO (Frame ())
newAssignmentFrame session = do
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
          let ys = [ description (exercise pkg) | Some pkg <- xs ]
          set exerciseList [items := ys, selection := 0]
          fillOwnText
       fillOwnText = do
          mPkg <- currentPackage
          case mPkg of
             Just (Some pkg) -> do
                dif <- get difficultySlider selection
                txt <- suggestTermFor dif (Some (exercise pkg))
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
      set f [visible := False] ]
   
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

debugFrame :: Session -> IO ()
debugFrame session = do
   f <- frame [text := "Debug", bgcolor := white]
   rulebox <- singleListBox  f []
   b   <- button f [text := "Apply", on command := do
             n <- get rulebox selection
             nextStep session n
             return ()]
   txt <- staticText f [text := "(no rules)"]
   
   stp <- singleListBox f []
   
   set f [layout := column 10 [row 10 [fill (widget rulebox), widget b, widget txt], fill (widget stp)], size := sz 400 200]
   
   let execObserver f = addObserver session f >> getValue session >>= f
   
   execObserver $ \(Some st) -> do
      let result = TAS.allfirsts (currentState (getDerivation st))
          rs     = [ show r | (r, _, _) <- concat result ]
          msg    = case length rs of
                      0 -> "(no rules)"
                      1 -> "(1 rule)"
                      n -> "(" ++ show n ++ " rules)"
      set rulebox [items := rs, selection := 0]
      set txt [text := msg]
   execObserver $ \(Some st) -> do
      let xs = maybe [] (map show . prefixToSteps) (TAS.prefix (currentState (getDerivation st)))
      set stp [items := xs]

getPackages :: Bool -> String -> [Some ExercisePackage]
getPackages b d = filter p packageList
 where 
    p (Some pkg) = 
       let ex = exercise pkg
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

myGrey = rgb 230 230 230