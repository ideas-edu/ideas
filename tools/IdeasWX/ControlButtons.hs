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
module ControlButtons (controlButtons) where

import Session
import Graphics.UI.WX
import Observable

controlButtons :: Window a -> Session -> IO String -> (String -> IO ()) -> IO (Panel ()) 
controlButtons w session input output = do
   p <- panel w []
   -- create buttons
   backButton   <- button p [text := "Back"]
   readyButton  <- button p [text := "Ready"]
   submitButton <- button p [text := "Submit"]
   hintButton   <- button p [text := "Hint"]
   stepButton   <- button p [text := "Step"]
   autoButton   <- button p [text := "Auto step"]   
   
   -- install event handlers
   set readyButton [ on command := do
      txt <- readyText session
      output txt ]
   
   set hintButton [ on command := do
      txt <- hintText session
      output txt ]
   
   set stepButton [ on command := do
      txt <- stepText session
      output txt ]
       
   set autoButton [on command := do
      txt <- nextStep session
      output txt ]

   set backButton [ on command := do
      txt1 <- input
      txt2 <- currentText session
      if txt1 == txt2 
         then undo session
         else notifyObservers session ]
   
   set submitButton [ on command := do
      cur <- input
      txt <- submitText cur session 
      output txt ]
      
   -- layout
   set p [layout := grid 10 10
            [ [widget backButton, widget readyButton, widget submitButton] 
            , [widget hintButton, widget stepButton, widget autoButton ]
            ] ]
   
   return p