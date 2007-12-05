{--------------------------------------------------- 
This is an interactive system in which a student can 
incrementally solve proposition formulae.

Copyright (c)        2006 - 2007 

Johan Jeuring, Harrie Passier, Bastiaan Heeren, Alex Gerdes
and Arthur van Leeuwen
---------------------------------------------------}

module Main where

-- GTK2Hs Imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- To keep the state and pass it to the event handlers (that are in IO)
import Data.IORef

-- Equations model
import Common.Assignment
import Common.Transformation
import Common.Strategy
import Domain.Logic
import Domain.Logic.Solver.LogicGenerator

main :: IO ()
main =
    do  initGUI

        -- read Glade file (FIXME hardcoded path)
        windowXmlM <- xmlNew "bin/exerciseassistant.glade"
        let windowXml = case windowXmlM of
             (Just windowXml) -> windowXml
             Nothing -> error "Can't find the glade file \"exerciseassistant.glade\" in the bin subdirectory of the current directory"
        window <- xmlGetWidget windowXml castToWindow "window"
        assignmentView <- xmlGetWidget windowXml castToTextView "assignmentView"
        derivationView <- xmlGetWidget windowXml castToTextView "derivationView"
        entryView <- xmlGetWidget windowXml castToTextView "entryView"
        feedbackView <- xmlGetWidget windowXml castToTextView "feedbackView"
        readyButton <- xmlGetWidget windowXml castToButton "readyButton"
        hintButton <- xmlGetWidget windowXml castToButton "hintButton"
        stepButton <- xmlGetWidget windowXml castToButton "stepButton"
        undoButton <- xmlGetWidget windowXml castToButton "undoButton"
        submitButton <- xmlGetWidget windowXml castToButton "submitButton"

        -- get buffers from views
        assignmentBuffer <- textViewGetBuffer assignmentView 
        derivationBuffer <- textViewGetBuffer derivationView
        entryBuffer <- textViewGetBuffer entryView 
        feedbackBuffer <- textViewGetBuffer feedbackView 

        -- initialize assignment
        let initialAssignment = parser logicAssignment $ "x/\\ (~y || ~(~z /\\ x))"
        textBufferSetText assignmentBuffer (prettyPrinter logicAssignment $ initialAssignment)
        textBufferSetText entryBuffer (prettyPrinter logicAssignment $ initialAssignment)

        assignmentState <- newIORef initialAssignment

        -- bind events
        onDelete window deleteEvent
        onDestroy window destroyEvent

        onClicked readyButton $ 
            do  textBufferSetText feedbackBuffer "ready"

        onClicked hintButton $
            do
                currentAssignment <- readIORef assignmentState
--                case giveHint logicAssignment currentAssignment of
--                    (doc, rule) -> textBufferSetText feedbackBuffer "test"
                textBufferSetText feedbackBuffer "hint"

        onClicked stepButton $
            do 
                currentAssignment <- readIORef assignmentState
                textBufferSetText feedbackBuffer "step"

        onClicked undoButton $
            do 
                currentAssignment <- readIORef assignmentState
                textBufferSetText feedbackBuffer "undo"

        onClicked submitButton $
            do 
                currentAssignment <- readIORef assignmentState
                textBufferSetText feedbackBuffer "submit"

        -- show widgets and run GUI
        widgetShowAll window
        mainGUI

deleteEvent :: Event -> IO Bool
deleteEvent = const (return False)

destroyEvent :: IO ()
destroyEvent = do mainQuit


logicAssignment :: Assignment LogicInContext
logicAssignment = Assignment
   { parser        = Domain.Logic.inContext . fst . parseLogic
   , prettyPrinter = ppLogicInContext
   , equivalence   = \x y -> noContext x `eqLogic` noContext y
   , finalProperty = isDNF . noContext
   , ruleset       = map liftLogicRule logicRules
   , strategy      = unlabel toDNF
   , generator     = fmap Domain.Logic.inContext $ arbLogic defaultConfig
   , language      = English
   }  
