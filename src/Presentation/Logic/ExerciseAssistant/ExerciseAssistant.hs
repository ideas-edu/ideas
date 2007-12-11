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
import Data.Maybe

-- Equations model
import Common.Assignment
import Common.Transformation
import Common.Strategy
import Domain.Logic

import Control.Monad

main :: IO ()
main = runAssignment dnfAssignment

runAssignment :: Assignment a -> IO ()
runAssignment assignment = 
    do  initGUI

        -- read Glade file (FIXME hardcoded path)
        windowXmlM <- xmlNew "bin/exerciseassistant.glade"
        let windowXml = case windowXmlM of
             Just windowXml -> windowXml
             Nothing -> error "Can't find the glade file \"exerciseassistant.glade\" in the bin subdirectory of the current directory"
        window         <- xmlGetWidget windowXml castToWindow   "window"
        assignmentView <- xmlGetWidget windowXml castToTextView "assignmentView"
        derivationView <- xmlGetWidget windowXml castToTextView "derivationView"
        entryView      <- xmlGetWidget windowXml castToTextView "entryView"
        feedbackView   <- xmlGetWidget windowXml castToTextView "feedbackView"
        readyButton    <- xmlGetWidget windowXml castToButton   "readyButton"
        hintButton     <- xmlGetWidget windowXml castToButton   "hintButton"
        stepButton     <- xmlGetWidget windowXml castToButton   "stepButton"
        undoButton     <- xmlGetWidget windowXml castToButton   "undoButton"
        submitButton   <- xmlGetWidget windowXml castToButton   "submitButton"

        -- get buffers from views
        assignmentBuffer <- textViewGetBuffer assignmentView 
        derivationBuffer <- textViewGetBuffer derivationView
        entryBuffer      <- textViewGetBuffer entryView 
        feedbackBuffer   <- textViewGetBuffer feedbackView 

        -- initialize assignment
        initialAssignment <- randomTerm assignment
        textBufferSetText assignmentBuffer (prettyPrinter assignment $ initialAssignment)
        textBufferSetText entryBuffer (prettyPrinter assignment $ initialAssignment)
        textBufferSetText feedbackBuffer (show (stepsRemaining assignment initialAssignment) ++ " steps remaining")

        derivationState <- newIORef (Start initialAssignment)
        updateDerivation derivationBuffer (prettyPrinter assignment) derivationState

        -- bind events
        onDelete window deleteEvent
        onDestroy window destroyEvent

        onClicked readyButton $ 
            do  textBufferSetText feedbackBuffer "ready"

        onClicked hintButton $
            do
                derivation <- readIORef derivationState
                case giveHint assignment (current derivation) of
                    (doc, rule) -> textBufferSetText feedbackBuffer (show rule ++ ";" ++ showDoc assignment doc)

        onClicked stepButton $
            do 
                derivation <- readIORef derivationState
                case giveStep assignment (current derivation) of
                    (doc, subterm, newterm) -> 
                       textBufferSetText feedbackBuffer $ 
                       "Use " ++ showDoc assignment doc ++ "\nto rewrite subterm\n" ++ 
                       prettyPrinter assignment subterm ++ "\nresulting in\n" ++
                       prettyPrinter assignment newterm

        onClicked undoButton $
            do 
                currentAssignment <- readIORef derivationState
                textBufferSetText feedbackBuffer "undo"

        onClicked submitButton $
            do 
                derivation <- readIORef derivationState
                txt <- get entryBuffer textBufferText
                case feedback assignment (current derivation) txt of
                   SyntaxError doc msug -> 
                      textBufferSetText feedbackBuffer $ 
                         showDoc assignment doc ++ maybe "" (\a -> "\nDid you mean " ++ prettyPrinter assignment a) msug
                   Incorrect doc msug ->
                      textBufferSetText feedbackBuffer $ 
                         showDoc assignment doc ++ maybe "" (\a -> "\nDid you mean " ++ prettyPrinter assignment a) msug
                   Correct doc singleRule -> do
                      let new = either undefined id $ parser assignment txt -- REWRITE !
                      textBufferSetText feedbackBuffer $
                         showDoc assignment doc ++ "\n" ++ 
                            if isJust singleRule 
                            then show (stepsRemaining assignment new) ++ " steps remaining"
                            else "unknown rule"
                      when (isJust singleRule) $ do
                        textBufferSetText assignmentBuffer txt
                        writeIORef derivationState $ Step derivation (fromJust singleRule) new
                        updateDerivation derivationBuffer (prettyPrinter assignment) derivationState
        
        -- show widgets and run GUI
        widgetShowAll window
        mainGUI

deleteEvent :: Event -> IO Bool
deleteEvent = const (return False)

destroyEvent :: IO ()
destroyEvent = do mainQuit

-- move to Common
data Derivation a = Start a | Step (Derivation a) (Rule a) a -- snoc list for fast access to current term

current :: Derivation a -> a
current (Start a)    = a
current (Step _ _ a) = a

showDerivation :: (a -> String) -> Derivation a -> String
showDerivation f (Start a)    = f a
showDerivation f (Step d r a) = showDerivation f d ++ "\n   => [" ++ name r ++ "]\n" ++ f a

updateDerivation :: TextBufferClass w => w -> (a -> String) -> IORef (Derivation a) -> IO ()
updateDerivation buffer pp ref = do
   derivation <- readIORef ref
   textBufferSetText buffer $ showDerivation pp derivation