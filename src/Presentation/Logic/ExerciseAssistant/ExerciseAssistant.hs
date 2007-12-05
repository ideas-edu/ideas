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

-- Equations model
-- import LogicDutchResources
-- import LogicEnglishResources()
-- import LogicFeedBack(feedback)
-- import LogicGenerator()
-- import LogicParser()

main :: IO ()
main =
    do initGUI
       windowXmlM <- xmlNew "bin/exerciseassistant.glade"
       let windowXml = case windowXmlM of
            (Just windowXml) -> windowXml
            Nothing -> error "Can't find the glade file \"exerciseassistant.glade\" in the bin subdirectory of the current directory"
       window <- xmlGetWidget windowXml castToWindow "window"
       onDelete window deleteEvent
       onDestroy window destroyEvent
       widgetShowAll window
       mainGUI

deleteEvent :: Event -> IO Bool
deleteEvent = const (return False)

destroyEvent :: IO ()
destroyEvent = do mainQuit
