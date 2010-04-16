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
module About (aboutIdeas) where

import Graphics.UI.WX
import Main.Options (versionText)

aboutIdeas :: Window a -> IO ()
aboutIdeas w = do
   d   <- dialog w [text := "IDEAS: Intelligent Domain-Specific Exercise Assistants", bgcolor := white]
   p   <- panel  d []
   ok  <- button d [text := "Ok"]
   png <- imageCreateFromFile "ideas.jpg"
   img <- panel p [on paint := \dc _ -> drawImage dc png (pt 0 0) [], size := sz 160 88]
   set p [ bgcolor := white
         , layout := row 10 [centre (widget img), centre (column 10 
              [ label "Project homepage:" 
              , label "   http://ideas.cs.uu.nl/"
              , label "" 
              , label versionText ])]]
   set d [layout := margin 25 $ column 50 [widget p, centre (widget ok)], size := sz 400 250]
   showModal d (\stop -> set ok [on command := stop Nothing])
   return ()