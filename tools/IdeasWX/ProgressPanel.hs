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
module ProgressPanel (progressPanel) where

import Graphics.UI.WX
import Session
import Observable

progressPanel :: Window a -> Session -> IO (Panel ())
progressPanel w session = do
   p   <- panel w []
   -- create controls
   bar <- hgauge p 100 []
   lab <- staticText p []
   -- event handler
   addObserver_ session $ do
      (x, y) <- progressPair session
      set lab [text := show x ++ "/" ++ show y]
      set bar [selection := if y==0 then 100 else (100*x) `div` y]
   -- set layout
   set p [layout := row 10 [hfill $ widget bar, fill $ widget lab]]
   return p