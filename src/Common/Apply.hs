-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- This module defines the type class Apply and some related utility functions.
-- Deprecated.
--
-----------------------------------------------------------------------------
module Common.Apply {-# DEPRECATED "Use Common.Classes instead" #-}
   ( Apply(..), applicable, applyD, applyM
   ) where

import Common.Classes