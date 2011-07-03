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
-- Exports a subset of Data.Generics.Uniplate
--
-----------------------------------------------------------------------------
module Common.Uniplate {-# DEPRECATED "import Data.Generics.Uniplate directly" #-}
   ( -- * Uniplate type class and utility functions
     Uniplate(..), universe, children, holes
   , transform, transformM, descend, descendM, rewrite, rewriteM
   ) where

import Data.Generics.Uniplate