-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Exports a subset of Data.Generics.Uniplate.Direct (the @Uniplate@ type
-- class and its utility plus constructor functions)
--
-----------------------------------------------------------------------------
module Ideas.Common.Utils.Uniplate
   ( -- * Uniplate type class and utility functions
     Uniplate
   , children, contexts, descend, descendM, holes, para
   , rewrite, rewriteM, transform, transformM, uniplate, universe
     -- * Instance constructors
   , (|-), (|*), (||*), plate
   ) where

import Data.Generics.Uniplate.Direct