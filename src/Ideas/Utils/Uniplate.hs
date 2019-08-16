-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
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

module Ideas.Utils.Uniplate
   ( -- * Uniplate type class and utility functions
     Uniplate
   , children, contexts, descend, descendM, holes, para
   , rewrite, rewriteM, transform, transformM, uniplate, universe
     -- * Instance constructors
   , (|-), (|*), (||*), plate
   ) where

import Data.Generics.Uniplate.Direct