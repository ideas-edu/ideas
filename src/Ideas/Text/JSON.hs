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
-- Support for JavaScript Object Notation (JSON) and remote procedure calls using
-- JSON. JSON is a lightweight alternative for XML.
--
-----------------------------------------------------------------------------

module Ideas.Text.JSON
   ( module Ideas.Text.JSON.Data
   , module Ideas.Text.JSON.Builder
   , module Ideas.Text.JSON.Decoder
   , module Ideas.Text.JSON.Class
   , module Ideas.Text.JSON.RPC
   ) where

import Ideas.Text.JSON.Builder
import Ideas.Text.JSON.Data
import Ideas.Text.JSON.Decoder
import Ideas.Text.JSON.Class
import Ideas.Text.JSON.RPC