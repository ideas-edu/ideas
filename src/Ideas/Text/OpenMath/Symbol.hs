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
-----------------------------------------------------------------------------

module Ideas.Text.OpenMath.Symbol where

type Symbol = (Maybe String, String)

-- * Constructor functions

makeSymbol :: String -> String -> Symbol
makeSymbol = (,) . Just

extraSymbol :: String -> Symbol
extraSymbol = (,) Nothing

-- * Selector functions

dictionary :: Symbol -> Maybe String
dictionary = fst

symbolName :: Symbol -> String
symbolName = snd

-- * Utility function

showSymbol :: Symbol -> String
showSymbol s = maybe "" (++".") (dictionary s) ++ symbolName s