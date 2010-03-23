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
module Text.OpenMath.Symbol where

data Symbol = Symbol
   { dictionary :: Maybe String
   , symbolName :: String
   }
 deriving (Eq, Ord)

instance Show Symbol where
   show s = maybe "" (++".") (dictionary s) ++ symbolName s

instance Read Symbol where
   readsPrec _ s = 
      case break (=='.') s of
         (xs,_:ys) -> [(makeSymbol xs ys, "")]
         _         -> [(extraSymbol s, "")]
               
makeSymbol :: String -> String -> Symbol
makeSymbol = Symbol . Just

extraSymbol :: String -> Symbol
extraSymbol = Symbol Nothing