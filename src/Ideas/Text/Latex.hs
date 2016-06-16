-----------------------------------------------------------------------------
-- Copyright 2016, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Support for LaTeX.
--
-----------------------------------------------------------------------------

module Ideas.Text.Latex
   ( ToLatex(..) 
   , array, commas, brackets, parens
   ) where

import Data.List
import Ideas.Common.Utils (ShowString)
   
class ToLatex a where
   toLatex     :: a -> String
   toLatexPrec :: Int -> a -> String
   toLatexList :: [a] -> String
   -- default implementations
   toLatex     = toLatexPrec 0
   toLatexPrec = const toLatex
   toLatexList = brackets . intercalate ", " . map toLatex
   {-# MINIMAL toLatex | toLatexPrec #-}

instance ToLatex a => ToLatex [a] where 
   toLatex     = toLatexList
   toLatexPrec = const toLatexList

instance ToLatex a => ToLatex (Maybe a) where
   toLatexPrec = maybe "" . toLatexPrec
 
instance ToLatex Char where
   toLatex     = return
   toLatexList = id

instance ToLatex Int where
   toLatex = show

instance ToLatex ShowString where
   toLatex = show

commas :: [String] -> String
commas = intercalate ",\\:"

brackets, parens :: String -> String
brackets s = "[" ++ s ++ "]"
parens   s = "(" ++ s ++ ")"

array :: String -> [String] -> String
array s rows = "\\begin{array}{" ++ s ++ "}" ++ intercalate "\\\\" rows ++ "\\end{array}"