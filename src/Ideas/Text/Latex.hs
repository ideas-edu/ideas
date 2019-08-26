{-# LANGUAGE OverloadedStrings #-}
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
-- Support for LaTeX.
--
-----------------------------------------------------------------------------

module Ideas.Text.Latex
   ( Latex, ToLatex(..)
   , array, commas, brackets, parens
   , command
   ) where

import Data.List
import Data.Semigroup as Sem
import Data.String

newtype Latex = L { showLatex :: String }

instance Show Latex where
   show = showLatex

instance IsString Latex where
   fromString = L

instance Sem.Semigroup Latex where
   L xs <> L ys = L (xs <> ys)

instance Monoid Latex where
   mempty  = L []
   mappend = (<>)

class ToLatex a where
   toLatex     :: a -> Latex
   toLatexPrec :: Int -> a -> Latex
   toLatexList :: [a] -> Latex
   -- default implementations
   toLatex     = toLatexPrec 0
   toLatexPrec = const toLatex
   toLatexList = brackets . commas . map toLatex
   {-# MINIMAL toLatex | toLatexPrec #-}

instance ToLatex a => ToLatex [a] where
   toLatex     = toLatexList
   toLatexPrec = const toLatexList

instance ToLatex a => ToLatex (Maybe a) where
   toLatexPrec = maybe mempty . toLatexPrec

instance ToLatex Char where
   toLatex     = fromString . return
   toLatexList = fromString

instance ToLatex Int where
   toLatex = fromString . show

commas :: [Latex] -> Latex
commas = mconcat . intersperse ",\\:"

brackets, parens :: Latex -> Latex
brackets s = "[" <> s <> "]"
parens   s = "(" <> s <> ")"

array :: String -> [[Latex]] -> Latex
array s rows = "\\begin{array}{" <> fromString s <> "}"
   <> mconcat (intersperse "\\\\" (map (mconcat . intersperse " & ") rows))
   <> "\\end{array}"

command :: String -> Latex
command s = toLatex ("\\" ++ s ++ " ")