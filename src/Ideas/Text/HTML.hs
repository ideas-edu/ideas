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
-- A minimal interface for constructing simple HTML pages
-- See http://www.w3.org/TR/html4/
--
-----------------------------------------------------------------------------
module Ideas.Text.HTML
   ( HTML, HTMLBuilder, showHTML
   , string, text
   , htmlPage, link
   , h1, h2, h3, h4, h5, h6
   , preText, ul, table
   , image, space, spaces, highlightXML
   , para, ttText, hr, br, pre, bullet
   , divClass, spanClass
     -- HTML generic attributes
   , idA, classA, styleA, titleA
     -- Font style elements
   , tt, italic, bold, big, small                  
   , module Data.Monoid
   ) where

import Data.Char
import Data.List
import Data.Monoid
import Prelude hiding (div)
import Ideas.Text.XML
import qualified Ideas.Text.XML as XML

type HTML = XML

type HTMLBuilder = XMLBuilder

showHTML :: HTML -> String
showHTML = compactXML

-- html helper functions
htmlPage :: String -> Maybe String -> HTMLBuilder -> HTML
htmlPage title css body = makeXML "html" $
   element "head"
      [ munless (null title) $
           tag "title" (string title)
      , case css of
           Nothing -> mempty
           Just n  -> element "link"
              [ "rel"  .=. "STYLESHEET"
              , "href" .=. n
              , "type" .=. "text/css"
              ]
      ]
   <> tag "body" body
   

link :: String -> HTMLBuilder -> HTMLBuilder
link url body = tag "a" $
   ("href" .=. url) <> body

h1, h2, h3, h4, h5, h6 :: String -> HTMLBuilder
h1 = tag "h1" . string
h2 = tag "h2" . string
h3 = tag "h3" . string
h4 = tag "h4" . string
h5 = tag "h5" . string
h6 = tag "h6" . string

para :: HTMLBuilder -> HTMLBuilder
para = tag "p"

preText :: String -> HTMLBuilder
preText = pre . string

pre :: HTMLBuilder -> HTMLBuilder
pre = tag "pre"

hr :: HTMLBuilder
hr = emptyTag "hr"

br :: HTMLBuilder
br = emptyTag "br"

ttText :: String -> HTMLBuilder
ttText = tt . string

ul :: [HTMLBuilder] -> HTMLBuilder
ul = element "ul" . map (tag "li")

-- | First argument indicates whether the table has a header or not
table :: Bool -> [[HTMLBuilder]] -> HTMLBuilder
table b rows = element "table" $
   ("border" .=. "1") :
   [ element "tr" $
        ("class" .=. getClass i) :
        map ((if i==0 then classA "topCell" else id) . tag "td") r
   | (i, r) <- zip [0::Int ..] rows
   ]   
 where
   getClass i
      | i == 0 && b = "topRow"
      | even i      = "evenRow"
      | otherwise   = "oddRow"

spaces :: Int -> HTMLBuilder
spaces n = mconcat (replicate n space)

space, bullet :: HTMLBuilder
space  = XML.unescaped "&nbsp;"
bullet = XML.unescaped "&#8226;"

image :: String -> HTMLBuilder
image n = tag "img" ("src" .=. n)

divClass :: String -> HTMLBuilder -> HTMLBuilder
divClass n = classA n . tag "div"

spanClass :: String -> HTMLBuilder -> HTMLBuilder
spanClass n = classA n . tag "span"

-- A simple XML highlighter
highlightXML :: Bool -> XML -> HTMLBuilder
highlightXML nice
   | nice      = builder . highlight . makeXML "pre" . string . showXML
   | otherwise = builder . highlight . makeXML "tt"  . string . compactXML
 where
   highlight :: HTML -> HTML
   highlight html = html {content = map (either (Left . f) Right) (content html)}

   -- find <
   f :: String -> String
   f [] = []
   f list@(x:xs)
      | "&lt;/" `isPrefixOf` list = -- close tag
           let (as, bs) = span isAlphaNum (drop 5 list)
           in "<font color='blue'>&lt;/" ++ as ++ "<font color='green'>" ++ g bs
      | "&lt;" `isPrefixOf` list = -- open tag
           let (as, bs) = span isAlphaNum (drop 4 list)
           in "<font color='blue'>&lt;" ++ as ++ "<font color='green'>" ++ g bs
      | otherwise = x : f xs
   -- find >
   g [] = []
   g list@(x:xs)
      | "/&gt;" `isPrefixOf` list =
           "</font>/&gt;</font>" ++ f (drop 5 list)
      | "&gt;" `isPrefixOf` list =
           "</font>&gt;</font>" ++ f (drop 4 list)
      | x=='=' = "<font color='orange'>=</font>" ++ g xs
      | otherwise = x : g xs

-----------------------------------------------------------
-- * HTML generic attributes

idA, classA, styleA, titleA :: String -> HTMLBuilder -> HTMLBuilder
idA    = setA "id"     -- document-wide unique id
classA = setA "class"  -- space-separated list of classes
styleA = setA "style"  -- associated style info
titleA = setA "title"  -- advisory title

setA :: String -> String -> HTMLBuilder -> HTMLBuilder
setA attr value = updateLast $ \e ->
   e { attributes = (attr := value) : attributes e }

-----------------------------------------------------------
-- * Font style elements

-- | Renders as teletype or monospaced Ideas.Text.
tt :: HTMLBuilder -> HTMLBuilder
tt = tag "tt"

-- | Renders as italic text style.
italic :: HTMLBuilder -> HTMLBuilder
italic = tag "i"

-- | Renders as bold text style.
bold :: HTMLBuilder -> HTMLBuilder
bold = tag "b"

-- BIG: Renders text in a "large" font.
big :: HTMLBuilder -> HTMLBuilder
big = tag "big"

-- SMALL: Renders text in a "small" font.
small :: HTMLBuilder -> HTMLBuilder
small = tag "small"