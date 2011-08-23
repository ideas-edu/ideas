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
module Text.HTML
   ( HTML, HTMLBuilder, showHTML
   , htmlPage, link
   , h1, h2, h3, h4, h5, h6
   , preText, ul, table
   , text, image, space, spaces, highlightXML
   , para, ttText, hr, br, pre, bullet
   , divClass, spanClass
     -- HTML generic attributes
   , idA, classA, styleA, titleA
     -- Font style elements
   , tt, italic, bold, big, small
   ) where

import Control.Monad
import Data.Char
import Data.List
import Prelude hiding (div)
import Text.XML hiding (text)
import qualified Text.XML as XML

type HTML = XML

type HTMLBuilder = XMLBuilder

showHTML :: HTML -> String
showHTML = compactXML

-- html helper functions
htmlPage :: String -> Maybe String -> HTMLBuilder -> HTML
htmlPage title css body = makeXML "html" $ do
   element "head" $ do
      unless (null title) $
         element "title" (text title)
      case css of
         Nothing -> return ()
         Just n  -> element "link" $ do
            "rel"  .=. "STYLESHEET"
            "href" .=. n
            "type" .=. "text/css"
   element "body" body

link :: String -> HTMLBuilder -> HTMLBuilder
link url body = element "a" $
   ("href" .=. url) >> body

h1, h2, h3, h4, h5, h6 :: String -> HTMLBuilder
h1 = element "h1" . text
h2 = element "h2" . text
h3 = element "h3" . text
h4 = element "h4" . text
h5 = element "h5" . text
h6 = element "h6" . text

para :: HTMLBuilder -> HTMLBuilder
para = element "p"

preText :: String -> HTMLBuilder
preText = pre . text

pre :: HTMLBuilder -> HTMLBuilder
pre = element "pre"

hr :: HTMLBuilder
hr = tag "hr"

br :: HTMLBuilder
br = tag "br"

ttText :: String -> HTMLBuilder
ttText = tt . text

ul :: [HTMLBuilder] -> HTMLBuilder
ul = element "ul" . mapM_ (element "li")

-- | First argument indicates whether the table has a header or not
table :: Bool -> [[HTMLBuilder]] -> HTMLBuilder
table b rows = element "table" $ do
   "border" .=. "1"
   forM_ (zip [0::Int ..] rows) $ \(i, r) ->
      element "tr" $ do
         "class" .=. getClass i
         mapM_ ((if i==0 then classA "topCell" else id) . element "td") r
 where
   getClass i
      | i == 0 && b = "topRow"
      | even i      = "evenRow"
      | otherwise   = "oddRow"

spaces :: Int -> HTMLBuilder
spaces n = replicateM_ n space

space, bullet :: HTMLBuilder
space  = XML.unescaped "&nbsp;"
bullet = XML.unescaped "&#8226;"

image :: String -> HTMLBuilder
image n = element "img" ("src" .=. n)

text :: String -> HTMLBuilder
text = XML.text

divClass :: String -> HTMLBuilder -> HTMLBuilder
divClass n = classA n . element "div"

spanClass :: String -> HTMLBuilder -> HTMLBuilder
spanClass n = classA n . element "span"

-- A simple XML highlighter
highlightXML :: Bool -> XML -> HTMLBuilder
highlightXML nice
   | nice      = builder . highlight . makeXML "pre" . text . showXML
   | otherwise = builder . highlight . makeXML "tt"  . text . compactXML
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

-- | Renders as teletype or monospaced text.
tt :: HTMLBuilder -> HTMLBuilder
tt = element "tt"

-- | Renders as italic text style.
italic :: HTMLBuilder -> HTMLBuilder
italic = element "i"

-- | Renders as bold text style.
bold :: HTMLBuilder -> HTMLBuilder
bold = element "b"

-- BIG: Renders text in a "large" font.
big :: HTMLBuilder -> HTMLBuilder
big = element "big"

-- SMALL: Renders text in a "small" font.
small :: HTMLBuilder -> HTMLBuilder
small = element "small"