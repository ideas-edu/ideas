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
-- A minimal interface for constructing simple HTML pages
--
-----------------------------------------------------------------------------
module Text.HTML 
   ( HTML, HTMLBuilder, showHTML
   , htmlPage, errorPage, link, h1, h2, h3, preText, ul, table
   , text, image, space
   , bold, italic, para, ttText, hr, br, pre, center
   ) where

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
      element "title" (text title)
      case css of
         Nothing -> return ()
         Just n  -> element "link" $ do
            "rel"  .=. "STYLESHEET" 
            "href" .=. n
            "type" .=. "text/css"
   element "body" body     

errorPage :: String -> HTML
errorPage s = htmlPage "Error" Nothing $ do
   h1 "Error"
   text s
   
link :: String -> HTMLBuilder -> HTMLBuilder
link url body = element "a" $ 
   ("href" .=. url) >> body

center :: HTMLBuilder -> HTMLBuilder
center = element "center"

h1 :: String -> HTMLBuilder
h1 = element "h1" . text

h2 :: String -> HTMLBuilder
h2 = element "h2" . text

h3 :: String -> HTMLBuilder
h3 = element "h3" . text

bold, italic :: HTMLBuilder -> HTMLBuilder
bold   = element "b" 
italic = element "i"

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

tt :: HTMLBuilder -> HTMLBuilder
tt = element "tt"

ttText :: String -> HTMLBuilder
ttText = tt . text

ul :: [HTMLBuilder] -> HTMLBuilder
ul = element "ul" . mapM_ (element "li")

table :: [[HTMLBuilder]] -> HTMLBuilder
table rows = element "table" $ do
   "border" .=. "1"
   mapM_ (element "tr" . mapM_ (element "td")) rows

space :: HTMLBuilder
space = XML.unescaped "&nbsp;"

image :: String -> HTMLBuilder 
image n = element "img" ("src" .=. n) 

text :: String -> HTMLBuilder
text = XML.text