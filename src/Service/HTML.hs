-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
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
module Service.HTML 
   ( HTML, HTMLBuilder, showHTML
   , htmlPage, errorPage, link, h1, h2, preText, ul, table, text
   , bold, italic, para, ttText, hr, br, pre
   ) where

import Service.XML hiding (text)
import qualified Service.XML as XML

type HTML = XML

type HTMLBuilder = XMLBuilder

showHTML :: HTML -> String
showHTML = compactXML

-- html helper functions
htmlPage :: String -> XMLBuilder -> HTML
htmlPage title body = makeXML "html" $ do
   element "head" $
      element "title" (text title)
   element "body" body     

errorPage :: String -> HTML
errorPage s = htmlPage "Error" $ do
   h1 "Error"
   text s
   
link :: String -> XMLBuilder -> XMLBuilder
link url body = element "a" $ 
   ("href" .=. url) >> body

h1 :: String -> XMLBuilder
h1 = element "h1" . text

h2 :: String -> XMLBuilder
h2 = element "h2" . text

bold, italic :: XMLBuilder -> XMLBuilder
bold   = element "b" 
italic = element "i"

para :: XMLBuilder -> XMLBuilder
para = element "p"

preText :: String -> XMLBuilder
preText = pre . text

pre :: XMLBuilder -> XMLBuilder
pre = element "pre"

hr :: XMLBuilder
hr = tag "hr"

br :: XMLBuilder
br = tag "br"

tt :: XMLBuilder -> XMLBuilder
tt = element "tt"

ttText :: String -> XMLBuilder
ttText = tt . text

ul :: [XMLBuilder] -> XMLBuilder
ul = element "ul" . mapM_ (element "li")

table :: [[XMLBuilder]] -> XMLBuilder
table rows = element "table" $ do
   "border" .=. "1"
   mapM_ (element "tr" . mapM_ (element "td")) rows

text :: String -> XMLBuilder
text = XML.text . escape

escape :: String -> String
escape = concatMap f 
 where
   f '<'  = "&lt;"
   f '>'  = "&gt;"
   f '\n' = "<br>"
   f c   = [c] 