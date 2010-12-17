-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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
   , htmlPage, errorPage, link, linkTitle
   , h1, h2, h3, h4, preText, ul, table
   , text, image, space, tt, spaces, highlightXML
   , font, bold, italic, para, ttText, hr, br, pre, center, bullet, divClass
   ) where

import Text.XML hiding (text)
import qualified Text.XML as XML
import Control.Monad
import Data.Char
import Data.List

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

linkTitle :: String -> String -> HTMLBuilder -> HTMLBuilder
linkTitle url title body = element "a" $ 
   ("href" .=. url) >> ("title" .=. title) >> body

center :: HTMLBuilder -> HTMLBuilder
center = element "center"

h1 :: String -> HTMLBuilder
h1 = element "h1" . text

h2 :: String -> HTMLBuilder
h2 = element "h2" . text

h3 :: String -> HTMLBuilder
h3 = element "h3" . text

h4 :: String -> HTMLBuilder
h4 = element "h4" . text

font :: String -> HTMLBuilder -> HTMLBuilder
font n = element "font" . ("class" .=. n >>)

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
   forM_ (zip [0::Int ..] rows) $ \(i, r) ->
      element "tr" $ do
         "class" .=. getClass i
         mapM_ (element "td") r
 where
   getClass i
      | i == 0    = "topRow"
      | even i    = "evenRow"
      | otherwise = "oddRow" 

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
divClass n body = element "div" ("class" .=. n >> body)

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