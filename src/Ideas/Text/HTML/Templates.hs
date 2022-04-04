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
-- W3CSS templates for html pages
--
-----------------------------------------------------------------------------

module Ideas.Text.HTML.Templates
   ( webpage, WebPage(..), Button(..), emptyWebPage, emptyButton, Style
   , fontAwesome
   ) where

import Ideas.Text.HTML
import Ideas.Text.HTML.W3CSS hiding (tag, ul, top, table, content)
import Ideas.Text.XML
import qualified Ideas.Text.HTML.W3CSS as W3

data WebPage = WebPage
   { title          :: String
   , menuButtons    :: [Button]
   , menuStyle      :: Style
   , iconBarsStyle  :: Style
   , sideWidth      :: Int
   , sideHeader     :: HTMLBuilder
   , sideButtons    :: [Button]
   , sideStyle      :: Style
   , iconCloseStyle :: Style
   , content        :: HTMLBuilder
   , footer         :: HTMLBuilder
   , footerStyle    :: Style
   }

emptyWebPage :: WebPage
emptyWebPage = WebPage
   { title          = "title"
   , menuButtons    = []
   , menuStyle      = id
   , iconBarsStyle  = id
   , sideWidth      = 600
   , sideHeader     = mempty
   , sideButtons    = []
   , sideStyle      = id
   , iconCloseStyle = id
   , content        = mempty
   , footer         = mempty
   , footerStyle    = id
   }

type Style = HTMLBuilder -> HTMLBuilder

data Button = Button
   { buttonUrl   :: String
   , buttonStyle :: HTMLBuilder -> HTMLBuilder
   , buttonText  :: HTMLBuilder
   }

emptyButton :: Button
emptyButton = Button "" id mempty

fromButtons :: [Button] -> HTMLBuilder
fromButtons = mconcat . map fromButton

fromButton :: Button -> HTMLBuilder
fromButton b = button (buttonUrl b) $ barItem $ buttonStyle b $ buttonText b

fontAwesome :: BuildXML a => String -> a
fontAwesome s = italic $ ("class" .=. ("fa fa-" ++ s)) <> string " "

-- https://www.w3schools.com/w3css/tryw3css_templates_webpage.htm
webpage :: WebPage -> HTMLPage
webpage wp = w3css $
   addCSS "https://fonts.googleapis.com/css?family=Roboto" $
   addCSS "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" $
   addStyle styleText $
   htmlPage (title wp) $
      navBar <> sideBar <> overlayEffect <> mainContent <> script
 where
   navBar = tag "div" $ W3.top $
      tag "div" $ bar $ W3.top $ leftAlign $ menuStyle wp $ mconcat
         [ if null (sideButtons wp) then mempty else
           button "javascript:void(0)" $ barItem $ right $ hideLarge $ iconBarsStyle wp $
              ("onclick" .=. "w3_open()") <> fontAwesome "bars"
         , fromButtons (menuButtons wp)
         ]

   sideBar = if null (sideButtons wp) then mempty else
      tag "nav" $ sidebar $ barBlock $ collapse $ sideStyle wp $ mconcat
         [ "id" .=. "mySidebar"
         , link "javascript:void(0)" $ right $ padding Large $ iconCloseStyle wp $ hideLarge $ mconcat
              [ "onclick" .=. "w3_close()"
              , "title"   .=. "Close Menu"
              , fontAwesome "remove"
              ]
         , sideHeader wp
         , fromButtons (sideButtons wp)
         ]

   overlayEffect = tag "div" $ overlay $ hideLarge $ mconcat
      [ "onclick" .=. "w3_close()"
      , "style"   .=. "cursor:pointer"
      , "title"   .=. "close side menu"
      , "id"      .=. "myOverlay"
      , " "
      ]

   -- the javascript is not to be escaped
   script = tag "script" (string scriptText)

   width | null (sideButtons wp) = "0px"
         | otherwise             = show (sideWidth wp) ++ "px"

   mainContent = tag "div" $ mainPage $ mconcat
      [ "style" .=. ("margin-left:" ++ width)
      , container $ vpadding XXXL $ content wp
      , tag "footer" $ mconcat
           [ "id" .=. "myFooter"
           , container $ footerStyle wp $ para $ footer wp
           ]
      ]

   styleText =
      "html,body,h1,h2,h3,h4,h5,h6 {font-family: Roboto, sans-serif;}\
      \.w3-sidebar {\
      \  z-index: 3;\
      \  width: " ++ width ++ ";\
      \  top: 43px;\
      \  bottom: 0;\
      \  height: inherit;\
      \}"

scriptText :: String
scriptText =
   "var mySidebar = document.getElementById(\"mySidebar\");\
   \var overlayBg = document.getElementById(\"myOverlay\");\
   \function w3_open() {\
   \    if (mySidebar.style.display === 'block') {\
   \        mySidebar.style.display = 'none';\
   \        overlayBg.style.display = 'none';\
   \    } else {\
   \        mySidebar.style.display = 'block';\
   \        overlayBg.style.display = 'block';\
   \    }\
   \}\
   \function w3_close() {\
   \    mySidebar.style.display = 'none';\
   \    overlayBg.style.display = 'none';\
   \}"