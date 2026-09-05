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
-- A minimal interface for using W3.CSS, a modern CSS framework with built-in
-- responsiveness.
-- See https://www.w3schools.com/w3css/
--
-----------------------------------------------------------------------------
module Ideas.Text.HTML.W3CSS where

import Data.Char
import Ideas.Text.HTML
import Ideas.Text.XML
import qualified Ideas.Text.XML as XML

w3css :: HTMLPage -> HTMLPage
w3css = addCSS "https://www.w3schools.com/w3css/4/w3.css"

w3class :: BuildXML a => String -> a -> a
w3class s a = ("class" .=. s) <> a

w3classIf :: BuildXML a => Bool -> String -> a -> a
w3classIf b s = if b then w3class s else id

data Color
   = Red | Pink | Purple | DeepPurple | Indigo | Blue | LightBlue
   | Cyan | Aqua | Teal | Green | LightGreen | Lime | Sand | Khaki
   | Yellow | Amber | Orange | DeepOrange | BlueGray | Brown | LightGray
   | Gray | DarkGray | Black | PaleRed | PaleYellow | PaleGreen | PaleBlue | White
 deriving Show

data Size = Tiny | Small | Medium | Large | XL | XXL | XXXL | Jumbo
 deriving (Eq, Ord)

data Position = TopLeft    | Top    | TopRight
              | CenterLeft | Center | CenterRight
              | BottomLeft | Bottom | BottomRight
 deriving (Show, Eq)

onTop, onLeft, onRight, onBottom :: Position -> Bool
onTop    = (`elem` [TopLeft, Top, TopRight])
onLeft   = (`elem` [TopLeft, CenterLeft, BottomLeft])
onRight  = (`elem` [TopRight, CenterRight, BottomRight])
onBottom = (`elem` [BottomLeft, Bottom, BottomRight])

instance Show Size where
   show Tiny   = "tiny"
   show Small  = "small"
   show Medium = "medium"
   show Large  = "large"
   show XL     = "xlarge"
   show XXL    = "xxlarge"
   show XXXL   = "xxxlarge"
   show Jumbo  = "jumbo"

uncamel :: String -> String
uncamel = concatMap f
 where
   f c = if isUpper c then ['-', toLower c] else [c]

--------------------------------------------------------------------------------
-- * Container Classes

-- | HTML container with 16px left and right padding
container :: BuildXML a => a -> a
container = XML.tag "div" . w3class "w3-container"

-- | HTML container with 16px left and right padding and 16px top and bottom margin
panel :: BuildXML a => a -> a
panel = XML.tag "div" . w3class "w3-panel"

-- | Circular badge
badge :: BuildXML a => a -> a
badge = XML.tag "span" . w3class "w3-badge"

-- | Rectangular tag
tag :: BuildXML a => a -> a
tag = XML.tag "span" . w3class "w3-tag"

-- | Unordered list
ul :: BuildXML a => [a] -> a
ul = ulWith id

-- | Unordered list
ulWith :: BuildXML a => (a -> a) -> [a] -> a
ulWith f = XML.tag "ul" . w3class "w3-ul" . f . mconcat . map (XML.tag "li")

-- | Code container
code :: BuildXML a => a -> a
code = XML.tag "div" . w3class "w3-code"

-- | Inline code container (for code snippets)
codespan :: BuildXML a => a -> a
codespan = XML.tag "code" . w3class "w3-codespan"

--------------------------------------------------------------------------------
-- * Table Classes

-- | Container for an HTML table
table :: BuildXML a => a -> a
table = XML.tag "table" .  w3class "w3-table"

-- | All properties set
tableAll :: BuildXML a => a -> a
tableAll = XML.tag "table" . w3class "w3-table-all"

-- | Striped table
striped :: BuildXML a => a -> a
striped = w3class "w3-striped"

-- | Bordered lines
bordered :: BuildXML a => a -> a
bordered = w3class "w3-bordered"

-- | Centered table
centered :: BuildXML a => a -> a
centered = w3class "w3-centered"

-- | Hoverable table
hoverable :: BuildXML a => a -> a
hoverable = w3class "w3-hoverable"

-- | Creates a responsive table
responsive :: BuildXML a => a -> a
responsive = w3class "w3-responsive"

alignTable :: String -> [[HTMLBuilder]] -> [[HTMLBuilder]]
alignTable s = map (zipWith f (s ++ repeat 'l'))
 where
   f 'c' = center
   f 'r' = rightAlign
   f _   = id

--------------------------------------------------------------------------------
-- * Card Classes

-- | Same as w3-card-2
card :: BuildXML a => a -> a
card = w3class "w3-card"

-- | Container for any HTML content (2px bordered shadow)
card2 :: BuildXML a => a -> a
card2 = w3class "w3-card-2"

-- | Container for any HTML content (4px bordered shadow)
card4 :: BuildXML a => a -> a
card4 = w3class "w3-card-4"

--------------------------------------------------------------------------------
-- * Responsive Classes

-- | Container for one row of fluid responsive content
row :: BuildXML a => a -> a
row = w3class "w3-row"

-- | Row where all columns have a default padding
rowPadding :: BuildXML a => a -> a
rowPadding = w3class "w3-row-padding"

-- | Container for fixed size centered content
content :: BuildXML a => a -> a
content = w3class "w3-content"

-- | Half (1/2) screen column container
half :: BuildXML a => a -> a
half = w3class "w3-half"

-- | Third (1/3) screen column container
third :: BuildXML a => a -> a
third = w3class "w3-third"

-- | Two third (2/3) screen column container
twothird :: BuildXML a => a -> a
twothird = w3class "w3-twothird"

-- | Quarter (1/4) screen column container
quarter :: BuildXML a => a -> a
quarter = w3class "w3-quarter"

-- | Three quarters (3/4) screen column container
threequarter :: BuildXML a => a -> a
threequarter = w3class "w3-threequarter"

-- | Column container for any HTML content
col :: BuildXML a => a -> a
col = w3class "w3-col"

-- | Occupies the rest of the column width
rest :: BuildXML a => a -> a
rest = w3class "w3-rest"

-- | Hide content on small screens (less than 601px)
hideSmall :: BuildXML a => a -> a
hideSmall = w3class "w3-hide-small"

-- | Hide content on medium screens
hideMedium :: BuildXML a => a -> a
hideMedium = w3class "w3-hide-medium"

-- | Hide content on large screens (larger than 992px)
hideLarge :: BuildXML a => a -> a
hideLarge = w3class "w3-hide-large"

-- | Responsive image
image :: BuildXML a => a -> a
image = w3class "w3-image"

-- | Adds mobile-first responsiveness to any element. Displays elements as block elements on mobile devices.
mobile :: BuildXML a => a -> a
mobile = w3class "w3-mobile"

-- l1 - l12 Responsive sizes for large screens
-- m1 - m12 Responsive sizes for medium screens
-- s1 - s12 Responsive sizes for small screens

--------------------------------------------------------------------------------
-- * Layout Classes

-- | Container for layout columns (cells).
cellRow :: BuildXML a => a -> a
cellRow = w3class "w3-cell-row"

-- | Layout column (cell).
cell :: BuildXML a => a -> a
cell = w3class "w3-cell"

-- | Aligns content at the top of a column (cell).
cellTop :: BuildXML a => a -> a
cellTop = w3class "w3-cell-top"

-- | Aligns content at the vertical middle of a column (cell).
cellMiddle :: BuildXML a => a -> a
cellMiddle = w3class "w3-cell-middle"

-- | Aligns content at the bottom of a column (cell).
cellBottom :: BuildXML a => a -> a
cellBottom = w3class "w3-cell-bottom"

--------------------------------------------------------------------------------
-- * Bar Classes - Navigation

-- | Horizontal bar
bar :: BuildXML a => a -> a
bar = w3class "w3-bar"

-- | Vertical bar
barBlock :: BuildXML a => a -> a
barBlock = w3class "w3-bar-block"

-- | Provides common style for bar items
barItem :: BuildXML a => a -> a
barItem = w3class "w3-bar-item"

-- | Side bar
sidebar :: BuildXML a => a -> a
sidebar = w3class "w3-sidebar"

-- | Used together with w3-sidebar to create a fully automatic responsive side navigation. For this class to work, the page content must be within a "w3-main" class
collapse :: BuildXML a => a -> a
collapse = w3class "w3-collapse"

-- | Container for page content when using the w3-collapse class for responsive side navigations
mainPage :: BuildXML a => a -> a
mainPage = w3class "w3-main"

--------------------------------------------------------------------------------
-- * Dropdown Classes

-- | Clickable dropdown element
dropdownClick :: BuildXML a => a -> a
dropdownClick = w3class "w3-dropdown-click"

-- | Hoverable dropdown element
dropdownHover :: BuildXML a => a -> a
dropdownHover = w3class "w3-dropdown-hover"

--------------------------------------------------------------------------------
-- * Button Classes

-- | Rectangular button with grey background color on hover
button :: BuildXML a => String -> a -> a
button url = link url . w3class "w3-button"

-- | Rectangular button with shadows on hover
btn :: BuildXML a => String -> a -> a
btn url = link url . w3class "w3-btn"

-- | Rectangular button with ripple effect
ripple :: BuildXML a => String -> a -> a
ripple url = link url . w3class "w3-ripple"

--------------------------------------------------------------------------------
-- * Input Classes

-- | Input elements
input :: BuildXML a => a -> a
input = w3class "w3-input"

-- | Checkbox input type
check :: BuildXML a => a -> a
check = w3class "w3-check"

-- | Radio input type
radio :: BuildXML a => a -> a
radio = w3class "w3-radio"

-- | Input select element
select :: BuildXML a => a -> a
select = w3class "w3-select"

--------------------------------------------------------------------------------
-- * Modal Classes

-- | Modal container
modal :: BuildXML a => a -> a
modal = w3class "w3-modal"

-- | Modal pop-up element
modalContent :: BuildXML a => a -> a
modalContent = w3class "w3-modal-content"

-- | Tooltip element
tooltip :: BuildXML a => a -> a
tooltip = w3class "w3-tooltip"

-- | Tooltip text
tooltipText :: BuildXML a => a -> a
tooltipText = w3class "w3-text"

--------------------------------------------------------------------------------
-- * Animation Classes

-- | Animates an element from -300px to 0px
animate :: BuildXML a => Position -> a -> a
animate p = w3classIf (onTop p)    "w3-animate-top"
          . w3classIf (onLeft p)   "w3-animate-left"
          . w3classIf (onBottom p) "w3-animate-bottom"
          . w3classIf (onRight p)  "w3-animate-right"

-- | Animates an element's opacity from 0 to 1
animateOpacity :: BuildXML a => a -> a
animateOpacity = w3class "w3-animate-opacity"

-- | Animates an element from 0 to 100% in size
animateZoom :: BuildXML a => a -> a
animateZoom = w3class "w3-animate-zoom"

-- | Animates an element's opacity from 0 to 1 and 1 to 0 (fades in AND out)
animateFading :: BuildXML a => a -> a
animateFading = w3class "w3-animate-fading"

-- | Spin an icon 360 degrees
spin :: BuildXML a => a -> a
spin = w3class "w3-spin"

-- | Animates the width of an input field to 100%
animateInput :: BuildXML a => a -> a
animateInput = w3class "w3-animate-input"

--------------------------------------------------------------------------------
-- * Font and Text Classes

-- | Specifies a font size: tiny 10px, small 12px, large 18px, xlarge 24px, xxlarge 32px, xxxlarge 48px, jumbo 64px
fontSize :: BuildXML a => Size -> a -> a
fontSize = w3class . ("w3-" ++) . show

-- | Specifies a wider text
wide :: BuildXML a => a -> a
wide = w3class "w3-wide"

-- | Changes the font to serif
serif :: BuildXML a => a -> a
serif = w3class "w3-serif"

--------------------------------------------------------------------------------
-- * Display Classes

-- | Centered content
center :: BuildXML a => a -> a
center = w3class "w3-center"

-- | Floats an element to the left (float: left)
left :: BuildXML a => a -> a
left = w3class "w3-left"

-- | Floats an element to the right (float: right)
right :: BuildXML a => a -> a
right = w3class "w3-right"

-- | Left aligned text
leftAlign :: BuildXML a => a -> a
leftAlign = w3class "w3-left-align"

-- | Right aligned text
rightAlign :: BuildXML a => a -> a
rightAlign = w3class "w3-right-align"

-- | Right and left aligned text
justify :: BuildXML a => a -> a
justify = w3class "w3-justify"

-- | Circled content
circle :: BuildXML a => a -> a
circle = w3class "w3-circle"

-- | Hidden content (display:none)
hide :: BuildXML a => a -> a
hide = w3class "w3-hide"

-- | Alias of w3-show (display:block)
showBlock :: BuildXML a => a -> a
showBlock = w3class "w3-show-block"

-- | Show content as inline-block (display:inline-block)
showInlineBlock :: BuildXML a => a -> a
showInlineBlock = w3class "w3-show-inline-block"

-- | Fixed content at the top of a page
top :: BuildXML a => a -> a
top = w3class "w3-top"

-- | Fixed content at the bottom of a page
bottom :: BuildXML a => a -> a
bottom = w3class "w3-bottom"

-- | Container for w3-display-classes (position: relative)
display :: BuildXML a => Position -> a -> a
display p = w3class "w3-display-container" . w3class (f p)
 where
   f TopLeft     = "w3-display-topleft"
   f Top         = "w3-display-topmiddle"
   f TopRight    = "w3-display-topright"
   f CenterLeft  = "w3-display-left"
   f Center      = "w3-display-middle"
   f CenterRight = "w3-display-right"
   f BottomLeft  = "w3-display-bottomleft"
   f Bottom      = "w3-display-bottommiddle"
   f BottomRight = "w3-display-bottomright"

-- | Displays content on hover inside the w3-display-container
displayHover :: BuildXML a => a -> a
displayHover = w3class "w3-display-hover"

--------------------------------------------------------------------------------
-- * Effect Classes

-- | Adds opacity/transparency to an element (opacity: 0.6)
opacity :: BuildXML a => a -> a
opacity = w3class "w3-opacity"

-- | Turns off opacity/transparency (opacity: 1)
opacityOff :: BuildXML a => a -> a
opacityOff = w3class "w3-opacity-off"

-- | Adds opacity/transparency to an element (opacity: 0.75)
opacityMin :: BuildXML a => a -> a
opacityMin = w3class "w3-opacity-min"

-- | Adds opacity/transparency to an element (opacity: 0.25)
opacityMax :: BuildXML a => a -> a
opacityMax = w3class "w3-opacity-max"

-- | Adds a grayscale effect to an element (grayscale: 50%)
grayscaleMin :: BuildXML a => a -> a
grayscaleMin = w3class "w3-grayscale-min"

-- | Adds a grayscale effect to an element (grayscale: 75%)
grayscale :: BuildXML a => a -> a
grayscale = w3class "w3-grayscale"

-- | Adds a grayscale effect to an element (grayscale: 100%)
grayscaleMax :: BuildXML a => a -> a
grayscaleMax = w3class "w3-grayscale-max"

-- | Adds a sepia effect to an element (sepia: 50%)
sepiaMin :: BuildXML a => a -> a
sepiaMin = w3class "w3-sepia-min"

-- | Adds a sepia effect to an element (sepia: 75%)
sepia :: BuildXML a => a -> a
sepia = w3class "w3-sepia"

-- | Adds a sepia effect to an element (sepia: 100%)
sepiaMax :: BuildXML a => a -> a
sepiaMax = w3class "w3-sepia-max"

-- | Creates an overlay effect
overlay :: BuildXML a => a -> a
overlay = w3class "w3-overlay"

--------------------------------------------------------------------------------
-- * Background Color Classes

-- | Background color
background :: BuildXML a => Color -> a -> a
background = w3class . ("w3" ++) .  uncamel . show

-- | Transparent background-color
transparent :: BuildXML a => a -> a
transparent = w3class "w3-transparent"

--------------------------------------------------------------------------------
--  * Color Classes

-- | Hover color
hover :: BuildXML a => Color -> a -> a
hover = w3class . ("w3-hover" ++) .  uncamel . show

--------------------------------------------------------------------------------
-- * Text Color Classes

-- | Text color
textColor :: BuildXML a => Color -> a -> a
textColor = w3class . ("w3-text" ++) .  uncamel . show

--------------------------------------------------------------------------------
-- * Hover Classes

-- | Hover text color
hoverColor :: BuildXML a => Color -> a -> a
hoverColor = w3class . ("w3-hover-text" ++) .  uncamel . show

-- | Adds transparency to an element on hover (opacity: 0.6)
hoverOpacity :: BuildXML a => a -> a
hoverOpacity = w3class "w3-hover-opacity"

-- | Removes transparency from an element on hover (100% opacity)
hoverOpacityOff :: BuildXML a => a -> a
hoverOpacityOff = w3class "w3-hover-opacity-off"

-- | Adds shadow to an element on hover
hoverShadow :: BuildXML a => a -> a
hoverShadow = w3class "w3-hover-shadow"

-- | Adds a black and white (100% grayscale) effect to an element
hoverGrayscale :: BuildXML a => a -> a
hoverGrayscale = w3class "w3-hover-grayscale"

-- | Adds a sepia effect to an element on hover
hoverSepia :: BuildXML a => a -> a
hoverSepia = w3class "w3-hover-sepia"

-- | Removes hover effects from an element
hoverNone :: BuildXML a => a -> a
hoverNone = w3class "w3-hover-none"

--------------------------------------------------------------------------------
-- * Round Classes

-- | Element rounded (border-radius): small 2px, medium 4px, large 8px, xlarge 16px, xxlarge 32px
rounded :: BuildXML a => Size -> a -> a
rounded s = w3class "w3-round" . w3class ("w3-round-" ++ show s)

--------------------------------------------------------------------------------
-- * Padding Classes

-- | Small: Padding 4px top and bottom, and 8px left and right, Medium: Padding 8px top and bottom, and 16px left and right, Large: Padding 12px top and bottom, and 24px left and right.
padding :: BuildXML a => Size -> a -> a
padding s
   | s < Medium = w3class "w3-padding-small"
   | s > Medium = w3class "w3-padding-large"
   | otherwise  = w3class "w3-padding-small"

-- | Padding top and bottom: medium 16px, large 24px, xlarge 32px, xxlarge 48px, xxxlarge 64px
vpadding :: BuildXML a => Size -> a -> a
vpadding s
   | s <= Medium = w3class "w3-padding-16"
   | s == Large  = w3class "w3-padding-24"
   | s == XL     = w3class "w3-padding-32"
   | s == XXL    = w3class "w3-padding-48"
   | otherwise   = w3class "w3-padding-64"

--------------------------------------------------------------------------------
-- * Margin Classes

-- | Adds an 16px margin to an element
margin :: BuildXML a => a -> a
margin = w3class "w3-margin"

marginPos :: BuildXML a => Position -> a -> a
marginPos p = w3classIf (onTop p)    "w3-margin-top"
            . w3classIf (onLeft p)   "w3-margin-left"
            . w3classIf (onBottom p) "w3-margin-bottom"
            . w3classIf (onRight p)  "w3-margin-right"

-- | Adds an 16px top and bottom margin to an element
section :: BuildXML a => a -> a
section = w3class "w3-section"

--------------------------------------------------------------------------------
-- * Border Classes

-- | Borders (top, right, bottom, left)
border :: BuildXML a => a -> a
border = w3class "w3-border"

borderPos :: BuildXML a => Position -> a -> a
borderPos p = w3classIf (onTop p)    "w3-border-top"
            . w3classIf (onLeft p)   "w3-border-left"
            . w3classIf (onBottom p) "w3-border-bottom"
            . w3classIf (onRight p)  "w3-border-right"

-- | Removes all borders
noBorder :: BuildXML a => a -> a
noBorder = w3class "w3-border-0"

-- | Border color
borderColor :: BuildXML a => Color -> a -> a
borderColor = w3class . ("w3-border" ++) .  uncamel . show

-- | Adds a thick border (bar) to an element
barPos :: BuildXML a => Position -> a -> a
barPos p = w3classIf (onTop p)    "w3-topbar"
         . w3classIf (onLeft p)   "w3-leftbar"
         . w3classIf (onBottom p) "w3-bottombar"
         . w3classIf (onRight p)  "w3-rightbar"

--------------------------------------------------------------------------------
-- * Color themes

data ColorTheme
   = L1 | L2 | L3 | L4 | L5 -- light
   | D1 | D2 | D3 | D4 | D5 -- dark
 deriving Show

-- standard color theme
theme_, textTheme, borderTheme :: BuildXML a => a -> a
theme_      = w3class "w3-theme"
textTheme   = w3class "w3-text-theme"
borderTheme = w3class "w3-border-theme"

theme :: BuildXML a => ColorTheme -> a -> a
theme = w3class . ("w3-theme-" ++) . map toLower . show