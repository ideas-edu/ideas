module Ideas.Text.HTML.W3CSS where

import Data.Char
import Ideas.Text.XML
import Ideas.Text.HTML
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
 
container :: BuildXML a => a -> a
container = XML.tag "div" . w3class "w3-container" -- ^ HTML container with 16px left and right padding  

panel :: BuildXML a => a -> a
panel = XML.tag "div" . w3class "w3-panel" -- ^ HTML container with 16px left and right padding and 16px top and bottom margin 

badge :: BuildXML a => a -> a
badge = XML.tag "span" . w3class "w3-badge" -- ^ Circular badge 

tag :: BuildXML a => a -> a
tag = XML.tag "span" . w3class "w3-tag" -- ^ Rectangular tag 

ul :: BuildXML a => [a] -> a
ul = ulWith id -- ^ Unordered list 

ulWith :: BuildXML a => (a -> a) -> [a] -> a
ulWith f = XML.tag "ul" . w3class "w3-ul" . f . mconcat . map (XML.tag "li") -- ^ Unordered list 

code :: BuildXML a => a -> a
code = XML.tag "div" . w3class "w3-code" -- ^ Code container 

codespan :: BuildXML a => a -> a
codespan = XML.tag "code" . w3class "w3-codespan" -- ^ Inline code container (for code snippets) 

--------------------------------------------------------------------------------
-- * Table Classes

table :: BuildXML a => a -> a
table = XML.tag "table" .  w3class "w3-table" -- ^ Container for an HTML table 

tableAll :: BuildXML a => a -> a
tableAll = XML.tag "table" . w3class "w3-table-all" -- ^ All properties set 

striped :: BuildXML a => a -> a
striped = w3class "w3-striped" -- ^ Striped table 

bordered :: BuildXML a => a -> a
bordered = w3class "w3-bordered" -- ^ Bordered lines 

centered :: BuildXML a => a -> a
centered = w3class "w3-centered" -- ^ Centered table 

hoverable :: BuildXML a => a -> a
hoverable = w3class "w3-hoverable" -- ^ Hoverable table 

responsive :: BuildXML a => a -> a
responsive = w3class "w3-responsive" -- ^ Creates a responsive table 

--------------------------------------------------------------------------------
-- * Card Classes

card :: BuildXML a => a -> a
card = w3class "w3-card" -- ^ Same as w3-card-2 

card2 :: BuildXML a => a -> a
card2 = w3class "w3-card-2" -- ^ Container for any HTML content (2px bordered shadow) 

card4 :: BuildXML a => a -> a
card4 = w3class "w3-card-4" -- ^ Container for any HTML content (4px bordered shadow) 

--------------------------------------------------------------------------------
-- * Responsive Classes

row :: BuildXML a => a -> a
row = w3class "w3-row" -- ^ Container for one row of fluid responsive content 

rowPadding :: BuildXML a => a -> a
rowPadding = w3class "w3-row-padding" -- ^ Row where all columns have a default padding 

content :: BuildXML a => a -> a
content = w3class "w3-content" -- ^ Container for fixed size centered content 

half :: BuildXML a => a -> a
half = w3class "w3-half" -- ^ Half (1/2) screen column container 

third :: BuildXML a => a -> a
third = w3class "w3-third" -- ^ Third (1/3) screen column container 

twothird :: BuildXML a => a -> a
twothird = w3class "w3-twothird" -- ^ Two third (2/3) screen column container 

quarter :: BuildXML a => a -> a
quarter = w3class "w3-quarter" -- ^ Quarter (1/4) screen column container 

threequarter :: BuildXML a => a -> a
threequarter = w3class "w3-threequarter" -- ^ Three quarters (3/4) screen column container 

col :: BuildXML a => a -> a
col = w3class "w3-col" -- ^ Column container for any HTML content 

rest :: BuildXML a => a -> a
rest = w3class "w3-rest" -- ^ Occupies the rest of the column width 

hideSmall :: BuildXML a => a -> a
hideSmall = w3class "w3-hide-small" -- ^ Hide content on small screens (less than 601px) 

hideMedium :: BuildXML a => a -> a
hideMedium = w3class "w3-hide-medium" -- ^ Hide content on medium screens 

hideLarge :: BuildXML a => a -> a
hideLarge = w3class "w3-hide-large" -- ^ Hide content on large screens (larger than 992px)     

image :: BuildXML a => a -> a
image = w3class "w3-image" -- ^ Responsive image 

mobile :: BuildXML a => a -> a
mobile = w3class "w3-mobile" -- ^ Adds mobile-first responsiveness to any element. Displays elements as block elements on mobile devices. 


-- l1 - l12 Responsive sizes for large screens 
-- m1 - m12 Responsive sizes for medium screens 
-- s1 - s12 Responsive sizes for small screens 

--------------------------------------------------------------------------------
-- * Layout Classes

cellRow :: BuildXML a => a -> a
cellRow = w3class "w3-cell-row" -- ^ Container for layout columns (cells). 

cell :: BuildXML a => a -> a
cell = w3class "w3-cell" -- ^ Layout column (cell). 

cellTop :: BuildXML a => a -> a
cellTop = w3class "w3-cell-top" -- ^ Aligns content at the top of a column (cell). 

cellMiddle :: BuildXML a => a -> a
cellMiddle = w3class "w3-cell-middle" -- ^ Aligns content at the vertical middle of a column (cell). 

cellBottom :: BuildXML a => a -> a
cellBottom = w3class "w3-cell-bottom" -- ^ Aligns content at the bottom of a column (cell). 

--------------------------------------------------------------------------------
-- * Bar Classes - Navigation

bar :: BuildXML a => a -> a
bar = w3class "w3-bar" -- ^ Horizontal bar 

barBlock :: BuildXML a => a -> a
barBlock = w3class "w3-bar-block" -- ^ Vertical bar 

barItem :: BuildXML a => a -> a
barItem = w3class "w3-bar-item" -- ^ Provides common style for bar items 

sidebar :: BuildXML a => a -> a
sidebar = w3class "w3-sidebar" -- ^ Side bar 

collapse :: BuildXML a => a -> a
collapse = w3class "w3-collapse" -- ^ Used together with w3-sidebar to create a fully automatic responsive side navigation. For this class to work, the page content must be within a "w3-main" class 

mainPage :: BuildXML a => a -> a
mainPage = w3class "w3-main" -- ^ Container for page content when using the w3-collapse class for responsive side navigations 

--------------------------------------------------------------------------------
-- * Dropdown Classes

dropdownClick :: BuildXML a => a -> a
dropdownClick = w3class "w3-dropdown-click" -- ^ Clickable dropdown element 

dropdownHover :: BuildXML a => a -> a
dropdownHover = w3class "w3-dropdown-hover" -- ^ Hoverable dropdown element 

--------------------------------------------------------------------------------
-- * Button Classes

button :: BuildXML a => String -> a -> a
button url = link url . w3class "w3-button" -- ^ Rectangular button with grey background color on hover 

btn :: BuildXML a => String -> a -> a
btn url = link url . w3class "w3-btn" -- ^ Rectangular button with shadows on hover 

ripple :: BuildXML a => String -> a -> a
ripple url = link url . w3class "w3-ripple" -- ^ Rectangular button with ripple effect 

--------------------------------------------------------------------------------
-- * Input Classes

input :: BuildXML a => a -> a
input = w3class "w3-input" -- ^ Input elements 

check :: BuildXML a => a -> a
check = w3class "w3-check" -- ^ Checkbox input type 

radio :: BuildXML a => a -> a
radio = w3class "w3-radio" -- ^ Radio input type 

select :: BuildXML a => a -> a
select = w3class "w3-select" -- ^ Input select element 

--------------------------------------------------------------------------------
-- * Modal Classes

modal :: BuildXML a => a -> a
modal = w3class "w3-modal" -- ^ Modal container 

modalContent :: BuildXML a => a -> a
modalContent = w3class "w3-modal-content" -- ^ Modal pop-up element 

tooltip :: BuildXML a => a -> a
tooltip = w3class "w3-tooltip" -- ^ Tooltip element 

tooltipText :: BuildXML a => a -> a
tooltipText = w3class "w3-text" -- ^ Tooltip text

--------------------------------------------------------------------------------
-- * Animation Classes

-- | -- ^ Animates an element from -300px to 0px 
animate :: BuildXML a => Position -> a -> a
animate p = w3classIf (onTop p)    "w3-animate-top"
          . w3classIf (onLeft p)   "w3-animate-left"
          . w3classIf (onBottom p) "w3-animate-bottom"
          . w3classIf (onRight p)  "w3-animate-right"

animateOpacity :: BuildXML a => a -> a
animateOpacity = w3class "w3-animate-opacity" -- ^ Animates an element's opacity from 0 to 1 

animateZoom :: BuildXML a => a -> a
animateZoom = w3class "w3-animate-zoom" -- ^ Animates an element from 0 to 100% in size 

animateFading :: BuildXML a => a -> a
animateFading = w3class "w3-animate-fading" -- ^ Animates an element's opacity from 0 to 1 and 1 to 0 (fades in AND out) 

spin :: BuildXML a => a -> a
spin = w3class "w3-spin" -- ^ Spin an icon 360 degrees 

animateInput :: BuildXML a => a -> a
animateInput = w3class "w3-animate-input" -- ^ Animates the width of an input field to 100% 

--------------------------------------------------------------------------------
-- * Font and Text Classes

-- | Specifies a font size: tiny 10px, small 12px, large 18px, xlarge 24px, xxlarge 32px, xxxlarge 48px, jumbo 64px
fontSize :: BuildXML a => Size -> a -> a
fontSize = w3class . ("w3-" ++) . show

wide :: BuildXML a => a -> a
wide = w3class "w3-wide" -- ^ Specifies a wider text 

serif :: BuildXML a => a -> a
serif = w3class "w3-serif" -- ^ Changes the font to serif 

--------------------------------------------------------------------------------
-- * Display Classes

center :: BuildXML a => a -> a
center = w3class "w3-center" -- ^ Centered content 

left :: BuildXML a => a -> a
left = w3class "w3-left" -- ^ Floats an element to the left (float: left) 

right :: BuildXML a => a -> a
right = w3class "w3-right" -- ^ Floats an element to the right (float: right) 

leftAlign :: BuildXML a => a -> a
leftAlign = w3class "w3-left-align" -- ^ Left aligned text 

rightAlign :: BuildXML a => a -> a
rightAlign = w3class "w3-right-align" -- ^ Right aligned text 

justify :: BuildXML a => a -> a
justify = w3class "w3-justify" -- ^ Right and left aligned text 

circle :: BuildXML a => a -> a
circle = w3class "w3-circle" -- ^ Circled content 

hide :: BuildXML a => a -> a
hide = w3class "w3-hide" -- ^ Hidden content (display:none) 

showBlock :: BuildXML a => a -> a
showBlock = w3class "w3-show-block" -- ^ Alias of w3-show (display:block) 

showInlineBlock :: BuildXML a => a -> a
showInlineBlock = w3class "w3-show-inline-block" -- ^ Show content as inline-block (display:inline-block) 

top :: BuildXML a => a -> a
top = w3class "w3-top" -- ^ Fixed content at the top of a page 

bottom :: BuildXML a => a -> a
bottom = w3class "w3-bottom" -- ^ Fixed content at the bottom of a page 

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

displayHover :: BuildXML a => a -> a
displayHover = w3class "w3-display-hover" -- ^ Displays content on hover inside the w3-display-container 

--------------------------------------------------------------------------------
-- * Effect Classes

opacity :: BuildXML a => a -> a
opacity = w3class "w3-opacity" -- ^ Adds opacity/transparency to an element (opacity: 0.6) 

opacityOff :: BuildXML a => a -> a
opacityOff = w3class "w3-opacity-off" -- ^ Turns off opacity/transparency (opacity: 1) 

opacityMin :: BuildXML a => a -> a
opacityMin = w3class "w3-opacity-min" -- ^ Adds opacity/transparency to an element (opacity: 0.75) 

opacityMax :: BuildXML a => a -> a
opacityMax = w3class "w3-opacity-max" -- ^ Adds opacity/transparency to an element (opacity: 0.25) 

grayscaleMin :: BuildXML a => a -> a
grayscaleMin = w3class "w3-grayscale-min" -- ^ Adds a grayscale effect to an element (grayscale: 50%) 

grayscale :: BuildXML a => a -> a
grayscale = w3class "w3-grayscale" -- ^ Adds a grayscale effect to an element (grayscale: 75%) 

grayscaleMax :: BuildXML a => a -> a
grayscaleMax = w3class "w3-grayscale-max" -- ^ Adds a grayscale effect to an element (grayscale: 100%) 

sepiaMin :: BuildXML a => a -> a
sepiaMin = w3class "w3-sepia-min" -- ^ Adds a sepia effect to an element (sepia: 50%) 

sepia :: BuildXML a => a -> a
sepia = w3class "w3-sepia" -- ^ Adds a sepia effect to an element (sepia: 75%) 

sepiaMax :: BuildXML a => a -> a
sepiaMax = w3class "w3-sepia-max" -- ^ Adds a sepia effect to an element (sepia: 100%) 

overlay :: BuildXML a => a -> a
overlay = w3class "w3-overlay" -- ^ Creates an overlay effect 

--------------------------------------------------------------------------------
-- * Background Color Classes

background :: BuildXML a => Color -> a -> a
background = w3class . ("w3" ++) .  uncamel . show -- ^ Background color

transparent :: BuildXML a => a -> a
transparent = w3class "w3-transparent" -- ^ Transparent background-color  

--------------------------------------------------------------------------------
--  * Color Classes

hover :: BuildXML a => Color -> a -> a
hover = w3class . ("w3-hover" ++) .  uncamel . show -- ^ Hover color

--------------------------------------------------------------------------------
-- * Text Color Classes

textColor :: BuildXML a => Color -> a -> a
textColor = w3class . ("w3-text" ++) .  uncamel . show -- ^ Text color

--------------------------------------------------------------------------------
-- * Hover Classes

hoverColor :: BuildXML a => Color -> a -> a
hoverColor = w3class . ("w3-hover-text" ++) .  uncamel . show -- ^ Hover text color

hoverOpacity :: BuildXML a => a -> a
hoverOpacity = w3class "w3-hover-opacity" -- ^ Adds transparency to an element on hover (opacity: 0.6) 

hoverOpacityOff :: BuildXML a => a -> a
hoverOpacityOff = w3class "w3-hover-opacity-off" -- ^ Removes transparency from an element on hover (100% opacity) 

hoverShadow :: BuildXML a => a -> a
hoverShadow = w3class "w3-hover-shadow" -- ^ Adds shadow to an element on hover 

hoverGrayscale :: BuildXML a => a -> a
hoverGrayscale = w3class "w3-hover-grayscale" -- ^ Adds a black and white (100% grayscale) effect to an element 

hoverSepia :: BuildXML a => a -> a
hoverSepia = w3class "w3-hover-sepia" -- ^ Adds a sepia effect to an element on hover 

hoverNone :: BuildXML a => a -> a
hoverNone = w3class "w3-hover-none" -- ^ Removes hover effects from an element

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

margin :: BuildXML a => a -> a
margin = w3class "w3-margin" -- ^ Adds an 16px margin to an element 

marginPos :: BuildXML a => Position -> a -> a
marginPos p = w3classIf (onTop p)    "w3-margin-top"
            . w3classIf (onLeft p)   "w3-margin-left"
            . w3classIf (onBottom p) "w3-margin-bottom"
            . w3classIf (onRight p)  "w3-margin-right"

section :: BuildXML a => a -> a
section = w3class "w3-section" -- ^ Adds an 16px top and bottom margin to an element 

--------------------------------------------------------------------------------
-- * Border Classes

border :: BuildXML a => a -> a
border = w3class "w3-border" -- ^ Borders (top, right, bottom, left) 

borderPos :: BuildXML a => Position -> a -> a
borderPos p = w3classIf (onTop p)    "w3-border-top"
            . w3classIf (onLeft p)   "w3-border-left"
            . w3classIf (onBottom p) "w3-border-bottom"
            . w3classIf (onRight p)  "w3-border-right"

noBorder :: BuildXML a => a -> a
noBorder = w3class "w3-border-0" -- ^ Removes all borders 

borderColor :: BuildXML a => Color -> a -> a
borderColor = w3class . ("w3-border" ++) .  uncamel . show -- ^ Border color

-- | Adds a thick border (bar) to an element 
barPos :: BuildXML a => Position -> a -> a
barPos p = w3classIf (onTop p)    "w3-topbar"
         . w3classIf (onLeft p)   "w3-leftbar"
         . w3classIf (onBottom p) "w3-bottombar"
         . w3classIf (onRight p)  "w3-rightbar"