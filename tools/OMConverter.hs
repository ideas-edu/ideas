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
-----------------------------------------------------------------------------
module Main (main) where

import Graphics.UI.WX
import Domain.Math.Expr
import Data.List
import Data.Maybe
import Main.Options (versionText)
import Service.ExercisePackage (omobjToTerm, termToOMOBJ)
import Text.OpenMath.Object
import Text.XML (parseXML)

title :: String
title = "OpenMath converter: " ++ versionText

main :: IO ()
main = start $ do
   f <- frame [text := title, bgcolor := white]
   p <- panel f [bgcolor := white]
   
   -- Creating controls
   textFrom    <- textCtrl p [bgcolor := myGrey]
   textTo      <- textCtrl p [bgcolor := myGrey]
   buttonParse <- button   p [text := "parse"]
   listSymbols <- singleListBox p []
   radioFrom   <- radioBox p Vertical ["OpenMath", "Text"] [selection := 1]
   radioTo     <- radioBox p Vertical ["OpenMath", "Text", "Prefix"] []
   
   set buttonParse [ on command := do
      i   <- get radioFrom selection 
      txt <- get textFrom  text
      case fromChoice i txt of
         Left err -> 
            set textTo [color := red, text := err]
         Right expr -> do
            j <- get radioTo selection
            set textTo [color := black, text := toChoice j expr]
            set listSymbols [items := map show $ collectSymbols expr]]
   
   set p [layout := margin 10 $ grid 10 10 
            [ [ column 10 [widget radioFrom, widget buttonParse, vglue ]
              , column 0 [hstretch $ label "From:", fill $ widget textFrom]
              ]
            , [ column 0 [widget radioTo, vglue]
              , column 0 [hstretch $ label "To:", fill $ widget textTo]
              ]
            , [ vfill $ empty
              , column 0 [hstretch $ label "Symbols:", fill $ widget listSymbols] 
              ]
            ]]
   set f [layout := fill $ widget p, size := sz 600 480]

----------------------------------------------------------
-- From

fromChoice :: Int -> String -> Either String Expr
fromChoice n 
   | n == 0    = fromOpenMath 
   | otherwise = fromText -- default

fromOpenMath :: String -> Either String Expr 
fromOpenMath txt = do 
   xml   <- parseXML txt
   omobj <- xml2omobj xml
   term  <- omobjToTerm omobj
   return (toExpr term)

fromText :: String -> Either String Expr
fromText = either (Left . show) Right . parseExpr

----------------------------------------------------------
-- To

toChoice :: Int -> Expr -> String
toChoice n
   | n == 1    = toText
   | n == 2    = toPrefix
   | otherwise = toOpenMath -- default

toOpenMath :: Expr -> String
toOpenMath expr = 
   maybe (show expr) (show . omobj2xml . termToOMOBJ) (fromExpr expr)

toText :: Expr -> String
toText = show

toPrefix :: Expr -> String
toPrefix = showExpr []

----------------------------------------------------------
-- Symbols

collectSymbols :: Expr -> [Symbol]
collectSymbols = sort . nub . rec
 where
   rec expr = 
      case getFunction expr of
         Just (s, xs) -> s : concatMap rec xs
         Nothing      -> []

-- local helper functions
myGrey = rgb 230 230 230