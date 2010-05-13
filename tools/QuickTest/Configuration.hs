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
-----------------------------------------------------------------------------
module Configuration where

import Data.Char
import Data.List
import Data.Maybe
import System.Environment

data Configuration = Cfg
   { commandLine    :: String
   , testDirectory  :: String
   , testExtensions :: [String]
   , expExtension   :: String
   , diffTool       :: String
   , filteredWords  :: [String]
   }

readConfiguration :: IO Configuration
readConfiguration = do 
   args  <- getArgs
   let cfgFile = case args of
                    [file] -> file
                    _      -> "quicktest.cfg"
   input <- readFile cfgFile
               `catch` \_ -> return ""
   let props = mapMaybe readProperty $ map (takeWhile (/='#')) $ lines input
   return $ Cfg
      { commandLine    = fromMaybe "" (lookup "COMMAND" props)
      , testDirectory  = fromMaybe "." (lookup "DIRECTORY" props)
      , testExtensions = [ b | (key, a) <- props, key=="EXTENSION", b <- commas a ]
      , expExtension   = fromMaybe ".exp" (lookup "EXPECTED" props)
      , diffTool       = fromMaybe "diff" (lookup "DIFF" props)
      , filteredWords  = [ b | (key, a) <- props, key=="FILTER", b <- commas a ]
      }

commandText :: Configuration -> String
commandText cfg = head (words (commandLine cfg))

commandArgs :: Configuration -> String -> [String]
commandArgs cfg file = map f (tail (words (commandLine cfg)))
 where 
   f []           = []
   f ('%':'f':xs) = file ++ f xs
   f (x:xs)       = x:f xs

diffFilter :: Configuration -> String -> Bool
diffFilter cfg s = not (any (`isInfixOf` s) (filteredWords cfg))

readProperty :: String -> Maybe (String, String)
readProperty s = 
   case break (== '=') s of
      (key, _:value) -> return (map toUpper (trim key), trim value)
      _ -> Nothing

commas :: String -> [String]
commas s = 
   case break (== ',') s of
      (xs, _:ys) -> trim xs : commas ys
      _ -> [trim s]

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f