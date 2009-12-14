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
module Configuration 
   ( Config, readConfig, writeConfig
   , ConfigEntry, fromConfig, toConfig
   , code
   ) where

import qualified Data.Map as M
import Common.Exercise (ExerciseCode, readCode, makeCode)
import Common.Utils
import Data.Maybe

-----------------------------------------------------------------------------
-- Configuration file

configFile :: String
configFile = "ideasWX.cfg"

header :: String
header = "# IdeasWX configuration file"

newtype Config = C (M.Map String String)

instance Show Config where
   show (C m) = 
      let f (k, a) = k ++ " = " ++ a
      in unlines $ (header:) $ map f $ M.toList m
      
readConfig :: IO Config
readConfig = do 
   xs <- readFile configFile
   let ps = mapMaybe (declaration . stripComment) (lines xs)
   return $ C $ M.fromList ps
 `catch` 
   \_ -> return $ C $ M.empty

writeConfig :: Config -> IO ()
writeConfig cfg =
   writeFile configFile (show cfg)
 `catch`
   \_ -> return ()
   
stripComment :: String -> String
stripComment [] = []
stripComment (x:xs) 
   | x == '#'  = []
   | otherwise = x:stripComment xs
   
declaration :: String -> Maybe (String, String)
declaration xs =
   case break (== '=') xs of
      (key, _:value) -> Just (trim key, trim value)
      _ -> Nothing
      
-----------------------------------------------------------------------------
-- Configuration content

type ConfigEntry a = (String, a, String -> Maybe a, a -> String)

fromConfig :: Config -> ConfigEntry a -> a
fromConfig (C m) (key, a, f, _) = fromMaybe a $
  M.lookup key m >>= f

toConfig :: ConfigEntry a -> a -> Config -> Config
toConfig (key, _, _, f) a (C m) = C $ M.insert key (f a) m

code :: ConfigEntry ExerciseCode
code = ("exercise", makeCode "logic" "dnf", readCode, show)