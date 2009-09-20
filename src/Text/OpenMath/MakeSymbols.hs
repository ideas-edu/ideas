-- Create symbol definitions from OpenMath content dictionary (ocd)
module Text.OpenMath.MakeSymbols where

import Text.OpenMath.ContentDictionary hiding (main)
import Control.Monad
import Data.Char
import Data.List

main :: IO ()
main = do
   let base   = "lib/Dictionaries"
       target = "src/Text/Openmath/Dictionary"
   ocds <- findOCDs base
   forM_ ocds $ \s -> do
      let modn = target ++ "/" ++ moduleName s ++ ".hs" 
      txt <- makeSymbols (base ++ "/" ++ s)
      putStrLn $ "  writing " ++ modn
      writeFile modn txt

moduleName :: String -> String
moduleName s = dropSuffix (map toUpper (take 1 s) ++ drop 1 s)
   
dropDir :: String -> String
dropDir s 
   | '/' `elem` s = dropDir $ drop 1  $dropWhile (/='/') s 
   | otherwise    = s 

dropSuffix :: String -> String
dropSuffix = takeWhile (/='.')

makeSymbols :: String -> IO String
makeSymbols file = do
   cd <- readContentDictionary file
   let cdname = dropDir file
   return $ unlines $ 
      [ "-- Automatically generated from content dictionary " ++ cdname ++ ". \
        \ Do not change."
      , "module Text.OpenMath.Dictionary." ++ moduleName cdname ++ " where\n"
      , "import Text.OpenMath.Symbol\n"
      , makeSymbolList cd
      ] ++
      map (makeSymbol (dropSuffix cdname)) (definitions cd)

symbolIdentifier :: Definition -> String
symbolIdentifier d = f (symbolName d) ++ "Symbol" 
 where f xs = map toLower (take 1 xs) ++ camelCase (drop 1 xs)

camelCase :: String -> String
camelCase []         = []
camelCase ('_':x:xs) = toUpper x : camelCase xs
camelCase (x:xs)     = x : camelCase xs 

makeSymbolList :: ContentDictionary -> String
makeSymbolList cd = unlines 
   [ "-- | List of symbols defined in " ++ dictionaryName cd ++ " dictionary" 
   , name ++ " :: [Symbol]"
   , name ++ " = [" ++ concat (intersperse ", " list) ++ "]"
   ]
 where
   name = dictionaryName cd ++ "List"
   list = map symbolIdentifier (definitions cd)

makeSymbol :: String -> Definition -> String
makeSymbol dict def = unlines $
   makeComment 80 (symbolDescription def) ++
   [ name ++ " :: Symbol"
   , name ++ " = makeSymbol " ++ show dict ++ " " ++ show (symbolName def)
   ]
 where
    name = symbolIdentifier def 
    
makeComment :: Int -> String -> [String]
makeComment n = breaks . comment . words
 where
   comment xs = ["{-|"] ++ xs ++ ["-}"]
   accLength  = scanl (\n -> (+n) . succ . length) 0
   breaks xs
      | null xs   = []
      | otherwise =
           case break ((>=n) . fst) (zip (drop 1 (accLength xs)) xs) of
              (as, bs) -> unwords (map snd as) : breaks (map snd bs)