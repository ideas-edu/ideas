module Text.OpenMath.Symbol where

data Symbol = Symbol
   { dictionary :: Maybe String
   , symbolName :: String
   }
 deriving (Eq, Ord)

instance Show Symbol where
   show s = maybe "" (++".") (dictionary s) ++ symbolName s
   
makeSymbol :: String -> String -> Symbol
makeSymbol = Symbol . Just

extraSymbol :: String -> Symbol
extraSymbol = Symbol Nothing