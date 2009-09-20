module Documentation.TestsPage (main) where

import Data.Char
import Data.List
import Documentation.DefaultPage
import System.Environment
import Text.HTML
import qualified Text.XML as XML

main :: IO ()
main = do
   args <- getArgs
   case args of
      [fileIn, fileOut] -> do
         input <- readFile fileIn
         generatePage (up 1 ++ fileOut) (testsPage input)
      _ -> fail "Invalid invocation"
   
testsPage :: String -> HTML
testsPage input = defaultPage "Tests" 0 $ do 
   h1 "Tests"
   let (hs, bs) = unzip (map format (lines input))
   bold (text "Failures: ") 
   text $ show $ length $ filter not bs
   brs hs
 where
   format :: String -> (HTMLBuilder, Bool)
   format s
      | "* " `isPrefixOf` s =
           (h2 (drop 2 s), True)
      | "** " `isPrefixOf` s =
           (bold (text $ drop 3 s), True)
      | any (`elem` ws) ["failed", "error", "error:", "falsifiable"] =
           (errorLine (ttText (htmlString s)), False)
      | otherwise = 
           (ttText (htmlString s), True)
    where
      ws = map (map toLower . filter isAlpha) (words s)
      
      
brs :: [HTMLBuilder] -> HTMLBuilder
brs = mapM_ (>> br)

-- no backspaces
htmlString :: String -> String
htmlString = reverse . f 0 . reverse
 where
   f _[] = []
   f i (x:xs) 
      | x == chr 8 = f (i+1) xs
      | i >  0     = f (i-1) xs
      | otherwise  = x:f 0 xs
      
errorLine :: HTMLBuilder -> HTMLBuilder
errorLine b = XML.element "font" $ do
   "color" XML..=. "red"
   bold b