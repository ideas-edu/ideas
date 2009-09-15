module Documentation.EqualityPage where

import Documentation.DefaultPage
import Common.View
import Domain.Math.Expr hiding ((^))
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Views
import Domain.Math.Polynomial.Exercises
import Domain.Math.Numeric.Views
import Domain.Math.SquareRoot.Views
import Text.HTML
import qualified Text.XML as XML

main :: IO ()
main = writeFile "out.html" $ show $ make show eqs terms code
 where
   eqs :: [(String, Expr -> Expr -> Bool)]
   eqs = 
      [ ("syntactic", (==))
      , ("cleanup", \x y -> cleanUpExpr x == cleanUpExpr y)
      , ("normalized", \x y -> normExpr cleanUpExpr x == normExpr cleanUpExpr y)
      , ("semantic", viewEquivalent (polyViewWith (squareRootViewWith rationalView)))
      ]
   
   terms :: [(Expr, Expr)]
   terms = 
      let x = Var "x" in
      [ (x+4/2, x+2)
      , (x+3, 3+x)
      , (2*(x+1), 2+2*x)
      , (sqrt 8, 2 * sqrt 2)
      , ((x+2)+3, x+5)
      , (3+(2+x), 5+x)
      , (3/0, 1)
      ]
      
   code :: [Int]
   code =  [14,12,8,14,14,14,0]

make :: (a -> String) -> [(String, a -> a -> Bool)] -> [(a, a)] -> [Int] -> HTML
make pp eqs terms code = 
   defaultPage "Equalities" 0 $ do
      h1 "Equalities"
      let bools  = map (\(x, y) -> map (\(_, f) -> f x y) eqs) terms
          expect = map ((++ repeat False) . fromInt) (code ++ repeat 0)
          header   = map (bold . text) ("term1" : "term2" : map fst eqs)
          f (x, y) bs es = ttText (pp x) : ttText (pp y) : zipWith g bs es
          g b e = (if (b==e) then id else errorLine) (text $ showBool b)
      table (header : zipWith3 f terms bools expect)
      para $ do
         bold (text "Validation code: ")
         ttText $ show $ map toInt bools
      
toInt :: [Bool] -> Int
toInt = sum . zipWith f [0..]
 where f i b = if b then 2^i else 0
 
fromInt :: Int -> [Bool]
fromInt 0 = []
fromInt n = (n `mod` 2 == 1) : fromInt (n `div` 2)
   
errorLine :: HTMLBuilder -> HTMLBuilder
errorLine b = XML.element "font" $ do
   "color" XML..=. "red"
   bold b