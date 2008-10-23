module Domain.Programming.Parser where

import Common.Parsing
import Domain.Programming.Expr
import Domain.Programming.Prelude
import Data.Char

-----------------------------------------------------------
--- Scanner

{- testje = map make $ subs $ fst $ parseRangedLogicPars "   (T -> T) ||  (q /\\ F )"
 where
   make (a, R (p1, p2)) = show a ++ "    ==    \"" ++  take (column p2 - column p1) (drop (column p1 - 1) input) ++ "\""
   input = "   (T -> T) ||  (q /\\ F )"  -}


scannerExpr :: Scanner
scannerExpr = defaultScanner
   { keywords         = ["False", "case", "of", "if", "then", "else"] 
   , keywordOperators = ["->", "\\"]
   }

-----------------------------------------------------------
--- Parser

parseExpr :: String -> Either SyntaxError Expr
parseExpr = f . parse pExpr . scanWith scannerExpr
 where 
   f (e, []) = Right e
   f (_, xs) = Left $ ErrorMessage $ unlines $ map show xs
   
pExpr :: TokenParser Expr
pExpr = foldl1 (<|>) [pLambda, pVar, pApply, pFix, pInt, pIfThenElse, pMatchList]

pLambda :: TokenParser Expr
pLambda = Lambda <$ pKey "\\" <*> pString <* pKey "->" <*> pExpr

pVar :: TokenParser Expr
pVar = (Var . fst) <$> pVarid

pApply :: TokenParser Expr
pApply = Apply <$> pExpr <*> pExpr

pFix :: TokenParser Expr
pFix  = Fix <$> pExpr

pInt :: TokenParser Expr
pInt = (Int . fromInteger) <$> pInteger

pIfThenElse :: TokenParser Expr
pIfThenElse = IfThenElse <$> pExpr <*> pExpr <*> pExpr

pMatchList:: TokenParser Expr
pMatchList = MatchList <$> pExpr <*> pExpr <*> pExpr

{-
myParens :: TokenParser a -> TokenParser a
myParens p = pSpec '(' *> p <* pSpec ')'

-}

-----------------------------------------------------------
--- Pretty-Printer

pprintExpr :: (Expr, Int) -> String
pprintExpr (expr, offset) =  
   case expr of
      Lambda x e -> "\\" ++ x ++ " -> " ++ pprintExpr (e,offset+(length x)+5)
      Var x -> x
      Apply f a -> pprintExpr (f,offset) ++ case a of
                                               (Var _)   -> " "  ++ pprintExpr (a,offset)
                                               _         -> " (" ++ pprintExpr (a,offset) ++ ")"
      Fix f -> pprintExpr (f,offset)
      Int n -> [intToDigit n]
      IfThenElse c t e -> "if " ++ pprintExpr (c,offset) ++ " then\n"
                                ++ spaces (offset+2) ++ pprintExpr (t,offset+2) ++ "\n"
                                ++ spaces offset ++ "else\n"
                                ++ spaces (offset+2) ++ pprintExpr (e,offset+2) ++ "\n"
      MatchList b n c -> "case " ++ pprintExpr (b,offset) ++ " of\n" 
                                 ++ spaces (offset+5) ++ pprintExpr (n,offset+5) ++ "\n"
                                 ++ spaces (offset+5) ++ pprintExpr (c,offset+5)
--      Let x b d -> "let " ++ x ++ " = " ++ pprintExpr (b,offset+(length x)+7) ++ "\n" ++ spaces offset ++
--                   "in " ++ pprintExpr (d,offset)
     where 
      spaces n = take n (Prelude.repeat ' ')

prettyIsort = putStr (pprintExpr (isortE,0) ++ "\n") 
