module Domain.Programming.Parser where

import Common.Parsing
import Domain.Programming.Expr
import Domain.Programming.Prelude
import Domain.Programming.Eval
import UU.Scanner (pVarsym)
import Data.Char

-----------------------------------------------------------
--- Scanner

scannerExpr :: Scanner
scannerExpr = defaultScanner
   { keywords         = [ "case", "of"
                        , "if", "then", "else"
                        , "let", "in"
                        ] 
   , keywordOperators = ["->", "\\", "="]
   }

-----------------------------------------------------------
--- Parser

parseExpr :: String -> Either SyntaxError Expr
parseExpr = f . parse pExpr . scanWith scannerExpr
 where 
   f (e, []) = Right e
   f (_, xs) = Left $ ErrorMessage $ unlines $ map show xs
   
pExpr :: TokenParser Expr
pExpr = pLambda <|> pOps

pLambda :: TokenParser Expr
pLambda = lambda <$ pKey "\\" <*> pVarid <* pKey "->" <*> pExpr
  where 
    lambda x e = Lambda (fst x) e

pOps :: TokenParser Expr 
pOps = flip ($) <$> pApply <*> optional ((\op r l -> Apply (Apply (Var op) l) r) <$> pVarsym <*> pApply) id

pApply :: TokenParser Expr
pApply = app <$> pList1 pAtom
 where 
   app ats = case ats of 
                  [at] -> at
                  _    -> foldl1 Apply ats

pAtom :: TokenParser Expr
pAtom  =  (Var . fst) <$> (pVarid <|> pConid)
      <|> (\(f,_) e body -> Apply (Lambda f body) (Fix $ Lambda f e))  -- Fix
              <$ pKey "let" <*> pVarid
              <* pKey "="   <*> pExpr
              <* pKey "in"  <*> pExpr 
      <|> (Int . fromInteger) <$> pInteger       -- Int
      <|> pSpec '(' *> pExpr <* pSpec ')'        -- parentheses
      <|> IfThenElse <$ pKey "if"   <*> pExpr    -- If
                     <* pKey "then" <*> pExpr 
                     <* pKey "else" <*> pExpr
      <|> MatchList <$  pKey "case" <*> pExpr    -- case
                    <*  pKey "of"   
                             <*> pExpr              -- nil
                             <*  pSpec ';' <*> pExpr -- cons

-----------------------------------------------------------
--- Pretty-Printer

ppExpr :: (Expr, Int) -> String
ppExpr (expr, i) = 
   case expr of
      Lambda x e -> 
          "\\" ++ x ++ " -> " ++ ppExpr (e,i+(length x)+5)
      Var x -> 
          x
      Apply f a -> 
          "(" ++ case f of
               (Apply (Var op) l) -> 
                   if not $ isAlpha (head op) then -- infix operator
                       ppExpr (l,i) ++ " " ++ op ++ " " ++ ppExpr (a,i)
                   else
                       op ++ " " ++ ppExpr (l,i) ++ " " ++ ppExpr (a,i)
               _ -> ppExpr (f,i) ++ case a of
                   (Var _)   -> " "  ++ ppExpr (a,i)
                   (Int _)   -> " "  ++ ppExpr (a,i)
                   _         -> " (" ++ ppExpr (a,i) ++ ")"
          ++ ")"
      Fix f -> 
          case f of 
              (Lambda x e) -> "let " ++ x ++ " = " ++ ppExpr (e,i+(length x)+7) ++ "\n"
                               ++ spc i ++ "in " ++ x
              _ -> error "Must be a lambda expression"
      Int n -> 
          [intToDigit n]
      IfThenElse c t e -> 
          "if " ++ ppExpr (c,i) ++ " then\n"
          ++ spc (i+2) ++ ppExpr (t,i+2) ++ "\n"
          ++ spc i ++ "else\n"
          ++ spc (i+2) ++ ppExpr (e,i+2)
      MatchList b n c -> 
          "case " ++ ppExpr (b,i) ++ " of\n" 
          ++ spc (i+5) ++ ppExpr (n,i+5) ++ ";\n"
          ++ spc (i+5) ++ ppExpr (c,i+5)
     where 
      spc n = take n (Prelude.repeat ' ')


prettyIsort = putStr (ppExpr (isortE,0) ++ "\n") 

testje = eval (isPermE # mylist # (isort # mylist))
  where
    isort = (\(Right e) -> e) $ parseExpr $ ppExpr (isortE,0)
