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

pprintExpr :: (Expr, Int) -> String
pprintExpr (expr, offset) = 
   case expr of
      Lambda x e -> "\\" ++ x ++ " -> " ++ pprintExpr (e,offset+(length x)+5)
      Var x -> x
      Apply (Lambda f body) (Fix (Lambda _ e)) -> "let " ++ f ++ " = " ++ pprintExpr (e,offset+(length f)+7)
                                                  ++ "\n" ++ spaces offset ++ "in " ++ pprintExpr (body, offset)
      Apply f a -> "(" ++ case f of
                        (Apply (Var op) l) -> if not (isAlpha (head op)) then 
                                                  pprintExpr (l,offset) ++ " " ++ op ++ " " ++ pprintExpr (a,offset)
                                              else
                                                  op ++ " " ++ pprintExpr (l,offset) ++ " " ++ pprintExpr (a,offset)
                        _ -> pprintExpr (f,offset) ++ case a of
                                               (Var _)   -> " "  ++ pprintExpr (a,offset)
                                               (Int _)   -> " "  ++ pprintExpr (a,offset)
                                               _         -> " (" ++ pprintExpr (a,offset) ++ ")"
                   ++ ")"
      Fix f -> case f of 
                 (Lambda x e) -> "let " ++ x ++ " = " ++ pprintExpr (e,offset+(length x)+7) ++ "\n"
                                 ++ spaces offset ++ "in " ++ x
                 _ -> error "Must be a function"
      Int n -> [intToDigit n]
      IfThenElse c t e -> "if " ++ pprintExpr (c,offset) ++ " then\n"
                                ++ spaces (offset+2) ++ pprintExpr (t,offset+2) ++ "\n"
                                ++ spaces offset ++ "else\n"
                                ++ spaces (offset+2) ++ pprintExpr (e,offset+2)
      MatchList b n c -> "case " ++ pprintExpr (b,offset) ++ " of\n" 
                                 ++ spaces (offset+5) ++ pprintExpr (n,offset+5) ++ ";\n"
                                 ++ spaces (offset+5) ++ pprintExpr (c,offset+5)
     where 
      spaces n = take n (Prelude.repeat ' ')


prettyIsort = putStr (pprintExpr (isortE,0) ++ "\n") 

testje = eval (isPermE # mylist # (isort # mylist))
  where
    isort = (\(Right e) -> e) $ parseExpr $ pprintExpr (isortE,0)