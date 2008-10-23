module Domain.Programming.Parser where

import Common.Parsing
import Domain.Programming.Expr
import Domain.Programming.Prelude
import Domain.Programming.Eval
import Data.Char

-----------------------------------------------------------
--- Scanner

{- testje = map make $ subs $ fst $ parseRangedLogicPars "   (T -> T) ||  (q /\\ F )"
 where
   make (a, R (p1, p2)) = show a ++ "    ==    \"" ++  take (column p2 - column p1) (drop (column p1 - 1) input) ++ "\""
   input = "   (T -> T) ||  (q /\\ F )"  -}


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
pOps = pApply -- if any ops are introduced: pChainl O1 (pChainl O2 ... (pChainl On pApply)

pApply :: TokenParser Expr
pApply = app <$> pList1 pAtom
 where 
   app ats = case ats of 
                  [at] -> at
                  _    -> foldl1 Apply ats

pAtom :: TokenParser Expr
pAtom  =  (Var . fst) <$> pVarid
      <|> (\(f,_) e body -> Apply (Lambda f body) (Fix $ Lambda f e))  -- Fix
              <$ pKey "let" <*> pVarid
              <* pKey "="   <*> pExpr
              <* pKey "in"  <*> pExpr
      <|> (Int . fromInteger) <$> pInteger       -- Int
      <|> pSpec '(' *> pExpr <* pSpec ')'        -- parantheses
      <|> IfThenElse <$ pKey "if"   <*> pExpr    -- If
                     <* pKey "then" <*> pExpr 
                     <* pKey "else" <*> pExpr
      <|> MatchList <$  pKey "case" <*> pExpr    -- case
                    <*  pKey "of"   
                             <*> pAtom              -- nil
                             <*  pSpec ';' <*> pAtom -- cons

-----------------------------------------------------------
--- Pretty-Printer

pprintExpr :: (Expr, Int) -> String
pprintExpr (expr, offset) =  
   case expr of
      Lambda x e -> "\\" ++ x ++ " -> " ++ pprintExpr (e,offset+(length x)+5)
      Var x -> x
      --Apply (Lambda f body) (Fix $ Lambda f e))
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
     where 
      spaces n = take n (Prelude.repeat ' ')

prettyIsort = putStr (pprintExpr (isortE,0) ++ "\n") 
