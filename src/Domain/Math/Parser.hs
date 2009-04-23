module Domain.Math.Parser where

import Prelude hiding ((^))
import Text.Parsing hiding (pParens)
import Common.Transformation
import Domain.Math.Symbolic
import Domain.Math.Equation
import Domain.Math.Expr
import Domain.Math.OrList
import Test.QuickCheck (arbitrary)

scannerExpr :: Scanner
scannerExpr = defaultScanner 
   { keywords          = ["pi", "sqrt", "or"]
   , keywordOperators  = ["==" ]
   , specialCharacters = "+-*/^()[]{},"
   }

parseWith :: TokenParser a -> String -> Either SyntaxError a
parseWith p = f . parse p . scanWith scannerExpr
 where 
   f (e, []) = Right e
   f (_, xs) = Left $ ErrorMessage $ unlines $ map show xs

parseExpr :: String -> Either SyntaxError Expr
parseExpr = parseWith pExpr

pExpr :: TokenParser Expr
pExpr = fromRanged <$> pOperators operatorTable (flip toRanged nul <$> pTerm)

pTerm :: TokenParser Expr
pTerm  =  sqrt <$ pKey "sqrt" <*> pAtom
      <|> pAtomMin

-- An atom, optionally preceded by a (unary) minus
pAtomMin :: TokenParser Expr
pAtomMin = optional (negate <$ pKey "-") id <*> pAtom

pAtom :: TokenParser Expr
pAtom  =  fromInteger <$> pInteger
      <|> (Var . fst) <$> (pVarid <|> pConid)
      <|> (\_ -> symbol "pi") <$> pKey "pi"
      <|> pParens pExpr

pEquations :: TokenParser a -> TokenParser (Equations a)
pEquations p = pLines True (pEquation p)

pEquation :: TokenParser a -> TokenParser (Equation a)
pEquation p = (:==:) <$> p <* pKey "==" <*> p

pOrList :: TokenParser a -> TokenParser (OrList a)
pOrList p = pSepList p (pKey "or")
 where pSepList p q = (\x xs -> OrList (x:xs)) <$> p <*> pList (q *> p)

-- This expression could have a fraction at top-level: both the numerator
-- and denominator are atoms, optionally preceded by a (unary) minus
pFractional :: TokenParser Expr
pFractional = flip ($) <$> pAtomMin <*> optional (flip (/) <$ pKey "/" <*> pAtomMin) id

operatorTable :: OperatorTable Expr
operatorTable = 
   [ (LeftAssociative, [("+", (+)), ("-", (-))])  -- infixl 6
   , (LeftAssociative, [("*", (*)), ("/", (/))])  -- infixl 7
   , (RightAssociative, [("^", (^))])
   ]  

pParens :: TokenParser a -> TokenParser a
pParens p = pKey "(" *> p <* pKey ")"

nul :: Range
nul = Range (Pos 0 0) (Pos 0 0)

-----------------------------------------------------------------------
-- Argument descriptor (for parameterized rules)

instance Argument Expr where
   makeArgDescr = exprArgDescr

exprArgDescr :: String -> ArgDescr Expr
exprArgDescr descr = ArgDescr descr Nothing (either (const Nothing) Just . parseExpr) show arbitrary