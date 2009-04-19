module Domain.Math.Parser where

import Prelude hiding ((^))
import Text.Parsing hiding (pParens)
import Common.Transformation
import Domain.Math.Symbolic
import Domain.Math.Equation
import Domain.Math.Expr
import Test.QuickCheck (arbitrary)

scannerExpr :: Scanner
scannerExpr = defaultScanner {keywords = ["pi", "sqrt"], specialCharacters  = "+-*/^()[]{},"}

parseExpr :: String -> Either SyntaxError Expr
parseExpr = f . parse pExpr . scanWith scannerExpr
 where 
   f (e, []) = Right e
   f (_, xs) = Left $ ErrorMessage $ unlines $ map show xs

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