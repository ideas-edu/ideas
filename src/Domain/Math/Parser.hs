module Domain.Math.Parser where

import Common.Parsing hiding (pParens)
import Domain.Math.Symbolic
import Domain.Math.Expr

scannerExpr :: Scanner
scannerExpr = defaultScanner {keywords = ["pi", "sqrt"], specialCharacters  = "+-*/()[]{},"}

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
pAtom  =  Con <$> pInteger
      <|> (Var . fst) <$> (pVarid <|> pConid)
      <|> (\_ -> symbol "pi") <$> pKey "pi"
      <|> pParens pExpr

-- This expression could have a fraction at top-level: both the numerator
-- and denominator are atoms, optionally preceded by a (unary) minus
pFractional :: TokenParser Expr
pFractional = flip ($) <$> pAtomMin <*> optional (flip (/) <$ pKey "/" <*> pAtomMin) id

operatorTable :: OperatorTable Expr
operatorTable = 
   [ (LeftAssociative, [("+", (+)), ("-", (-))])  -- infixl 6
   , (LeftAssociative, [("*", (*)), ("/", (/))])  -- infixl 7
   ]  

pParens :: TokenParser a -> TokenParser a
pParens p = pKey "(" *> p <* pKey ")"

nul :: Range
nul = Range (Pos 0 0) (Pos 0 0)