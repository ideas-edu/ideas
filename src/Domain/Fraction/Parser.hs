-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on UU parsing library)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Fraction.Parser 
   ( parseFrac, ppFrac, ppFracPrio, ppFracInContext, ppFracPars
   ) where

import UU.Parsing
import UU.Parsing.CharParser
import UU.Scanner
import Domain.Fraction.Frac
import Domain.Fraction.Zipper
import Data.Char
import Ratio

-----------------------------------------------------------
--- Parser

-- | Parser for logic formulas that respects all associativity and priority laws 
-- | of the constructors
parseFrac  :: String -> (Frac, [Message Char Pos])
parseFrac = runParser pFrac
 where
   pFrac   =  pChainl ((:+:) <$ addSym <|> (:-:) <$ subSym) pFrac'
   pFrac'  =  pChainl ((:*:) <$ mulSym <|> (:/:) <$ divSym) pFrac'' 
   pFrac'' =  Var <$> pVar <|> Lit <$> pRat <|> pparens pFrac

pNat :: CharParser Integer
pNat = read <$> pList1 ('0' <..> '9')

pRat :: CharParser Rational
pRat = (\x _ y -> x%y) <$> pNat <*> pSym '%' <*> pNat

mulSym = pSym '*'
divSym = pSym '/' 
addSym = pSym '+'
subSym = pSym '-'

fstPair :: Pair a b -> a
fstPair (Pair a b)  =  a

runParser  :: CharParser a -> String -> (a, [Message Char Pos])
runParser pFrac input = (result, messages)
 where
   steps    = parseString pFrac (filter (not . isSpace) input)
   result   = fstPair (evalSteps steps)
   messages = getMsgs steps
   
pparens :: CharParser a -> CharParser a
pparens = pPacked (pSymLow '(') (pSymLow ')')  

pVar :: CharParser String
pVar = pList1 (pAnySymInf ['a'..'z'])    

pAnySymInf xs = foldr1 (<|>) (map pSymInf xs)

pSymInf a       =  pCostSym   1000 a a
pSymLow a       =  pCostSym      1 a a
                                   
-----------------------------------------------------------
--- Pretty-Printer

ppFrac :: Frac -> String
ppFrac = ppFracPrio 0
        
ppFracPrio :: Int -> Frac -> String
ppFracPrio n p = foldFrac (var, lit, binop 7 "*", binop 7 "/", binop 6 "+", binop 6 "-") p n ""
 where
   binop prio op p q n = parIf (n > prio) (p (prio+1) . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   lit       = const . (++) . show
   nott p n  = ("~"++) . p 4
   parIf b f = if b then ("("++) . f . (")"++) else f
   
ppFracInContext :: FracInContext -> String
ppFracInContext = ppFracInContextPrio 0

-- hack
ppFracInContextPrio :: Int -> FracInContext -> String
ppFracInContextPrio prio (Loc ctx logic) = concatMap f . ppFracPrio prio . noContext $ Loc ctx (Var "*")
 where
   f '*' = "[ " ++ ppFracPrio (getPrio ctx) logic ++ " ]"
   f c   = [c]
   getPrio Top          = prio
   getPrio (AddL _ _) = 6
   getPrio (AddR _ _) = 6
   getPrio (SubL _ _) = 6
   getPrio (SubR _ _) = 6
   getPrio (DivL _ _) = 7
   getPrio (DivR _ _) = 7
   getPrio (MulL _ _) = 7
   getPrio (MulR _ _) = 7

-- | Pretty printer that produces extra parentheses: also see parseFracPars
ppFracPars :: Frac -> String
ppFracPars = ppFracParsCode 0
        
-- | Implementation uses the well-known trick for fast string concatenation
ppFracParsCode :: Int -> Frac -> String
ppFracParsCode n p = foldFrac (var, lit, binop 7 "*", binop 7 "/", binop 6 "+", binop 6 "-") p n ""
 where
   binop prio op p q n = parIf (n/=0 && (n==3 || prio/=n)) (p prio . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   lit       = const . (++) . show
   nott  p n = ("~"++) . p 3
   parIf b f = if b then ("("++) . f . (")"++) else f
