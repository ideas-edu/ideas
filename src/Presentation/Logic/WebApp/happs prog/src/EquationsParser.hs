{--------------------------------------------------- 
Copyright (c)        2005 - 2006 
Johan Jeuring and Harrie Passier
---------------------------------------------------}
module EquationsParser where

-- Standard Haskell libraries
import Char
import List (intersperse)

-- GHC library
import GHC.Real
import Debug.Trace

-- UU libraries
import UU.Parsing
import UU.Parsing.CharParser
import UU.Pretty
import UU.Scanner


-- Equations model
import Equations (Equations(..), Equation(..), Expr(..))
import EquationsPretty (prettyEquation)
import EquationsEnglishResources

{- Fine tuning insertion and deletion -}

pSymInf a       =  pCostSym   1000 a a
pSymLow a       =  pCostSym      1 a a

{- Simple parsers that should be in the library (and maybe actually are). -}

pparens :: IsParser p Char => p a -> p a
pparens = pPacked (pSymLow '(') (pSymLow ')')  

pinteger :: IsParser p Char => p String
pinteger =  pList1 (pAnySym ['1'..'9'] <|> pSym '0')        
        
pvarid  :: IsParser p Char => p String
pvarid = pList1 (pAnySymInf ['a'..'z'])        

pAnySymInf xs = foldr1 (<|>) (map pSymInf xs)

-- string2int = foldl (\val dig -> (10*val + ord dig -ord '0')) 0

{- Parsing -}

parseEquations  :: String -> Equations
parseEquations  =  fstPair 
                .  evalSteps
                .  parseEquationsMsgs

parseMsgs  :: String -> [Message Char Pos]
parseMsgs  =  getMsgs
           .  parseEquationsMsgs

parseEquationsMsgs  :: String -> Steps (Pair Equations (Pair Input ())) Char Pos
parseEquationsMsgs  =  parseString pEquations
                    .  filter (/=' ')

pEquations :: IsParser p Char => p Equations
pEquations  =  pList (pEquation <* (pSymLow '\n'))

pEquation  :: IsParser p Char => p Equation
pEquation  =  (:=:) <$> pExpr <* pSymInf '=' <*> pExpr

pExpr  :: IsParser p Char => p Expr
pExpr  =  pChainl pTermOp pTerm

pTermOp  ::  IsParser p Char => p (Expr -> Expr -> Expr)
pTermOp  =  (:+:) <$ pSymInf '+'
        <|> (:-:) <$ pSymInf '-'       

pTerm  ::  IsParser p Char => p Expr
pTerm  =   pChainr pFactorOp pFactor

pFactorOp  ::  IsParser p Char => p (Expr -> Expr -> Expr)
pFactorOp  =   (:*:) <$ pSymLow '*'
          <|>  (//)  <$ pSymLow '/'       
          where Con (xd:%xn) // Con (yd:%yn) = Con ((xd*yn) :% (xn*yd)) 
                x            // y            = x:/:y     

pFactor  :: IsParser p Char => p Expr
pFactor  =  Var <$> pvarid 
        <|> (\f x -> Con (f (read x :% 1))) <$> pMaybeMinus <*> pinteger
        <|> pparens pExpr 
        
pMaybeMinus :: IsParser p Char => p (Rational -> Rational) 
pMaybeMinus = opt (const negate <$> pSymLow '-') id 
        
{- Utility functions for the parsers. Should probably be elsewhere. -}

fstPair             :: Pair a b -> a
fstPair (Pair a b)  =  a

myShowMessages :: (Eq s, Show s) => [Message s Pos] -> Equations -> String -> String
myShowMessages  messages equations entered = concatMap (\message -> myShowMessage message equations entered) 
                                                       (removeDuplicatelines messages)
  where removeDuplicatelines [] = []
        removeDuplicatelines (message:messages) = 
          message:removeDuplicatelines (filter (\m -> getLine m /= getLine message) messages)
        
        getLine (Msg _ (Pos line _ _) _) = line

myShowMessage ::  (Eq s, Show s) => Message s Pos -> Equations -> String -> String
myShowMessage (Msg expecting position action) equations entered =
  let (Pos line column filename) = position     
  in disp (    text error_in_lineText >#< pp line >|< text ":" 
          >-<  safeindex ((entered,equations),line) (lines entered) line 
          >-<  text did_you_maybe_meanText 
          >-<  prettyEquation (safeindex ((entered,equations),line) equations line)
          >#<  text "\n"
          )
          40
          ""
  where  safeindex  p   =  \l i -> if i > length l 
                                   then error ("myShowMessage l>i" ++ show p) 
                                   else l!!(i-1) 