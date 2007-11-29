{----------------------------------------------------------------
This module contains a temporary parser for proposistion formula

Copyright (c)        2006 - 2007 
Johan Jeuring and Harrie Passier
---------------------------------------------------}


module LogicParser where

-- Standard Haskell libraries
import Char
import List (intersperse)

-- GHC library
import GHC.Real
import Debug.Trace

-- UU libraries
-- UU libraries
import UU.Parsing
import UU.Parsing.CharParser
import UU.Pretty
import UU.Scanner

--import ParserComb 

-- Logic model
import LogicFormula
import LogicDutchResources
import LogicPretty

parseFormula  :: String -> Formula
parseFormula  =  fstPair 
              .  evalSteps
              .  parseFormulaMsgs

parseMsgs  :: String -> [Message Char Pos]
parseMsgs  =  getMsgs
           .  parseFormulaMsgs

parseFormulaMsgs  :: String -> Steps (Pair Formula (Pair Input ())) Char Pos
parseFormulaMsgs  =  parseString pFormula
                  .  filter (/=' ')

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

{-------------------------------------------------------------------------
 Logic parser : formula
 The parser reads a string according to a concrete syntax
 and produces an abstract syntax
-------------------------------------------------------------------------}

pFormula,disjunction,conjunction,implication,basic,nott :: IsParser p Char => p Formula

pFormula     =  pChainr  (const (:<->:) <$>  eqvSym)  disjunction 
disjunction  =  pChainr  (const (:||:)  <$>  orSym )  conjunction 
conjunction  =  pChainr  (const (:&&:)  <$>  andSym)  implication 
implication  =  pChainr  (const (:->:)  <$>  impSym)  basic       

basic = Var <$>  pvarid
     <|> (pparens pFormula)
     <|> const T <$>  (pSym 'T')
     <|> const F <$>  (pSym 'F')
     <|> nott

nott = (\a b -> Not b) <$> notSym <*> basic 

andSym,orSym,impSym,eqvSym,notSym :: IsParser p Char => p String
andSym = pToks "/\\"
orSym  = pToks "||" 
impSym = pToks "->"
eqvSym = pToks "<->"
notSym = pToks "~"

{- Utility functions for the parsers. Should probably be elsewhere. -}

fstPair             :: Pair a b -> a
fstPair (Pair a b)  =  a

-- hack to reuse the Equations code: formula is inserted in a list to get a list instead of a formula

myShowMessages :: (Eq s, Show s) => [Message s Pos] -> Formula -> String -> String
myShowMessages  messages formula entered = concatMap (\message -> myShowMessage message formula entered) 
                                                       (removeDuplicatelines messages)
  where removeDuplicatelines [] = []
        removeDuplicatelines (message:messages) = 
          message:removeDuplicatelines (filter (\m -> getLine m /= getLine message) messages)
        
        getLine (Msg _ (Pos line _ _) _) = line

myShowMessage ::  (Eq s, Show s) => Message s Pos -> Formula -> String -> String
myShowMessage (Msg expecting position action) formula entered =
  let (Pos line column filename) = position     
  in disp (    text error_in_lineText  
          >-<  text did_you_maybe_meanText 
          >-<  prettyFormula formula        
          >#<  text "\n"
          )
          40
          ""
  where   safeindex :: [a] -> Int -> a
          safeindex  l  =  \i -> if i > length l 
                                   then error ("myShowMessage l>i") 
                                   else l!!(i-1) 
                                   