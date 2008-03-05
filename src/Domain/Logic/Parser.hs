-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on UU parsing library)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Logic.Parser 
   ( parseLogic, parseLogicPars, ppLogic, ppLogicPrio, ppLogicPars
   ) where

import UU.Parsing
import UU.Parsing.CharParser
import UU.Scanner
import Domain.Logic.Formula
import Data.Char

-----------------------------------------------------------
--- Parser

-- | Parser for logic formulas that respects all associativity and priority laws 
-- | of the constructors
parseLogic  :: String -> (Logic, [Message Char Pos])
parseLogic = runParser pLogic
 where
   pLogic       =  pChainr  ((:<->:) <$  eqvSym)  disjunction 
   disjunction  =  pChainr  ((:||:)  <$  orSym )  conjunction 
   conjunction  =  pChainr  ((:&&:)  <$  andSym)  implication 
   implication  =  pChainr  ((:->:)  <$  impSym)  basic
   basic        =  basicWith pLogic

-- | Parser for logic formulas that insists on more parentheses: "and" and "or" are associative, 
-- | but implication and equivalence are not. Priorities of the operators are unknown, and thus 
-- | parentheses have to be written explicitly. No parentheses are needed for Not (Not p). Superfluous
-- | parentheses are permitted
parseLogicPars  :: String -> (Logic, [Message Char Pos])
parseLogicPars = runParser pLogic
 where
   basic     =  basicWith pLogic
   pLogic    =  flip ($) <$> basic <*> opt composed id
   composed  =  flip (:<->:) <$ eqvSym <*> basic
            <|> flip (:->:)  <$ impSym <*> basic
            <|> (\xs p -> foldr1 (:&&:) (p:xs)) <$> pList1_gr (andSym *> basic)
            <|> (\xs p -> foldr1 (:||:) (p:xs)) <$> pList1_gr (orSym *> basic)

basicWith :: CharParser Logic -> CharParser Logic
basicWith p  =  Var <$> pvarid
            <|> pparens p
            <|> T <$ pSym 'T'
            <|> F <$ pSym 'F'
            <|> Not <$ notSym <*> basicWith p 
                
andSym = pToks "/\\"
orSym  = pToks "||" 
impSym = pToks "->"
eqvSym = pToks "<->"
notSym = pToks "~"

fstPair :: Pair a b -> a
fstPair (Pair a b)  =  a

runParser  :: CharParser a -> String -> (a, [Message Char Pos])
runParser pLogic input = (result, messages)
 where
   steps    = parseString pLogic (filter (not . isSpace) input)
   result   = fstPair (evalSteps steps)
   messages = getMsgs steps
   
pparens :: CharParser a -> CharParser a
pparens = pPacked (pSymLow '(') (pSymLow ')')  

pvarid  :: CharParser String
pvarid = pList1 (pAnySymInf ['a'..'z'])    

pAnySymInf xs = foldr1 (<|>) (map pSymInf xs)

pSymInf a       =  pCostSym   1000 a a
pSymLow a       =  pCostSym      1 a a
                                   
-----------------------------------------------------------
--- Pretty-Printer

ppLogic :: Logic -> String
ppLogic = ppLogicPrio 0
        
ppLogicPrio :: Int -> Logic -> String
ppLogicPrio n p = foldLogic (var, binop 3 "->", binop 0 "<->", binop 2 "/\\", binop 1 "||", nott, var "T", var "F") p n ""
 where
   binop prio op p q n = parIf (n > prio) (p (prio+1) . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   nott p n  = ("~"++) . p 4
   parIf b f = if b then ("("++) . f . (")"++) else f
   
{-
ppLogicInContext :: Context Logic -> String
ppLogicInContext = ppLogicInContextPrio 0

-- hack
ppLogicInContextPrio :: Int -> Context Logic -> String
ppLogicInContextPrio = undefined prio (Loc ctx logic) = concatMap f . ppLogicPrio prio . noContext $ Loc ctx (Var "*")
 where
   f '*' = "[ " ++ ppLogicPrio (getPrio ctx) logic ++ " ]"
   f c   = [c]
   getPrio Top          = prio
   getPrio (NotD     _) = 5
   getPrio (AndL   _ _) = 3
   getPrio (AndR   _ _) = 2
   getPrio (OrL    _ _) = 2
   getPrio (OrR    _ _) = 1
   getPrio (EquivL _ _) = 1
   getPrio (EquivR _ _) = 0
   getPrio (ImplL  _ _) = 4
   getPrio (ImplR  _ _) = 3 -}

-- | Pretty printer that produces extra parentheses: also see parseLogicPars
ppLogicPars :: Logic -> String
ppLogicPars = ppLogicParsCode 0
        
-- | Implementation uses the well-known trick for fast string concatenation
ppLogicParsCode :: Int -> Logic -> String
ppLogicParsCode n p = foldLogic (var, binop 3 "->", binop 3 "<->", binop 1 "/\\", binop 2 "||", nott, var "T", var "F") p n ""
 where
   binop prio op p q n = parIf (n/=0 && (n==3 || prio/=n)) (p prio . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   nott  p n = ("~"++) . p 3
   parIf b f = if b then ("("++) . f . (")"++) else f