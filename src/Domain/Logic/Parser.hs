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
   ( parseLogic, ppLogic, ppLogicPrio, ppLogicInContext,myShowMessages
   ) where

import UU.Parsing
import UU.Parsing.CharParser
import UU.Scanner
import UU.Pretty
  -- for the combinators used in myShowMessages
  -- Should maybe be simplified?

import Domain.Logic.Formula
import Domain.Logic.Zipper
--import Logic.Solver.LogicDutchResources
--import Logic.Solver.LogicPretty
import Data.Char
import Domain.Logic.Solver.LogicEnglishResources
  -- for myShowMessages

-----------------------------------------------------------
--- Parser

parseLogic  :: String -> (Logic, [Message Char Pos])
parseLogic input = (result, messages)
 where
   steps    = parseString pLogic (filter (not . isSpace) input)
   result   = fstPair (evalSteps steps)
   messages = getMsgs steps

fstPair             :: Pair a b -> a
fstPair (Pair a b)  =  a

pLogic       =  pChainr  ((:<->:) <$  eqvSym)  disjunction 
disjunction  =  pChainr  ((:||:)  <$  orSym )  conjunction 
conjunction  =  pChainr  ((:&&:)  <$  andSym)  implication 
implication  =  pChainr  ((:->:)  <$  impSym)  basic       

basic = Var <$> pvarid
     <|> pparens pLogic
     <|> T <$ pSym 'T'
     <|> F <$ pSym 'F'
     <|> Not <$ notSym <*> basic 

andSym = pToks "/\\"
orSym  = pToks "||" 
impSym = pToks "->"
eqvSym = pToks "<->"
notSym = pToks "~"

pparens :: CharParser a -> CharParser a
pparens = pPacked (pSymLow '(') (pSymLow ')')  

pvarid  :: CharParser String
pvarid = pList1 (pAnySymInf ['a'..'z'])    

pAnySymInf xs = foldr1 (<|>) (map pSymInf xs)

pSymInf a       =  pCostSym   1000 a a
pSymLow a       =  pCostSym      1 a a

-- hack to reuse the Equations code: formula is inserted in a list to get a list instead of a formula

myShowMessages :: (Eq s, Show s) => [Message s Pos] -> Logic -> String -> String
myShowMessages  messages formula entered = concatMap (\message -> myShowMessage message formula entered) 
                                                       (removeDuplicatelines messages)
  where removeDuplicatelines [] = []
        removeDuplicatelines (message:messages) = 
          message:removeDuplicatelines (filter (\m -> getLine m /= getLine message) messages)
        
        getLine (Msg _ (Pos line _ _) _) = line

myShowMessage ::  (Eq s, Show s) => Message s Pos -> Logic -> String -> String
myShowMessage (Msg expecting position action) formula entered =
  let (Pos line column filename) = position     
  in disp (    text error_in_lineText >#< pp line >|< text ": 1" 
          >-<  text did_you_maybe_meanText 
          >-<  ppLogic formula
          >#<  text "\n"
          )
          40
          ""
  where   safeindex :: [a] -> Int -> a
          safeindex  l  =  \i -> if i > length l 
                                   then error ("myShowMessage l>i") 
                                   else l!!(i-1) 
                                   
-----------------------------------------------------------
--- Pretty-Printer

ppLogic :: Logic -> String
ppLogic = ppLogicPrio 0
        
ppLogicPrio :: Int -> Logic -> String
ppLogicPrio n p = foldLogic (const, binop 3 "->", binop 0 "<->", binop 2 "/\\", binop 1 "||", nott, const "T", const "F") p n
 where
   binop prio op p q n = parIf (n > prio) (unwords [p (prio+1), op, q prio])
   nott p n = "~" ++ p 4
   parIf b s = if b then "(" ++ s ++ ")" else s
   
ppLogicInContext :: LogicInContext -> String
ppLogicInContext = ppLogicInContextPrio 0

-- hack
ppLogicInContextPrio :: Int -> LogicInContext -> String
ppLogicInContextPrio prio (Loc ctx logic) = concatMap f . ppLogicPrio prio . noContext $ Loc ctx (Var "*")
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
   getPrio (ImplR  _ _) = 3
