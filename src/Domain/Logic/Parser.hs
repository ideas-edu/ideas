-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.Logic.Parser 
   ( parseLogic, parseLogicPars, ppLogic, ppLogicPrio, ppLogicPars
   ) where

import Common.Parsing
import Domain.Logic.Formula

{- testje = map make $ subs $ fst $ parseRangedLogicPars "   (T -> T) ||  (q /\\ F )"
 where
   make (a, R (p1, p2)) = show a ++ "    ==    \"" ++  take (column p2 - column p1) (drop (column p1 - 1) input) ++ "\""
   input = "   (T -> T) ||  (q /\\ F )"  -}
   
logicScanner :: Scanner
logicScanner = (makeCharsSpecial "~" defaultScanner)
   { keywords         = ["T", "F"]
   , keywordOperators = "~" : concatMap (map fst . snd) operatorTable
   }

operatorTable :: OperatorTable Logic
operatorTable = 
   [ (RightAssociative, [("<->", (:<->:))])
   , (RightAssociative, [("||",  (:||:))])
   , (RightAssociative, [("/\\", (:&&:))])
   , (RightAssociative, [("->",  (:->:))])
   ]

-----------------------------------------------------------
--- Parser

-- | Parser for logic formulas that respects all associativity and priority laws 
-- | of the constructors
parseLogic  :: String -> (Ranged Logic, [Message Token])
parseLogic = parse pLogic . scanWith logicScanner
 where
   pLogic = pOperators operatorTable (basicWithPos pLogic)
   
-- | Parser for logic formulas that insists on more parentheses: "and" and "or" are associative, 
-- | but implication and equivalence are not. Priorities of the operators are unknown, and thus 
-- | parentheses have to be written explicitly. No parentheses are needed for Not (Not p). Superfluous
-- | parentheses are permitted
parseLogicPars  :: String -> (Ranged Logic, [Message Token])
parseLogicPars = parse pLogic . scanWith logicScanner
 where
   basic     =  basicWithPos pLogic
   pLogic    =  flip ($) <$> basic <*> optional composed id
   composed  =  flip (binaryOp (:<->:)) <$ pKey "<->" <*> basic
            <|> flip (binaryOp (:->:))  <$ pKey  "->" <*> basic
            <|> (\xs p -> foldr1 (binaryOp (:&&:)) (p:xs)) <$> pList1 (pKey "/\\" *> basic)
            <|> (\xs p -> foldr1 (binaryOp (:||:)) (p:xs)) <$> pList1 (pKey "||"  *> basic)
            
basicWithPos :: Parser Token (Ranged Logic) -> Parser Token (Ranged Logic)
basicWithPos p  =  (\(s, r) -> toRanged (Var s) r) <$> pVarid
               <|> pParens p
               <|> toRanged T <$> pKey "T"
               <|> toRanged F <$> pKey "F"
               <|> unaryOp Not <$> pKey "~" <*> basicWithPos p
                    
-----------------------------------------------------------
--- Pretty-Printer

ppLogic :: Logic -> String
ppLogic = ppLogicPrio 0
        
ppLogicPrio :: Int -> Logic -> String
ppLogicPrio n p = foldLogic (var, binop 3 "->", binop 0 "<->", binop 2 "/\\", binop 1 "||", nott, var "T", var "F") p n ""
 where
   binop prio op p q n = parIf (n > prio) (p (prio+1) . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   nott p _  = ("~"++) . p 4
   parIf b f = if b then ("("++) . f . (")"++) else f

-- | Pretty printer that produces extra parentheses: also see parseLogicPars
ppLogicPars :: Logic -> String
ppLogicPars = ppLogicParsCode 0
        
-- | Implementation uses the well-known trick for fast string concatenation
ppLogicParsCode :: Int -> Logic -> String
ppLogicParsCode n p = foldLogic (var, binop 3 "->", binop 3 "<->", binop 1 "/\\", binop 2 "||", nott, var "T", var "F") p n ""
 where
   binop prio op p q n = parIf (n/=0 && (n==3 || prio/=n)) (p prio . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   nott  p _ = ("~"++) . p 3
   parIf b f = if b then ("("++) . f . (")"++) else f