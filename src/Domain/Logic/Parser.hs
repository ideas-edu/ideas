-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Logic.Parser
   ( parseLogic, parseLogicPars, parseLogicUnicodePars
   , ppLogic, ppLogicPrio, ppLogicPars, ppLogicUnicodePars
   ) where

import Text.Parsing
import Control.Arrow
import Domain.Logic.Formula
   
logicScanner :: Scanner
logicScanner = (makeCharsSpecial "~" defaultScanner)
   { keywords         = ["T", "F"]
   , keywordOperators = "~" : concatMap (map fst . snd) operatorTable
   }

logicUnicodeScanner :: Scanner
logicUnicodeScanner = (makeCharsSpecial (concat unicodeSyms) defaultScanner)
   { keywords         = ["T", "F"]
   , keywordOperators = unicodeSyms
   }
   
operatorTable :: OperatorTable SLogic
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
parseLogic :: String -> Either SyntaxError (Ranged SLogic)
parseLogic = analyseAndParse pLogic . scanWith logicScanner
 where
   pLogic = pOperators operatorTable (basicWithPos pLogic)
   
-- | Parser for logic formulas that insists on more parentheses: "and" and "or" are associative, 
-- | but implication and equivalence are not. Priorities of the operators are unknown, and thus 
-- | parentheses have to be written explicitly. No parentheses are needed for Not (Not p). Superfluous
-- | parentheses are permitted
parseLogicPars :: String -> Either SyntaxError (Ranged SLogic)
parseLogicPars s
   = left ambiguousOperators
   $ analyseAndParse (pLogicGen asciiTuple)
   $ scanWith logicScanner s
 where
   ambiguousOperators err =
      let msg = ErrorMessage "Ambiguous use of operators (write parentheses)"
      in either (const err) (const msg) (parseLogic s)

parseLogicUnicodePars :: String -> Either SyntaxError (Ranged SLogic)
parseLogicUnicodePars = analyseAndParse (pLogicGen unicodeTuple)
                      . scanWith logicUnicodeScanner

pLogicGen (impl, equiv, and, or, nt, tr, fl) = pLogic
 where
   pLogic = flip ($) <$> basic <*> optional composed id
   basic     =  basicWithPosGen (nt, tr, fl) pLogic
   composed  =  flip (binaryOp (:<->:)) <$ pKey equiv <*> basic
            <|> flip (binaryOp (:->:))  <$ pKey impl  <*> basic
            <|> (\xs p -> foldr1 (binaryOp (:&&:)) (p:xs)) <$> pList1 (pKey and *> basic)
            <|> (\xs p -> foldr1 (binaryOp (:||:)) (p:xs)) <$> pList1 (pKey or  *> basic)
 
basicWithPos :: Parser Token (Ranged SLogic) -> Parser Token (Ranged SLogic)
basicWithPos = basicWithPosGen ("~", "T", "F")

basicWithPosGen (nt, tr, fl) p = 
   (\(s, r) -> toRanged (Var s) r) <$> pVarid
   <|> pParens p
   <|> toRanged T  <$> pKey tr
   <|> toRanged F  <$> pKey fl
   <|> unaryOp Not <$> pKey nt <*> basicWithPos p

-----------------------------------------------------------
--- Helper-function for parentheses analyses

analyseAndParse :: Parser Token a -> [Token] -> Either SyntaxError a
analyseAndParse p ts =
   case checkParentheses ts of
      Just err -> Left err
      Nothing  -> case parse p ts of
                     (_, m:_) -> Left (fromMessage m)
                     (a, _)   -> Right a

-----------------------------------------------------------
--- Pretty-Printer

ppLogic :: SLogic -> String
ppLogic = ppLogicPrio 0
        
ppLogicPrio :: Int -> SLogic -> String
ppLogicPrio n p = foldLogic (var, binop 3 "->", binop 0 "<->", binop 2 "/\\", binop 1 "||", nott, var "T", var "F") p n ""
 where
   binop prio op p q n = parIf (n > prio) (p (prio+1) . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   nott p _  = ("~"++) . p 4
   parIf b f = if b then ("("++) . f . (")"++) else f

-- | Pretty printer that produces extra parentheses: also see parseLogicPars
ppLogicPars :: SLogic -> String
ppLogicPars = ppLogicParsGen asciiTuple

-- | Pretty printer with unicode characters
ppLogicUnicodePars :: SLogic -> String
ppLogicUnicodePars = ppLogicParsGen unicodeTuple

ppLogicParsGen (impl, equiv, and, or, nt, tr, fl) p = foldLogic alg p 0 ""
 where
   alg = (var, binop 3 impl, binop 3 equiv, binop 1 and, binop 2 or, nott, var tr, var fl)
   binop prio op p q n = parIf (n/=0 && (n==3 || prio/=n)) 
                               (p prio . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   nott  p _ = (nt++) . p 3
   parIf b f = if b then ("("++) . f . (")"++) else f

-----------------------------------------------------------
--- Ascii symbols

--asciiSyms :: [String]
--asciiSyms = [implASym, equivASym, andASym, orASym, notASym]

asciiTuple = (implASym, equivASym, andASym, orASym, notASym, "T", "F")

implASym, equivASym, andASym, orASym, notASym :: String
implASym  = "->"
equivASym = "<->"
andASym   = "/\\"
orASym    = "||"
notASym   = "~"
   
-----------------------------------------------------------
--- Unicode symbols

unicodeSyms :: [String]
unicodeSyms = [implUSym, equivUSym, andUSym, orUSym, notUSym]

unicodeTuple = (implUSym, equivUSym, andUSym, orUSym, notUSym, "T", "F")

implUSym, equivUSym, andUSym, orUSym, notUSym :: String
implUSym  = "\8594"
equivUSym = "\8596"
andUSym   = "\8743"
orUSym    = "\8744"
notUSym   = "\172"