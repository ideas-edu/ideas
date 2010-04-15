-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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
   , ppLogicPars, ppLogicUnicodePars
   ) where

import Common.Utils (ShowString(..))
import Text.Parsing
import Control.Arrow
import Domain.Logic.Formula
   
logicScanner :: Scanner
logicScanner = (specialSymbols "~" defaultScanner)
   { keywords         = ["T", "F"]
   , keywordOperators = "~" : concatMap (map fst . snd) operatorTable
   }

logicUnicodeScanner :: Scanner
logicUnicodeScanner = (specialSymbols (concat unicodeSyms) defaultScanner)
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
parseLogic :: String -> Either String SLogic
parseLogic = analyseAndParse pLogic . scanWith logicScanner
 where
   pLogic = pOperators operatorTable (basicWithPos pLogic)
   
-- | Parser for logic formulas that insists on more parentheses: "and" and "or" are associative, 
-- | but implication and equivalence are not. Priorities of the operators are unknown, and thus 
-- | parentheses have to be written explicitly. No parentheses are needed for Not (Not p). Superfluous
-- | parentheses are permitted
parseLogicPars :: String -> Either String SLogic
parseLogicPars s
   = either Left suspiciousVariable 
   $ left (ambiguousOperators parseLogic s)
   $ analyseAndParse (pLogicGen asciiTuple)
   $ scanWith logicScanner s

parseLogicUnicodePars :: String -> Either String SLogic
parseLogicUnicodePars s 
   = either Left suspiciousVariable 
   $ left (ambiguousOperators (parseLogic . concatMap f) s)
   $ analyseAndParse (pLogicGen unicodeTuple)
   $ scanWith logicUnicodeScanner s
 where
   -- quick fix (since we only need to know whether the parser succeeds)
   f c | [c] == andUSym   = andASym
       | [c] == orUSym    = orASym
       | [c] == notUSym   = notASym
       | [c] == implUSym  = implASym
       | [c] == equivUSym = equivASym
       | otherwise        = [c]

pLogicGen (impl, equiv, and, or, nt, tr, fl) = pLogic
 where
   pLogic = flip ($) <$> basic <*> optional composed id
   basic     =  basicWithPosGen (nt, tr, fl) pLogic
   composed  =  flip (:<->:) <$ pKey equiv <*> basic
            <|> flip (:->:)  <$ pKey impl  <*> basic
            <|> (\xs p -> foldr1 (:&&:) (p:xs)) <$> pList1 (pKey and *> basic)
            <|> (\xs p -> foldr1 (:||:) (p:xs)) <$> pList1 (pKey or  *> basic)
 
basicWithPos :: Parser Token SLogic -> Parser Token SLogic
basicWithPos = basicWithPosGen ("~", "T", "F")

basicWithPosGen t@(nt, tr, fl) p = 
       (Var . ShowString) <$> pVarid
   <|> pParens p
   <|> T  <$ pKey tr
   <|> F  <$ pKey fl
   <|> Not <$ pKey nt <*> basicWithPosGen t p

-----------------------------------------------------------
--- Helper-functions for syntax warnings

-- analyze parentheses
analyseAndParse :: Parser Token a -> [Token] -> Either String a
analyseAndParse p ts =
   case checkParentheses ts of
      Just err -> Left (show err)
      Nothing  -> either (Left . f) Right (parse p ts)
 where
   f (Just t) = show (tokenPosition t) ++ ": Unexpected " ++ show t
   f Nothing  = "Syntax error"

ambiguousOperators :: (String -> Either a b) -> String -> String -> String
ambiguousOperators p s err =
   let msg = "Ambiguous use of operators (write parentheses)"
   in either (const err) (const msg) (p s)

-- Report variables 
suspiciousVariable :: SLogic -> Either String SLogic
suspiciousVariable r =
   case filter p (map fromShowString (varsLogic r)) of
      v:_ -> Left $ "Unexpected variable " ++ v
                 ++ ". Did you forget an operator?" 
      _   -> Right r
 where
   p xs = length xs > 1 && all (`elem` "pqrst") xs

-----------------------------------------------------------
--- Pretty-Printer

-- | Pretty printer that produces extra parentheses: also see parseLogicPars
ppLogicPars :: SLogic -> String
ppLogicPars = ppLogicParsGen asciiTuple

-- | Pretty printer with unicode characters
ppLogicUnicodePars :: SLogic -> String
ppLogicUnicodePars = ppLogicParsGen unicodeTuple

ppLogicParsGen (impl, equiv, and, or, nt, tr, fl) p = foldLogic alg p 0 ""
 where
   alg = (pp . fromShowString, binop 3 impl, binop 3 equiv, binop 1 and, binop 2 or, nott, pp tr, pp fl)
   binop prio op p q n = parIf (n/=0 && (n==3 || prio/=n)) 
                               (p prio . ((" "++op++" ")++) . q prio)
   pp s = const (s++)
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