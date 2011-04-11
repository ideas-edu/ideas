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
module Domain.RelationAlgebra.Parser (parseRelAlg, ppRelAlg) where

import Control.Arrow
import Control.Monad
import Domain.RelationAlgebra.Formula
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
   
-----------------------------------------------------------
--- Parser

parseRelAlg  :: String -> Either String RelAlg
parseRelAlg = parseWith relalg
 where
   relalg = buildExpressionParser table term
   
   term = liftM2 (foldl (flip ($))) atom (many pUn)
   
   pUn = choice 
      [ reservedOp "~" >> return Inv
      , reservedOp "-" >> return Not
      ]
   
   atom = choice
      [ P.reserved lexer "V" >> return V
      , P.reserved lexer "E" >> return empty
      , P.reserved lexer "I" >> return I
      , liftM Var (P.identifier lexer)
      , P.parens lexer relalg
      ]
   
   table = 
      [ [ Infix (reservedOp ";" >> return (:.:)) AssocNone
        , Infix (reservedOp "!" >> return (:+:)) AssocNone
        ]
      , [ Infix (reservedOp "/\\" >> return (:&&:)) AssocRight ]
      , [ Infix (reservedOp "\\/" >> return (:||:)) AssocRight ]
      ]

-----------------------------------------------------------
--- Lexer

lexer :: P.TokenParser a
lexer = P.makeTokenParser $ emptyDef 
   { reservedNames   = ["V", "E", "I"]
   , reservedOpNames = ["~", "-", ";", "!", "\\/", "/\\"]
   , identStart      = letter
   , identLetter     = letter
   , opStart         = fail "" 
   , opLetter        = fail ""
   }

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

parseWith :: Parser a -> String -> Either String a
parseWith p = left show . runParser start () ""
 where
   start = (P.whiteSpace lexer) >> p >>= \a -> eof >> return a
                  
-----------------------------------------------------------
--- Pretty-Printer

ppRelAlg :: RelAlg -> String
ppRelAlg = ppRelAlgPrio (0, "")

ppRelAlgPrio :: (Int, String) -> RelAlg -> String 
ppRelAlgPrio = (\f n -> f n "") . flip (foldRelAlg alg)
 where
   alg = (var, binop 4 ";", binop 4 "!", binop 3 "/\\", binop 2 "\\/"
         , nott, inv, var "V", var "I"
         ) 
   binop prio op p q (n, parent) = 
      parIf (n > prio || (prio==4 && n==4 && op/=parent)) (p (prio+1, op) . ((" "++op++" ")++) . q (prio, op))
   var       = const . (++)
   nott p _  = p (6, "") . ("-"++) 
   inv  p _  = p (6, "") . ("~"++)
   parIf b f = if b then ("("++) . f . (")"++) else f