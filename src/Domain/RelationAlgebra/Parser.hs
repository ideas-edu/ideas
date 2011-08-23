-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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

import Domain.RelationAlgebra.Formula
import Text.Parsing
import qualified Text.ParserCombinators.Parsec.Token as P

-----------------------------------------------------------
--- Parser

parseRelAlg  :: String -> Either String RelAlg
parseRelAlg = parseSimple relalg
 where
   relalg = buildExpressionParser table term

   term = foldl (flip ($)) <$> atom <*> many pUn

   pUn = choice
      [ Inv <$ reservedOp "~"
      , Not <$ reservedOp "-"
      ]

   atom = choice
      [ V     <$  P.reserved lexer "V"
      , empty <$  P.reserved lexer "E"
      , I     <$  P.reserved lexer "I"
      , Var   <$> P.identifier lexer
      , P.parens lexer relalg
      ]

   table =
      [ [ Infix ((:.:) <$ reservedOp ";") AssocRight -- or none-associative?
        , Infix ((:+:) <$ reservedOp "!") AssocRight -- or none-associative?
        ]
      , [ Infix ((:&&:) <$ reservedOp "/\\") AssocRight ]
      , [ Infix ((:||:) <$ reservedOp "\\/") AssocRight ]
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