-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  unknown
--
--
-----------------------------------------------------------------------------

module Domain.Programming.Utils where

import Domain.Programming.Helium

pat2expr :: Pattern -> Expression
pat2expr p = 
  case p of 
    Pattern_Literal r l                -> Expression_Literal r l
    Pattern_Variable r n               -> Expression_Variable r n
    Pattern_Constructor r n _          -> Expression_Constructor r n
    Pattern_Parenthesized r p          -> Expression_Parenthesized r $ pat2expr p
    Pattern_InfixConstructor r' l op r -> Expression_InfixApplication r'
                                            (MaybeExpression_Just (pat2expr l))
                                            (var op)
                                            (MaybeExpression_Just (pat2expr r))
    Pattern_List r ps                  -> Expression_List r $ map pat2expr ps

expr2pat e = 
  case e of
    Expression_Literal r l                -> Pattern_Literal r l
    Expression_Variable r n               -> Pattern_Variable r n
    Expression_Constructor r n            -> Pattern_Constructor r n []
    Expression_Parenthesized r e          -> Pattern_Parenthesized r $ expr2pat e
--    Expression_InfixApplication r' l op r -> Pattern_InfixApplication r'
--                                               (MaybeExpression_Just (pat2expr l))
--                                               ( op)
--                                               (MaybeExpression_Just (pat2expr r))
    Expression_List r es                  -> Pattern_List r $ map expr2pat es
         

var = Expression_Variable noRange
pat = Pattern_Variable noRange
patBinding pat expr w = Declaration_PatternBinding noRange pat $ 
                          RightHandSide_Expression noRange expr w
lambda ps expr = Expression_Lambda noRange ps expr
letItBe ds expr = Expression_Let noRange ds expr

pp = putStrLn . ppModule
comp = (\(Right m)->m) . compile . fst