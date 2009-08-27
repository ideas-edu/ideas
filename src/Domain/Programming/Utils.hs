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
import Data.Maybe

pat2expr :: Pattern -> Maybe Expression
pat2expr p = 
  case p of 
    Pattern_Literal r l                -> Just $ Expression_Literal r l
    Pattern_Variable r n               -> Just $ Expression_Variable r n
    Pattern_Constructor r n _          -> Just $ Expression_Constructor r n
    Pattern_Parenthesized r p          -> do e <- pat2expr p
                                             return $ Expression_Parenthesized r e
    Pattern_InfixConstructor rg l op r -> do l' <- pat2expr l
                                             r' <- pat2expr r
                                             return $ Expression_InfixApplication rg
                                                        (MaybeExpression_Just l')
                                                        (var op)
                                                        (MaybeExpression_Just r')
    Pattern_List r ps                  -> do exprs <- mapM pat2expr ps
                                             return $ Expression_List r exprs
    _                                  -> Nothing

expr2pat e = 
  case e of
    Expression_Literal r l                -> Just $ Pattern_Literal r l
    Expression_Variable r n               -> Just $ Pattern_Variable r n
    Expression_Constructor r n            -> Just $ Pattern_Constructor r n []
    Expression_Parenthesized r e          -> do p <- expr2pat e
                                                return $ Pattern_Parenthesized r p
    Expression_List r es                  -> do ps <- mapM expr2pat es
                                                return $ Pattern_List r ps
    _                                     -> Nothing
         
name2string (Name_Identifier _ _ n) = n
name = Name_Identifier noRange []
var = Expression_Variable noRange
pat = Pattern_Variable noRange
patBinding pat expr w = Declaration_PatternBinding noRange pat $ 
                          RightHandSide_Expression noRange expr w
lambda ps expr = Expression_Lambda noRange ps expr
letItBe ds expr = Expression_Let noRange ds expr

pp = putStrLn . ppModule
compExercise = (\(Right m)->m) . compile . fst

fromMaybeDecl :: MaybeDeclarations -> Declarations
fromMaybeDecl m = case m of
                    MaybeDeclarations_Just ds -> ds
                    _                         -> []

toplevelDecls (Module_Module _ _ _ (Body_Body _ _ ds)) = ds
