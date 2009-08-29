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

import Control.Monad
import Domain.Programming.Helium
import Domain.Programming.HeliumRules
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
    Pattern_Wildcard r                 -> Just $ Expression_Variable r $ name "_"
    _                                  -> Nothing

expr2pat :: Expression -> Maybe Pattern
expr2pat e = 
  case e of
    Expression_Literal r l                -> Just $ Pattern_Literal r l
    Expression_Variable r n               -> Just $ if n == name "_" 
                                                    then Pattern_Wildcard r
                                                    else Pattern_Variable r n
    Expression_Constructor r n            -> Just $ Pattern_Constructor r n []
    Expression_Parenthesized r e          -> do p <- expr2pat e
                                                return $ Pattern_Parenthesized r p
    Expression_InfixApplication rg ml op mr -> do l <- expr2pat =<< fromMaybeExpr ml
                                                  r <- expr2pat =<< fromMaybeExpr mr
                                                  return $ Pattern_InfixConstructor rg l
                                                           (getName op) r
    Expression_List r es                  -> do ps <- mapM expr2pat es
                                                return $ Pattern_List r ps
    _                                     -> Nothing

fromMaybeExpr :: MaybeExpression -> Maybe Expression
fromMaybeExpr mexpr = 
  case mexpr of
    MaybeExpression_Just expr -> Just expr
    _                         -> Nothing

class GetName a where
  getName :: a -> Name
instance GetName Expression where
  getName expr = case expr of
                   Expression_Variable _ n -> n
                   Expression_Constructor _ n -> n
                   _ -> error $ "No constructor instancse for: " ++ show expr
instance GetName Pattern where
  getName pat = case pat of
                  Pattern_Variable _ n -> n
                  Pattern_Constructor _ n _ -> n
                  _ -> error $ "No constructor instancse for: " ++ show pat

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

fromMaybeDecl :: MaybeDeclarations -> Maybe Declarations
fromMaybeDecl m = 
  case m of
    MaybeDeclarations_Just ds -> Just ds
    _                         -> Nothing

toplevelDecls :: Module -> Declarations
toplevelDecls (Module_Module _ _ _ (Body_Body _ _ ds)) = ds

-- You can't conjure up an a out of the blue... of course ;-) It suits me nonetheless.
class MonadPlus m => MonadMul m where
  mone :: a -> m a
  mmul :: m a -> m a -> m a
instance MonadMul Maybe where
  mone = Just
  mmul (Just x) (Just y) = Just x
  mmul _ _               = Nothing

mprod :: MonadMul m => a -> [m a] -> m a
mprod a = foldr mmul (mone a)

mprod1 :: MonadMul m => [m a] -> m a
mprod1 = foldr1 mmul

funName :: FunctionBinding -> Name
funName = (\(n, _, _, _) -> n) . deconstrucFunBinding

deconstrucFunBinding :: FunctionBinding -> (Name, Patterns, Expression, MaybeDeclarations)
deconstrucFunBinding (FunctionBinding_FunctionBinding _ lhs rhs) = (name, ps, expr, ds)
  where
    deLHS l = case l of 
                LeftHandSide_Function _ n ps -> (n, ps)
                LeftHandSide_Infix _ l op r   -> (op, [l, r])
                LeftHandSide_Parenthesized _ lhs' ps -> let (n, ps') = deLHS lhs' 
                                                        in (n, ps' ++ ps)
    (name, ps) = deLHS lhs
    (expr, ds) = case rhs of
                    RightHandSide_Expression _ expr w -> (expr, w)
                    RightHandSide_Guarded _ gexpr w   -> error "Todo: guarded expressions"


