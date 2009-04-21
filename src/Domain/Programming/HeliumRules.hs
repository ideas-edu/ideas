module Domain.Programming.HeliumRules where

import Common.Context
--import Common.Uniplate
import Common.Transformation
import Common.Uniplate
import Common.Apply
import Domain.Programming.Helium
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad


introModule :: Rule Module
introModule = minorRule $ makeSimpleRule "Intro module" f
  where 
    f _ = return $ 
            Module_Module 
              noRange                           -- Range
              MaybeName_Nothing                 -- Module name
              MaybeExports_Nothing              -- Exports
              ( Body_Body 
                noRange               -- Body, range
                []                    -- Imports
                [ Declaration_PatternBinding    -- Declarations
                  noRange                                  -- range
                  ( Pattern_Variable                       -- Pattern variable
                    noRange                                                                                                   ( Name_Identifier noRange [] "f" )   -- name
                  )
                  ( RightHandSide_Expression 
                    noRange
                    undef         -- expression
                    MaybeDeclarations_Nothing       -- declarations
                  ) 
                ]
              )


--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

introNormalApplication :: Int -> Rule Module
introNormalApplication = (toRule "Introduce application") . normalApplication

normalApplication :: Int -> Expression -> Maybe Expression
normalApplication nargs e = case e of
  undef -> return $ Expression_NormalApplication 
                      noRange
                      undef
                      (take nargs (repeat undef))
  _ -> Nothing

introInfixApplication :: Rule Module
introInfixApplication = toRule "Introduce operator" f
  where 
    f e = case e of 
            undef -> return $ Expression_InfixApplication 
                                noRange
                                MaybeExpression_Nothing -- need to do something with this... maybe undef...
                                undef                   
                                MaybeExpression_Nothing 
            _     -> Nothing


-- Variables
introIdentifier :: String -> Rule Module
introIdentifier name = toRule "Intro identifier" f
  where
    f e | e == undef = return $ Expression_Variable 
                                  noRange 
                                  (Name_Identifier noRange [] name)
    f _ = Nothing

introOperator :: String -> Rule Module
introOperator name = toRule "Intro operator" f
  where
    f e | e == undef = return $ Expression_Variable 
                                  noRange 
                                  (Name_Operator noRange [] name)
    f _ = Nothing


-- Literals
introInt :: String -> Rule Module
introInt i = toRule "Intro int" f
  where
    f e | e == undef = return $ Expression_Literal noRange $ Literal_Int noRange i
    f _ = Nothing

introString :: String -> Rule Module
introString s = toRule "Intro literal" f
  where
    f e | e == undef = return $ Expression_Literal noRange $ Literal_String noRange s
    f _ = Nothing


--------------------------------------------------------------------------------
-- Lifting expressions to declarations to modules
--------------------------------------------------------------------------------

-- use GP (EMGM) for this
liftDecl :: (Declaration -> Maybe Declaration) -> Module -> Maybe Module
liftDecl f (Module_Module mrange mname exps (Body_Body brange imps (d:ds))) = do 
  d' <- f d
  return (Module_Module mrange mname exps (Body_Body brange imps (d':ds)))
liftDecl _ _ = Nothing

-- Needs to be extended with all decl constructors
liftExpression :: (Expression -> Maybe Expression) -> Declaration -> Maybe Declaration
liftExpression f d = case d of
  (Declaration_FunctionBindings rd bs)  -> undefined
  (Declaration_PatternBinding rd p rhs) -> case rhs of
    (RightHandSide_Expression re expr w) -> f expr >>= \ expr' -> return
                                             (Declaration_PatternBinding rd p (RightHandSide_Expression re expr' w))
    (RightHandSide_Guarded rg gexpr w) -> undefined


--------------------------------------------------------------------------------
-- Help functions
--------------------------------------------------------------------------------

toRule :: String -> (Expression -> Maybe Expression) -> Rule Module
toRule s f = makeSimpleRule s $ liftDecl $ liftExpression f


{-
getRules :: Module -> [Rule (Context Module)]
getRules expr = 
   case expr of
      Lambda x e -> introLambda x : getRules e
      MatchList b n c -> introMatchList : getRules b ++ getRules n ++ getRules c
      Var x -> introVar x : []
      -- Let x b d -> introLet x : getRules b ++ getRules d
      Apply (Lambda f b) (Fix (Lambda g e)) | f==g -> introLet f : getRules b ++ getRules e
      Fix (Lambda x e) -> getRules (makeLet x e (Var "x"))
      Apply f a -> introApply : getRules f ++ getRules a
      IfThenElse c t e -> introIf : getRules c ++ getRules t ++ getRules e
      _ -> error (show expr)
-}


--buildModule :: [Rule (Context Module)] -> Module
--buildModule = fromContext . foldl (flip applyD) (inContext undef)

--applyRule :: Module -> (Module -> Maybe Module) -> Maybe Module
--applyRule e f = somewhereM f e

{-
focusUndef :: Rule (Context Module)
focusUndef = minorRule $ makeSimpleRule "Focus on undefined" f
 where
   f :: Context Module -> Maybe (Context Module)
   f ce = case findUndefined (fromContext ce) of
             is:_ -> return (setLocation (makeLocation is) ce)
             _    -> Nothing

focusTop :: Rule (Context Module)
focusTop = minorRule $ makeSimpleRule "Focus Top" f
 where
   f :: Context Module -> Maybe (Context Module)
   f = return . setLocation (makeLocation [])

findUndefined :: Module -> [[Int]]
findUndefined e
   | e == undef = [[]]
   | otherwise  = [ i:is | (i, c) <- zip [0..] (children e), is <- findUndefined c ]

-}