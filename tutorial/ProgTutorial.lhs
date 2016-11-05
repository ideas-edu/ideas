
<div class="page-header"> 
<div class="ideas-logo"><img src="ideas.png"/></div>
<div class="ounl-logo"><img src="ounl.png"/></div>
&nbsp; Ideas tutorial (version ?.?)
</div>
<div class="page-content">

Making a domain reasoner for programming
========================================

This tutorial shows a number of applications of the Ideas framework, demonstrated
by a domain reasoner for a programming tutor. 

We define a new module and specify a number of inputs that we need. 

> {-# LANGUAGE DeriveDataTypeable #-}
>
> module ProgTutorial where
> 
> import Ideas.Common.Library
> import Ideas.Main.Default
> import qualified Ideas.Service.State as Ideas
> import Ideas.Service.BasicServices as BS
> import Ideas.Service.Diagnose
> import Data.Generics.Uniplate.DataOnly (transformBi)
> import Data.Data
> import Data.Maybe
> import Control.Monad.State
> import Text.PrettyPrint.Leijen as P

Defining the domain
-------------------

We start with defining data types for a simple imperative programming language
that supports conditional statements (If), loops (While), assignments and
sequences of statements. The language supports integers and booleans, and a
number of unary and binary operations.

> data Program = Program [Stat] deriving (Data, Typeable, Eq, Read)
> 
> data Stat = 
>        If       Expr Stat
>    |   While    Expr Stat
>    |   Assign   String Expr
>    |   Seq      [Stat]
>    deriving (Data, Typeable, Eq, Read)
> 
> data Expr = 
>        Op       Expr BinOp Expr
>    |   LitExpr  Literal 
>    |   Not      Expr
>    |   Hole     Int
>    |   Ident    String
>    deriving (Data, Typeable, Eq, Read)
> 
> data BinOp = Add | Mul | And | Or | Eqs deriving (Data, Typeable, Eq, Read)
> 
> data Literal = 
>        IntLit   Int 
>    |   BoolLit  Bool 
>    deriving (Data, Typeable, Eq, Read)

To show code in a nice way we provide an instance of the
<a href="http://hackage.haskell.org/package/wl-pprint">Pretty</a> class for each
data type. 

> instance Pretty BinOp where
>     pretty Add    = text "+"
>     pretty Mul    = text "*" 
>     pretty And    = text "AND" 
>     pretty Or     = text "OR" 
>     pretty Eqs    = text "=="
>
> instance Pretty Literal where
>     pretty (IntLit i)  = int i
>     pretty (BoolLit b) = text (if b then "true" else "false")
>
> instance Pretty Expr where
>     pretty (Op x op y)  = pretty x P.<+> pretty op P.<+> pretty y
>     pretty (LitExpr l)  = pretty l
>     pretty (Not x)      = text "!" P.<+> pretty x 
>     pretty (Hole i)     = text "?"  -- P.<+> pretty i
>     pretty (Ident s)   = pretty s
>
> instance Pretty Stat where
>     pretty (If e s)     = text "if" P.<+> pretty e P.<$> indent 4 (pretty s)
>     pretty (While e s)  = text "while" P.<+> pretty e P.<$> indent 4 (pretty s)
>     pretty (Assign i e) = pretty i P.<+> text "=" P.<+> pretty e P.<> semi
>     pretty (Seq stats)  = vsep (map pretty stats)
>
> instance Pretty Program where
>     pretty (Program stats) = vsep (map pretty stats)

We use the <a href="http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm">Uniplate
generics library</a> and derive instances for each data type.

> {-!
> deriving instance UniplateDirect Expr
> deriving instance UniplateDirect Program
> deriving instance UniplateDirect Stat
> deriving instance UniplateDirect Literal
> !-}

We define some expressions and statements and create a model program that is the sequence of
these statements.

> stat1, stat2, stat3, stat4 :: Stat
> stat1 = Assign "x" (LitExpr (IntLit 5))
> stat2 = Assign "y" (LitExpr (IntLit 1))
> stat3 = If cond1 (Seq [stat1, stat2])
> stat4 = While cond2 stat3
>
> cond1, cond2, exp1, expSimple :: Expr
> cond1     = Op (Ident "b") Eqs (LitExpr (BoolLit True))
> cond2     = Op (Ident "i") Eqs (LitExpr (IntLit 10))
> exp1      = Op (Op (Not (Ident "p")) And (Not (Ident "q"))) Eqs (LitExpr (BoolLit True))
> expSimple  = Op (Ident "p") And (Ident "q")
> 
> model1 :: Program 
> model1 = Program [stat1, stat3, stat4, stat1]

Printing this model shows the following code:

~~~~{.java}
Main> pretty model1
x = 5;
if b == true
    x = 5;
    y = 1;
while i == 10
    if b == True
        x = 5;
        y = 1;
x = 5;
~~~~

Creating rules to build a program
---------------------------------

A simple way to construct a program step by step is to append new statements
to a program, starting with an empty program.

We need a rule that can append a statement at the end of a program.

> appendStat :: Stat -> Rule Program
> appendStat s = describe ("Append a statement: " ++ show (pretty s)) $ makeRule "p.append" (f s)
>    where
>       f :: Stat -> Program -> Maybe Program
>       f s (Program stats) = Just $ Program (stats ++ [s])

We need a model program to know what statements need to be appended. We dynamically generate
the strategy with the model program as input. We use sequenceS, which is an alias for the
sequence strategy combinator. Note that using the sequence combinator means that we do not allow
adding statements in a different order, even though this might be allowed for some statements.

> makeStrategy :: Program -> LabeledStrategy Program
> makeStrategy (Program stats) = label "Example exercise" $ sequenceS (map appendStat stats)

We can now define programming exercises. 

> makeProgExercise :: Program -> Exercise Program
> makeProgExercise model = (progExercise "") { strategy = liftToContext (makeStrategy model)}

> progExercise :: String -> Exercise Program
> progExercise exDesc = emptyExercise
>     { exerciseId    = describe exDesc $ newId "p.ex"
>     , status        = Experimental
>     , prettyPrinter = show . pretty
>     , parser        = readM
>     , similarity    = withoutContext (==)
>     , equivalence   = withoutContext (==)
>     --, ready         = predicate isCon
>     --, examples      = level Easy [expr1] ++ level Medium [expr2]
>     }

> refineExercise :: String -> Exercise Expr
> refineExercise exDesc = emptyExercise
>     { exerciseId    = describe exDesc $ newId "p.r.ex"
>     , status        = Experimental
>     , prettyPrinter = show . pretty
>     , parser        = readM
>     , similarity    = withoutContext (==)
>     , equivalence   = withoutContext (<==>)
>     --, ready         = predicate isCon
>     --, examples      = level Easy [expr1] ++ level Medium [expr2]
>     }

> (<==>) :: Expr -> Expr -> Bool
> (Hole _) <==> _ = True
> _ <==> (Hole _)  = True
> x <==> y  = x == y   

We can test this by printing the steps for building the model program.

< Main > printDerivation (makeProgExercise model1) (Program []) 
<    => p.append
< x = 5;

<    => p.append
< x = 5;
< if True
<    x = 5;

<    => p.append
< x = 5;
< if True
<    x = 5;
< while False
<    if True
<    x = 5;

Note that *nested* statements are not built up step by step in this simple example.

Refining expressions in a program
---------------------------------

The Hole constructor of the Expr data type can be used as a placeholder for
expressions. The constructor contains an integer that uniquely identifies it.
A Hole (represented by a question mark) can be used by students
if they do not know how to define the necessary expression yet. 

We need a refine rule that can replace an Hole by an expression. 

> -- refineExpr :: Expr -> Int -> Rule Program
> refineExpr expr i = describe ("refine expression " ++ show (pretty expr)) $ makeRule ruleId (f expr i)
>    where
>       --f :: Expr -> Int -> Program -> Maybe Program
>       f expr i p = Just $ transformBi refine' p
> 
>       refine' e
>          | holeId e == Just i = expr 
>          | otherwise             = e
>
>       ruleId = "p.refine" ++ show i

The following helper function is needed to find the question mark that needs
to be replaced.

> holeId :: Expr -> Maybe Int
> holeId (Hole i)   = Just i
> holeId _          = Nothing

During the generation of the strategy we need to number the Hole that are
encountered. We use the State monad to keep track of the next number
we may assign. 

> type StrategyGenerator a = Int -> a -> State Int (LabeledStrategy Program)

For the main data types of our domain (Program, Stat and Expr) we define an
instance of GenStrategy.

> type GenState a = State Int a
> class GenStrategy a where
>     genStrat :: StrategyGenerator a

> instance GenStrategy Program where
>    genStrat i (Program stats) = do
>       s <- mapM (genStrat i) stats
>       return $ label "todo" $ sequenceS s

If a statement contains an expression, we first need to get the next available number.
The first step of the strategy for this statement is to introduce the question mark.
In the next step the question mark can be replaced by the actual expression.

> instance GenStrategy Stat where
>   genStrat loc (Assign i e) = do
>       newId <- getNextNr
>       return $ label "" $ appendStat (Assign i (Hole newId)) .*. refineExpr e newId
>   genStrat loc stat = return $ label "" $ appendStat stat

> --instance GenStrategy Expr where
> genStratE :: Data a => Int -> Expr -> State Int (LabeledStrategy a)
> genStratE loc expr = case expr of
>     Not e -> do
>         (newId, s) <- idAndS e
>         return $ label "" $ refineExpr (Not (Hole newId)) loc .*. s
>     Op e1 op e2 -> do
>         (newId1, s1) <- idAndS e1
>         (newId2, s2) <- idAndS e2
>         return $ label "" $ refineExpr (Op (Hole newId1) op (Hole newId2)) loc .*. label "refine left or right" (s1 .%. s2)
>     _ -> do
>         return $ label "" $ refineExpr expr loc
>     where
>         idAndS expr = do    
>             newId <- getNextNr
>             s <- genStratE newId expr
>             return (newId, s)

> getNextNr :: State Int Int
> getNextNr = do
>     i <- get
>     put (i+1)
>     return i

The other statements, such as If and While, and some expressions also contain expressions.

__Exercise__: Expand the genStrat instance for Stat.

__Exercise__: Implement the genStrat instance for Expr.

> makeProgExercise2 :: Program -> Exercise Program
> makeProgExercise2 model = (progExercise "") { strategy = liftToContext (evalState (genStrat 0 model) 0)}

> makeRefineEx :: Expr -> Exercise Expr
> makeRefineEx model = (refineExercise "refine") 
>     { strategy = liftToContext (evalState (genStratE (-1) model) 0)}

> startExpr :: Expr
> startExpr = Hole (-1)

Generating hints
----------------

> emptyProgram :: Program
> emptyProgram = Program []

> askHint :: Exercise a -> Ideas.State a -> String
> askHint ex state = case BS.onefirst state of
>     (Left errM)                     -> errM
>     Right ((rule, loc, env), state) -> show (description rule)

> startExercise :: Exercise Program -> Ideas.State Program
> startExercise ex = Ideas.emptyState ex emptyProgram

> checkProgress :: Exercise a -> Ideas.State a -> a -> Diagnosis a
> checkProgress ex state p = diagnose state (inContext ex p) Nothing
       
> doRefineEx :: IO ()
> doRefineEx = do
>     let ex    = makeRefineEx expSimple
>         state = Ideas.emptyState ex startExpr
>     exLoop ex state
>     where
>         exLoop ex state = do
>             input <- getLine
>             case input of 
>                 "q" -> return ()
>                 _   -> do
>                    let diag = checkProgress ex state (read input :: Expr)
>                    print diag
>                    if isJust (newState' diag) then do
>                        let newState = fromJust $ newState' diag
>                            hint     = askHint ex newState    
>                        putStrLn $ "Hint " ++ hint
>                        exLoop ex newState
>                    else do
>                        putStrLn "Try again"
>                        exLoop ex state


> newState' :: Diagnosis a -> Maybe (Ideas.State a)
> newState' diagnosis =
>    case diagnosis of
>       Buggy _ _        -> Nothing
>       NotEquivalent _  -> Nothing
>       Similar  _ s     -> Just s
>       WrongRule _ s _  -> Just s
>       Expected _ s _   -> Just s
>       Detour   _ s _ _ -> Just s
>       Correct  _ s     -> Just s
>       Unknown  _ s     -> Just s