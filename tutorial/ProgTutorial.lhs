
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
> import Data.Generics.Uniplate.DataOnly (transformBi)
> import Data.Data
> import Control.Monad.State
> import Text.PrettyPrint.Leijen as P

Defining the domain
-------------------

We start with defining data types for a simple imperative programming language
that supports a conditional statements (If), loops (While), assignments and
sequences of statements. The language supports integers and booleans, and a
number of unary and binary operations.

> data Program = Program [Stat] deriving (Data, Typeable, Eq, Read)
> 
> data Stat = 
>        If       Expr Stat
>    |   While    Expr Stat
>    |   Assign   Ident Expr
>    |   Seq      [Stat]
>    deriving (Data, Typeable, Eq, Read)
> 
> data Expr = 
>        Op       Expr BinOp Expr
>    |   LitExpr  Literal 
>    |   Not      Expr
>    |   Unknown  Int
>    |   IdExpr   Ident
>    deriving (Data, Typeable, Eq, Read)
> 
> data BinOp = Add | Mult | And | Or | Eqs deriving (Data, Typeable, Eq, Read)
>    
> data Ident = Ident String deriving (Data, Typeable, Eq, Read)
> 
> data Literal = 
>     IntLit   Int 
>  |  BoolLit  Bool 
>  deriving (Data, Typeable, Eq, Read)

To show code in a nice way we provide an instance of the
<a href="http://hackage.haskell.org/package/wl-pprint">Pretty</a> class for each
data type. 

> instance Pretty BinOp where
>     pretty Add    = text "+"
>     pretty Mult   = text "*" 
>     pretty And    = text "AND" 
>     pretty Or     = text "OR" 
>     pretty Eqs    = text "=="
>
> instance Pretty Ident where
>     pretty (Ident s) = text s
>
> instance Pretty Literal where
>     pretty (IntLit i)  = int i
>     pretty (BoolLit b) = text (if b then "true" else "false")
>
> instance Pretty Expr where
>     pretty (Op x op y)  = pretty x P.<+> pretty op P.<+> pretty y
>     pretty (LitExpr l)  = pretty l
>     pretty (Not x)      = text "!" P.<+> pretty x 
>     pretty (Unknown i)  = text "?" P.<+> pretty i
>     pretty (IdExpr i)   = pretty i
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
> deriving instance UniplateDirect Ident
> deriving instance UniplateDirect Literal
> !-}

We define some expressions and statements and create a model program that is the sequence of
these statements.

> stat1, stat2, stat3, stat4 :: Stat
> stat1 = Assign (Ident "x") (LitExpr (IntLit 5))
> stat2 = Assign (Ident "y") (LitExpr (IntLit 1))
> stat3 = If cond1 (Seq [stat1, stat2])
> stat4 = While cond2 stat3
>
> cond1, cond2 :: Expr
> cond1 = Op (IdExpr (Ident "b")) Eqs (LitExpr (BoolLit True))
> cond2 = Op (IdExpr (Ident "i")) Eqs (LitExpr (IntLit 10))
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
> appendStat s = describe "Append a statement" $ makeRule "p.append" (f s)
>    where
>       f :: Stat -> Program -> Maybe Program
>       f s (Program stats) = Just $ Program (stats ++ [s])

We need a model program to know what statements need to be appended. We dynamically generate
the strategy with the model program as input.

> makeStrategy :: Program -> LabeledStrategy Program
> makeStrategy (Program stats) = label "Example ex" $ sequenceS (map appendStat stats)

We can now define programming exercises. 

> makeProgExercise :: Program -> Exercise Program
> makeProgExercise model = progExercise { strategy = liftToContext (makeStrategy model)}

> progExercise :: Exercise Program
> progExercise = emptyExercise
>   { exerciseId    = describe "todo" $
>                        newId "p.ex1"
>   , status        = Experimental
>   --, strategy      = liftToContext (makeStrategy model1)
>   , prettyPrinter = show . pretty
>   , parser        = readM
>   --, equivalence   = withoutContext eqExpr
>   --, ready         = predicate isCon
>   --, examples      = level Easy [expr1] ++ level Medium [expr2]
>   }

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

Refining expressions in a program
---------------------------------

The Unknown constructor of the Expr data type can be used as a placeholder for
expressions. The constructors contains an integer that uniquely identifies it.
An Unknown (represented by a question mark) can be used by students
if they do not know how to define the necessary expression yet. 

We need a refine rule that can replace an Unknown by an expression. 

> refineExpr :: Expr -> Int -> Rule Program
> refineExpr expr i = describe "todo" $ makeRule "p.refine" (f expr i)
>    where
>       f :: Expr -> Int -> Program -> Maybe Program
>       f expr i p = Just $ transformBi refine' p
> 
>       refine' e
>          | unknownId e == Just i = expr 
>          | otherwise             = e

The following helper function is needed to find the question mark that needs
to be replaced.

> unknownId :: Expr -> Maybe Int
> unknownId (Unknown i)   = Just i
> unknownId _             = Nothing

During the generation of the strategy we need to number the Unknown that are
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
>       return $ label "" $ appendStat (Assign i (Unknown newId)) .*. refineExpr e newId
>   genStrat loc stat = return $ label "" $ appendStat stat

> getNextNr :: State Int Int
> getNextNr = do
>     i <- get
>     put (i+1)
>     return i

..

> makeProgExercise2 :: Program -> Exercise Program
> makeProgExercise2 model = progExercise { strategy = liftToContext (evalState (genStrat 0 model) 0)}

Todo
* other statements with expressions
* expressions in expressions

