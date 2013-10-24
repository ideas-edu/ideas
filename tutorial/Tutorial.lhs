Ideas tutorial (part 1)
=======================

This tutorial shows how to make a simple domain reasoner with the Ideas framework.
We start by defining a minimal exercise and show how this can be compiled into an 
application that can handle feedback requests. Make sure you have installed a 
Haskell compiler and the cabal package manager (see Haskell Platform). Get the 
latest version of the ideas package from Hackage and install the library with the 
following command:

       cabal install ideas

We can now start writing a new Haskell module and import two modules from the 
Ideas package.

> module Main where
> 
> import Ideas.Common.Library
> import Ideas.Main.Default

This will import basic functionality (`Ideas.Common.Library`) for defining
your own exercise. Module `Ideas.Main.Default` is needed for step 4 of this 
tutorial.

In this tutorial we will develop a domain reasoner for a simple arithmetic
expression language. The goal of the domain reasoner is to evaluate expressions.
We define a data type for expressions with addition, (unary) negation, and
integer constants.

> data Expr = Add Expr Expr | Negate Expr | Con Int
>    deriving (Eq, Show, Read)

For now we will use derived instances for testing equality, showing, and 
reading expressions. We define two examples of expressions in this datatype. 

> -- expression 5+(-2)
> expr1 :: Expr
> expr1 = Add (Con 5) (Negate (Con 2))
>
> -- expression -(3+(-5))
> expr2 :: Expr
> expr2 = Negate (Add (Con 3) (Negate (Con 5)))

Step 1: defining an exercise
----------------------------

We define rules to calculate the addition of two constants and to negate a constant.
The `Rule` datatype is parameterized over the values that are transformed (which is in
our case the `Expr` datatype). The function `makeRule` takes a name for the rule (an
identifier) and a function of type `a -> Maybe a` as its arguments. 
Constructor `Nothing` of the `Maybe`
datatype is used to indicate that the rule cannot be applied.

> addRule :: Rule Expr
> addRule = describe "Add two numbers" $ makeRule "eval.add" f
>  where
>    f :: Expr -> Maybe Expr
>    f (Add (Con x) (Con y)) = Just $ Con (x+y)
>    f _ = Nothing
> 
> negateRule :: Rule Expr
> negateRule = describe "Negate number" $ makeRule "eval.negate" f
>  where
>    f :: Expr -> Maybe Expr
>    f (Negate (Con x)) = Just $ Con (-x)
>    f _ = Nothing

Have a look at the type of the `makeRule` function and observe that the function
is overloaded in both arguments. The first argument is the rule's identifier,
which has to be part of the `IsId` type class. The `String` type is an instance
of this class as can be seen from the example. This type class helps in creating
identifiers for concepts. The `Rule` data type carries an identifier of type
`Id`; later we will see that many other concepts also have an identifier
(including `Strategy` and `Exercise`). Identifiers should have a unique name, and
this name can be hierarchicial. Hierarchical names can be created with the `'.'`
character in the name, or by using the `(#)` combinator. Values that carry an 
identifier can be given a more elaborate description with the `describe` function.

The transformations in the rules above use a function of type `a -> Maybe a`, 
but sometimes you want a rule to return multiple results. In these situations
you can use a function of type `a -> [a]`. The `MakeTrans` type class that is 
part of `makeRule`'s type generalizes over the type of a transformation function,
and has `Maybe` and `[]` as instances.

We first test the rules we defined in a Haskell interpreter by applying the 
rules to some expressions. For this, we use function `apply` from the `Apply`
type class.

< Main> apply addRule (Add (Con 5) (Con 3))
< Just (Con 8)
<
< Main> apply negateRule (Negate (Con 5))
< Just (Con (-5))
< 
< Main> apply addRule expr1
< Nothing
< 
< Main> apply negateRule expr2
< Nothing

The last example shows that rules are only applied at top-level, and not 
automatically to some arbitrary sub-expression. The rules can be combined 
into a strategy: the strategy combinator `<|>` denotes choice. We `label` 
the strategy with an identifier.

> addOrNegate :: LabeledStrategy Expr
> addOrNegate = label "one-step" $
>    addRule <|> negateRule

Also strategies can be applied to a term.

< Main> apply addOrNegate (Add (Con 5) (Con 3))
< Just (Con 8)
< 
< Main> apply addOrNegate expr1
< Nothing

We can now make a minimal exercise that uses the `addOrNegate` strategy
for solving: why we need to lift the strategy to a `Context` is explained in 
step 2 of this tutorial. Exercises should have a unique identifier for 
identification. We use `show` for pretty-printing expressions. See the 
documentation of the `Exercise` datatype for the other components of an 
exercise: `emptyExercise` provides sensible defaults so we do not have to 
worry about these fields yet.

> minimalExercise :: Exercise Expr
> minimalExercise = emptyExercise
>    { exerciseId    = describe "Evaluate an expression (minimal)" $
>                         newId "eval.minimal"
>    , strategy      = liftToContext addOrNegate
>    , prettyPrinter = show
>    }

Again, we can apply an exercise to a given expression:

< Main> apply minimalExercise (Add (Con 5) (Con 3))
< Just (Con 8)
         
For an `Exercise`, however, function `printDerivation` is more interesting
because it shows a worked-out example and not just the final answer.
         
< Main> printDerivation minimalExercise (Add (Con 5) (Con 3))
< Add (Con 5) (Con 3)
<    => eval.add
< Con 8

Step 2: adding traverals
------------------------

Eerst symbolen definieren voor de constructoren

> addSymbol, negateSymbol :: Symbol
> addSymbol    = newSymbol "add"
> negateSymbol = newSymbol "negate"

Een instantie maken van de klasse IsTerm

> instance IsTerm Expr where
>    toTerm (Add x y)  = binary addSymbol (toTerm x) (toTerm y)
>    toTerm (Negate x) = unary negateSymbol (toTerm x)
>    toTerm (Con x)    = TNum (toInteger x)
>    
>    fromTerm (TNum x) = return (Con (fromInteger x))
>    fromTerm term     = fromTermWith f term
>     where
>       f s [x]    | s == negateSymbol = return (Negate x)
>       f s [x, y] | s == addSymbol    = return (Add x y)
>       f _ _ = fail "invalid expression"

Een strategie met de traversal combinator somewhere.

> evalStrategy :: LabeledStrategy (Context Expr)
> evalStrategy = label "eval" $
>    repeatS (somewhere (liftToContext addOrNegate))
>    
> --test4 = applyAll evalStrategy $ newContext mempty $ termNavigator $ 
> --   Mul (Add (Con 5) (Con 3)) (Square (Con 2))

Een voorbeeld om te testen

Een vernieuwde exercise

> evalExerciseTraversal :: Exercise Expr
> evalExerciseTraversal = emptyExercise
>    { exerciseId    = describe "Evaluate an expression" $
>                         newId "eval"
>    , strategy      = evalStrategy
>    , navigation    = termNavigator
>    , prettyPrinter = show
>    }
>    
> --test5 = printDerivation evalExercise $ 
> --   Mul (Add (Con 5) (Con 3)) (Square (Con 2)) -- !! meerdere antwoorden

Een voorbeeldaanroep

Step 3: equivalence, similarity, and ready
---------------------------------------

> eqExpr :: Expr -> Expr -> Bool
> eqExpr x y = eval x == eval y
>
> eval :: Expr -> Int
> eval (Add x y)  = eval x + eval y
> eval (Negate x) = -eval x
> eval (Con x)    = x 
>
> simExpr :: Expr -> Expr -> Bool -- ???? voorbeeld
> simExpr x y = normalize x == normalize y
>
> normalize :: Expr -> Expr
> normalize (Add x y)  = Add (normalize x) (normalize y)
> normalize (Negate (Con x)) | x == 0 = Con 0
> normalize (Negate x) = Negate (normalize x)
> normalize (Con x)    = Con x
> 
> isCon :: Expr -> Bool
> isCon (Con _) = True
> isCon _       = False
>
> parserRead :: String -> Either String Expr
> parserRead s = 
>    case reads s of
>       [(a, xs)] | all (==' ') xs -> return a
>       _ -> fail ("no read: " ++ s)
>
> evalExercise :: Exercise Expr
> evalExercise = emptyExercise
>    { exerciseId    = describe "Evaluate an expression" $
>                         newId "eval"
>    , status        = Experimental
>    , strategy      = evalStrategy
>    , prettyPrinter = show
>    , navigation    = termNavigator
>    , parser        = parserRead
>    , equivalence   = withoutContext eqExpr
>    , similarity    = withoutContext simExpr
>    , ready         = predicate isCon
>    , examples      = level Easy [expr1] ++ level Medium [expr2]
>    }

Step 4: making a CGI-webservice
------------------------------- 

> dr :: DomainReasoner
> dr = describe "Domain reasoner for tutorial" (newDomainReasoner "eval") 
>    { exercises = [Some evalExercise]
>    , services  = myServices
>    }
>
> myServices :: [Service]
> myServices = metaServiceList dr ++ serviceList
>
> main :: IO ()
> main = defaultMain dr


$ ghc -i../src --make Tutorial.lhs

$ ./Tutorial.exe --help
IDEAS: Intelligent Domain-specific Exercise Assistants
Copyright 2013, Open Universiteit Nederland
version 1.1, revision 5899, logging disabled

Usage: ideas [OPTION]     (by default, CGI protocol)

Options:
           --version              show version number
  -?       --help                 show options
  -f FILE  --file=FILE            use input FILE as request
           --make-pages[=DIR]     generate pages for exercises and services
           --test[=DIR]           run tests on directory (default: 'test')
           --make-script=ID       generate feedback script for exercise
           --analyze-script=FILE  analyze feedback script and report errors
           
