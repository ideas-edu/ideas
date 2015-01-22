<div class="page-header"> 
<div class="ideas-logo"><img src="ideas.png"/></div>
<div class="ounl-logo"><img src="ounl.png"/></div>
&nbsp; Ideas tutorial (version 1.2)
</div>
<div class="page-content">

Making a domain reasoner
========================

This tutorial shows how to make a simple domain reasoner with the Ideas framework.
We start by defining a minimal exercise and show how this can be compiled into an 
application that can handle feedback requests. Make sure you have installed a 
Haskell compiler and the cabal package manager 
(see [Haskell Platform](http://www.haskell.org/platform/)). Get the 
latest version of the [ideas package](http://hackage.haskell.org/package/ideas) 
from Hackage and install the library with the following command:

~~~~~~~~
cabal install ideas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can now start writing a new Haskell module and import some modules from the 
Ideas package.

> module Main where
> 
> import Ideas.Common.Library
> import Ideas.Main.Default

This will import basic functionality (`Ideas.Common.Library`) for defining
your own exercise. The other import (`Ideas.Main.Default`) is needed for step 
4 of this tutorial.

In this tutorial we will develop a domain reasoner for a simple arithmetic
expression language. The goal of the domain reasoner is to evaluate expressions.
We define a data type for expressions with addition, (unary) negation, and
integer constants.

> data Expr = Add Expr Expr | Negate Expr | Con Int
>    deriving (Eq, Show, Read)

For now we will use derived instances for testing equality, showing, and 
reading expressions. We define two examples of expressions in this data type. 

> -- expression 5+(-2)
> expr1 :: Expr
> expr1 = Add (Con 5) (Negate (Con 2))
>
> -- expression (-2)+(3+5)
> expr2 :: Expr
> expr2 = Add (Negate (Con 2)) (Add (Con 3) (Con 5))

Step 1: defining an exercise
----------------------------

We define rules to calculate the addition of two constants and to negate a constant.
The `Rule` data type is parameterized over the values that are transformed (which is in
our case the `Expr` data type). The function `makeRule` takes a name for the rule (an
identifier) and a function of type `a -> Maybe a` as its arguments. 
Constructor `Nothing` of the `Maybe`
data type is used to indicate that the rule cannot be applied.

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

Have a look at the type of the `makeRule` function in the 
[documentation](http://ideas.cs.uu.nl/docs/latest/api/Common-Rule-Abstract.html#v:makeRule),
and observe that the function
is overloaded in both arguments. The first argument is the rule's identifier,
which has to be part of the `IsId` type class. The `String` type is an instance
of this class as can be seen from the example. This type class helps in creating
identifiers for concepts. The `Rule` data type carries an identifier of type
`Id`; later we will see that many other concepts also have an identifier
(including `Strategy` and `Exercise`). Identifiers should have a unique name, and
this name can be hierarchical. Hierarchical names can be created with the `'.'`
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
> addOrNegate = label "add-or-negate" $
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
[documentation](http://ideas.cs.uu.nl/docs/latest/api/Common-Exercise.html) 
of the `Exercise` data type for the other components of an 
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

For arithmetic expressions we want to apply the rules `somewhere`, i.e., 
possibly also to the sub-expressions. We want to use traversal functions such 
as `somewhere` in 
our strategy definitions, but this is only possible if we know the structure
of the terms we want to traverse. We use a zipper data structure for keeping
a point of focus. Instead of defining a zipper on the `Expr` data type, we 
define a translation to the `Term` data type in the Ideas library and use a 
zipper on `Term`s. Besides the zipper, some more untyped, generic functions are 
offered for the `Term` data type.

Two symbols are defined for the two constructors of `Expr`.

> addSymbol, negateSymbol :: Symbol
> addSymbol    = newSymbol "add"
> negateSymbol = newSymbol "negate"

These symbols are used for the `IsTerm` instance: we have to make sure that 
`fromTerm` after `toTerm` is the identity function.

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

We can now define an improved strategy that applies `addOrNegate` somewhere: 
the traversal combinators can only be used on strategies (or rules) that are 
lifted to a `Context` (or some other data type with a zipper). Therefore we 
have to lift the `addOrNegate` strategy to a `Context` before using `somewhere`.
We repeat the strategy until it can no longer be applied. Observe that the 
`evalStrategy` works on `Context Expr`s.

> evalStrategy :: LabeledStrategy (Context Expr)
> evalStrategy = label "eval" $
>    repeatS (somewhere (liftToContext addOrNegate))

Testing this strategy is more involved because we first have to put an `Expr`
into a `Context`: for this context we use the `termNavigator`.

< Main> apply evalStrategy $ newContext $ termNavigator expr1
< Just Con 3 @ []

In the output, `@ []` prints the current focus of the zipper, which is here
the top-level node of the expression. For `expr2`, the strategy can start
evaluating sub-expression `Negate (Con 2)` or sub-expression `Add (Con 3) (Con 5)`.
Therefore, evaluating this expression gives two solution paths (with the 
same result). This can be inspected by using `applyAll`, which returns all 
results of application in a list.

< Main> applyAll evalStrategy $ newContext $ termNavigator expr2
< [Con 6 @ [],Con 6 @ []]

We define an extended exercise that is based on `evalStrategy`. In the 
exercise definition, we have to declare that navigation is based on the 
`termNavigator`.

> basicExercise :: Exercise Expr
> basicExercise = emptyExercise
>    { exerciseId    = describe "Evaluate an expression (basic)" $
>                         newId "eval.basic"
>    , strategy      = evalStrategy
>    , navigation    = termNavigator
>    , prettyPrinter = show
>    }

We can now print worked-out solutions for `expr1` and `expr2`. Note that 
`printDerivations` prints all solutions (and `printDerivation` only shows one).

< Main> printDerivations basicExercise expr1
< Derivation #1
< Add (Con 5) (Negate (Con 2))
<    => eval.negate
< Add (Con 5) (Con (-2))
<    => eval.add
< Con 3
< 
< Main> printDerivations basicExercise expr2
< Derivation #1
< Add (Negate (Con 2)) (Add (Con 3) (Con 5))
<    => eval.add
< Add (Negate (Con 2)) (Con 8)
<    => eval.negate
< Add (Con (-2)) (Con 8)
<    => eval.add
< Con 6
< 
< Derivation #2
< Add (Negate (Con 2)) (Add (Con 3) (Con 5))
<    => eval.negate
< Add (Con (-2)) (Add (Con 3) (Con 5))
<    => eval.add
< Add (Con (-2)) (Con 8)
<    => eval.add
< Con 6

Step 3: equivalence and ready
---------------------------------------

For diagnosing a student step, we have to define which expressions are 
semantically equivalent. When left undefined in an exercise, all expressions 
are equivalent, which is not very helpful. For the `Expr` data type, we specify
that two values are equivalent when they evaluate to the same `Int` value.

> eqExpr :: Expr -> Expr -> Bool
> eqExpr x y = eval x == eval y
>
> eval :: Expr -> Int
> eval (Add x y)  = eval x + eval y
> eval (Negate x) = -eval x
> eval (Con x)    = x 

We also want to define the goal of an exercise: we are ready rewriting an 
expression when we have reached a constant value.

> isCon :: Expr -> Bool
> isCon (Con _) = True
> isCon _       = False

We give an extended definition for the exercise with `equivalence` and 
`ready`. We also specify its `status`, the `parser` for expressions, and 
two example expressions (of a certain difficulty). 

> evalExercise :: Exercise Expr
> evalExercise = emptyExercise
>    { exerciseId    = describe "Evaluate an expression (full)" $
>                         newId "eval.full"
>    , status        = Experimental
>    , strategy      = evalStrategy
>    , prettyPrinter = show
>    , navigation    = termNavigator
>    , parser        = readM
>    , equivalence   = withoutContext eqExpr
>    , ready         = predicate isCon
>    , examples      = level Easy [expr1] ++ level Medium [expr2]
>    }

The `readM` function is
defined in the Ideas library and provides a simple parser for values (here:
a parser for `Expr`s) based on an instance for the `Read` type class.
We now have a somewhat simple, but fully functional exercise for evaluating
expressions.

Step 4: making a CGI-webservice
------------------------------- 

An exercise can be used by external tools by turning it into a domain reasoner:
such a reasoner supports some exercises, and provides a number of (standard)
feedback services. We use the three exercises we have defined so far, 
together with the standard set of services.

> dr :: DomainReasoner
> dr = describe "Domain reasoner for tutorial" (newDomainReasoner "eval") 
>    { exercises = [Some minimalExercise, Some basicExercise, Some evalExercise]
>    , services  = myServices
>    }
>
> myServices :: [Service]
> myServices = metaServiceList dr ++ serviceList

A default main function is provided by the Ideas framework.

> main :: IO ()
> main = defaultMain dr

Compile the module to get an executable. In this tutorial we assume that the 
code is placed in a file called `Tutorial.hs`, and the result of compilation
is an executable `Tutorial.exe` (on Windows). The software, however, is portable
and can also be compiled for other platforms (including Mac OS and Linux).

~~~~
$ ghc --make Tutorial.hs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Running the executable with the `--help` flag gives the options.

~~~~
$ Tutorial.exe --help
IDEAS: Intelligent Domain-specific Exercise Assistants
Copyright 2014, Open Universiteit Nederland
version 1.2, revision 6534, logging disabled

Usage: ideas [OPTION]     (by default, CGI protocol)

Options:
           --version              show version number
  -?       --help                 show options
  -f FILE  --file=FILE            use input FILE as request
           --make-pages[=DIR]     generate pages for exercises and services
           --test[=DIR]           run tests on directory (default: 'test')
           --make-script=ID       generate feedback script for exercise
           --analyze-script=FILE  analyze feedback script and report errors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The application handles requests: one way is to place the request in a file and 
to pass the file name to the application. In the example requests we use XML,
but also other encodings are supported. If we want to know the list of 
supported exercises, we place the following request in a file `exerciselist.xml`

~~~~ {#mycode .xml}   
<request service="exerciselist" source="tutorial"/>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                              
It is a good custom to always include the source of the request to let the 
domain reasoner know where the request came from.
The result of this request is:

~~~~ {#mycode .xml}
$ Tutorial.exe --file=exerciselist.xml                                          
<reply result="ok" version="1.2 (6534)">
  <list>
    <elem exerciseid="eval.basic" description="Evaluate an expression (basic)" status="Experimental"/>
    <elem exerciseid="eval.full" description="Evaluate an expression (full)" status="Experimental"/>
    <elem exerciseid="eval.minimal" description="Evaluate an expression (minimal)" status="Experimental"/>
  </list>
</reply>                                            
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Or we request a worked-out solution for `Add (Con 5) (Negate (Con 2))`. 

~~~~ {#mycode .xml}
<request exerciseid="eval.full" service="derivation" encoding="string" source="tutorial">
	<state>                
		<expr>Add (Con 5) (Negate (Con 2))</expr>
	</state>
</request>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this request we have to specify that the encoding of expressions is a plain 
string and that we want to use the parser/pretty-printer defined for the 
exercise. The default encoding follows the 
[OpenMath standard](http://www.openmath.org/standard/) for representing 
mathematical objects. The result of this request is:

~~~~ {#mycode .xml}   
$ Tutorial.exe --file=solution.xml
<reply result="ok" version="1.2 (6534)">
  <list>
    <elem ruleid="eval.negate">
      <expr>
        Add (Con 5) (Con (-2))
      </expr>
      <context>
        <item name="location" value="[1]"/>
      </context>
    </elem>
    <elem ruleid="eval.add">
      <expr>
        Con 3
      </expr>
    </elem>
  </list>
</reply>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The executable `Tutorial.exe` is also a cgi-binary that can be deployed on a 
web-server. Because there is support for generating HTML as output, it is 
possible to interactively explore the domain reasoner with a browser and a
local server. 

* Install a webserver, such as [WampServer](http://www.wampserver.com/) for Windows.
* Make sure you enable the execution of CGI scripts (in `httpd.conf`)
* Rename the executable to `Tutorial.cgi` and place it in the directory for cgi scripts
* Start a browser and type in the URL `http://localhost/Tutorial.cgi?input=<request service="index" encoding="html"/>`
      
You can now start exploring the supported exercises and feedback services. 
For instance, go to the exercise `eval.full` and click on `derivations` in the yellow box
to see the worked-out solutions for two examples.

Suggested exercises
-------------------

* Add multiplication to the expression language (and extend the evaluation strategy)
* Add distribution rules to the strategy
* Add support for calculating with fractions (e.g. 5/7 + 1/2)
    * Find the least common multiple of the denominators
    * Rewrite top-heavy fractions to mixed fractions (e.g. 17/14 becomes 1+3/14)

</div>
<div class="page-footer">
This tutorial is based on ideas-1.2. Last changed: May 2014
</div>