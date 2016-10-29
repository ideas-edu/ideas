<div class="page-header"> 
<div class="ideas-logo"><img src="ideas.png"/></div>
<div class="ounl-logo"><img src="ounl.png"/></div>
&nbsp; Ideas tutorial (version 1.5) - solutions to suggested exercices
</div>
<div class="page-content">

Solutions to suggested exercises
================================

This document shows a possible solution to the suggested exercises in the tutorial.

> module Main where
> 
> import Ideas.Common.Library
> import Ideas.Main.Default

We extend the expression datatype with constructors for multiplication and division.
The term instance needs to be extended with new symbols for multiplication and division.

> data Expr  =  Con Int
>            |  Negate Expr 
>            |  Add Expr Expr 
>            |  Mul Expr Expr 
>            |  Div Expr Expr
>    deriving (Eq, Show, Read)
>
> negateSymbol, addSymbol, mulSymbol, divSymbol :: Symbol
> negateSymbol = newSymbol "negate"
> addSymbol    = newSymbol "add"
> mulSymbol    = newSymbol "mul"
> divSymbol    = newSymbol "div"
>
> instance IsTerm Expr where
>    toTerm (Con x)    = TNum (toInteger x)
>    toTerm (Negate x) = unary negateSymbol (toTerm x)
>    toTerm (Add x y)  = binary addSymbol (toTerm x) (toTerm y)
>    toTerm (Mul x y)  = binary mulSymbol (toTerm x) (toTerm y)
>    toTerm (Div x y)  = binary divSymbol (toTerm x) (toTerm y)
>    
>    fromTerm (TNum x) = return (Con (fromInteger x))
>    fromTerm term     = fromTermWith f term
>     where
>       f s [x]    | s == negateSymbol = return (Negate x)
>       f s [x, y] | s == addSymbol    = return (Add x y)
>       f s [x, y] | s == mulSymbol    = return (Mul x y)
>       f s [x, y] | s == divSymbol    = return (Div x y)
>       f _ _ = fail "invalid expression"

We add some examples in which we use multiplication and division.

> -- expression 
> expr1, expr2, expr3, expr4, expr5, expr6, expr7 :: Expr
> expr1 = Add (Con 5) (Negate (Con 2))                                      -- 5+(-2)
> expr2 = Add (Negate (Con 2)) (Add (Con 3) (Con 5))                        -- (-2)+(3+5)
> expr3 = Mul (Con 3) (Add (Mul (Con 2) (Con 5)) (Negate (Con 3)))          -- 3 * (2*5 - 3)
> expr4 = Mul (Div (Con 3) (Con 6)) (Add (Con 1) (Con 1))                   -- 3/6 * 1+1
> expr5 = Add (Con 7) (Negate (Mul (Con 5) (Negate (Div (Con 1) (Con 2))))) -- 7 + -(5*-(1/2))
> expr6 = Div (Div (Con 4) (Con 5)) (Div (Con 9) (Con 8))                   -- (4/5)/(9/8)
> expr7 = Mul (Div (Div (Con 3) (Con 4)) (Con 7)) (Div (Con 1) (Div (Con 2) (Con 3))) 
>                                                                           -- ((3/4)/7)*(1/(2/3)

We copy the negateRule and addRule from the tutorial,

> negateRule :: Rule Expr
> negateRule = describe "Negate number" $ makeRule "eval.negate" f
>  where
>    f :: Expr -> Maybe Expr
>    f (Negate (Con x))  =  Just $ Con (-x)
>    f _                 =  Nothing
>
> addRule :: Rule Expr
> addRule = describe "Add two numbers" $ makeRule "eval.add" f
>  where
>    f :: Expr -> Maybe Expr
>    f (Add (Con x) (Con y))  =  Just $ Con (x+y)
>    f _                      =  Nothing

and add a rule for multiplication.

> mulRule :: Rule Expr
> mulRule = describe "Multiply two numbers" $ makeRule "eval.mul" f
>  where
>    f :: Expr -> Maybe Expr
>    f (Mul (Con x) (Con y))  =  Just $ Con (x*y)
>    f _                      =  Nothing
>

Multiplication distributes over addition. This can be viewed as pushing multiplications as far as possible downwards in the expression.

> mulAddRule :: Rule Expr
> mulAddRule = describe "Distribute multiplication over addition" $ makeRule "eval.dist" f
>  where
>    f :: Expr -> Maybe Expr
>    f (Mul x (Add y z))  =  Just $ Add (Mul x y) (Mul x z)
>    f (Mul (Add x y) z)  =  Just $ Add (Mul x z) (Mul y z)
>    f _                  =  Nothing

When we evaluate an expression, we no longer return an integer, but an expression in which divisions still may appear. 
Alternatively, we can use the type Ratio as the result type, but that might look slightly less attractive when there are no divisions in the argument.

We introduce a number of rules that push divisions as far as possible upwards.

> divNegateRule :: Rule Expr
> divNegateRule = describe "Push negation through division" $ makeRule "eval.divNegate" f
>  where
>    f :: Expr -> Maybe Expr
>    f (Negate (Div x y))  =  Just $ Div (Negate x) y
>    f _                   =  Nothing
>
> divAddRule :: Rule Expr
> divAddRule = describe "Push add through divisions" $ makeRule "eval.divAdd" f
>  where 
>    f :: Expr -> Maybe Expr
>    f (Add (Div x y) (Div v w))  =  Just $ Div (Add (Mul x w) (Mul v y)) (Mul y w)
>    f (Add (Div x y) v)          =  Just $ Div (Add x (Mul v y)) y
>    f (Add x (Div y v))          =  Just $ Div (Add (Mul x v) y) v
>    f _                          =  Nothing
>
> divMulRule :: Rule Expr
> divMulRule = describe "Push multiply through divisions" $ makeRule "eval.divMul" f
>  where 
>    f :: Expr -> Maybe Expr
>    f (Mul (Div x y) (Div v w))  =  Just $ Div (Mul x v) (Mul y w)
>    f (Mul (Div x y) v)          =  Just $ Div (Mul x v) y
>    f (Mul x (Div v w))          =  Just $ Div (Mul x v) w
>    f _                          =  Nothing

The divDivRule is the only rule where Div is not pushed to top-level. These are
the standard rules for calculating divisions of divisions. Since the Mul and Add
rules do not produce divisions of divisions, there is no risk for a loop in the
final evaluation strategy.

> divDivRule :: Rule Expr
> divDivRule = describe "Push divisions through divisions" $ makeRule "eval.divDiv" f
>  where 
>    f :: Expr -> Maybe Expr
>    f (Div (Div x y) (Div v w))  =  Just $ Mul (Div x y) (Div w v)
>    f (Div (Div x y) v)          =  Just $ Div x (Mul y v)
>    f (Div x (Div v w))          =  Just $ Mul x (Div w v)
>    f _                          =  Nothing

I have two simplification rules for divisions; the topHeavyRule splits a
division into an addition and a division. I cannot combine the topHeavyRule with
the other division rules (in particular: divAdd) since evaluation will loop
otherwise.

> divSimplificationRule :: Rule Expr
> divSimplificationRule = describe "Simplify a division" $ makeRule "eval.divSimplification" f
>  where 
>    f :: Expr -> Maybe Expr
>    f (Div (Con x) (Con y)) 
>      | x == 0    = Just $ Con 0
>      | x == y    = Just $ Con 1
>      | g >  1    = Just $ Div (Con (div x g)) (Con (div y g)) 
>      where g = gcd x y
>    f _ = Nothing
>
> topHeavyRule :: Rule Expr
> topHeavyRule = describe "Simplify a top heavy division" $ makeRule "eval.topHeavy" f
>  where 
>    f :: Expr -> Maybe Expr
>    f (Div (Con x) (Con y)) 
>      | x > y  =  Just $ Add (Con (div x y)) (Div (Con (mod x y)) (Con y))
>    f _ = Nothing

allEvaluationRules is the strategy that combines all rules that move multiplications downwards, and divisions upwards, and evaluates all expressions with constants. 
I do not apply divSimplificationRule, but this could be done.

> allEvaluationRules :: LabeledStrategy Expr
> allEvaluationRules = label "all rules" $
>    negateRule .|. addRule .|. mulRule .|. mulAddRule .|. divNegateRule .|. divAddRule .|. divMulRule .|. divDivRule 
>    -- .|. divSimplificationRule this simplification can also be done during evaluation

allSimplificationRules is the strategy that combines all simplification rules.

> allSimplificationRules :: LabeledStrategy Expr
> allSimplificationRules = label "all rules" $
>    topHeavyRule .|. divSimplificationRule .|. addRule

Evaluation consists of applying allEvaluationRules as often as possible, resulting in pushing divisions up as much as possible, followed by applying
allSimplificationRules, which simplifies the divisions, replaces top heavy divisions by additions, and does some additional simplications using the addRule in the resulting expression.

> evalStrategy :: LabeledStrategy (Context Expr)
> evalStrategy = label "eval" $
>    repeatS (somewhere (liftToContext allEvaluationRules)) .*. repeatS (somewhere (liftToContext allSimplificationRules))

Evaluation has been extended with a case for Mul, but not for Div. 

> eval :: Expr -> Int
> eval (Add x y)  = eval x + eval y
> eval (Mul x y)  = eval x * eval y
> eval (Negate x) = -eval x
> eval (Con x)    = x 

An expression is in `normal' form if it is a Con, a non-top heavy division, or the addition of a constant and a division.

> isConOrAddDivOrDiv                          :: Expr -> Bool
> isConOrAddDivOrDiv (Con _)                  =  True
> isConOrAddDivOrDiv (Add (Con _) (Div _ _))  =  True
> isConOrAddDivOrDiv (Div (Con _) (Con _))    =  True
> isConOrAddDivOrDiv _                        =  False
>
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
>    , ready         = predicate isConOrAddDivOrDiv
>    , examples      = level Easy [expr1] ++ level Medium [expr2] ++ level Medium [expr3]
>    }
>
> eqExpr :: Expr -> Expr -> Bool
> eqExpr x y = eval x == eval y
>
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

</div>
<div class="page-footer">
This tutorial is based on ideas-1.5. Last changed: October 2016
</div>
