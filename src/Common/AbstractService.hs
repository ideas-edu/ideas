module Common.AbstractService where

type ExerciseID = ()
type Term = (ExerciseID, Maybe Prefix, Expression)
type Rule = ()
type Location = ()

type Expression = () -- either concrete or abstract syntax

data Result = SyntaxError
            | Buggy [Rule]   
            | NotEquivalent      
            | Ok [Rule] Term      -- equivalent
            | Detour [Rule] Term  -- equivalent
            | Unknown Term        -- equivalent
            
generate :: ExerciseID -> Int -> Term
generate = undefined

derivation :: Term -> [(Rule, Term)]
derivation = undefined

allfirsts :: Term -> [(Rule, Term, Location)]
allfirsts = undefined

onefirst :: Term -> (Rule, Term, Location)
onefirst = undefined

applicable :: Location -> Expression -> [Rule]
applicable = undefined

apply :: Rule -> Location -> Term -> Term
apply = undefined

ready :: Term -> Bool
ready = undefined

stepsremaining :: Term -> Int
stepsremaining = undefined

submit :: Term -> Expression -> Result
submit = undefined