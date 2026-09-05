-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Encoding.RulePresenter (ruleToHTML) where

import Data.List
import Data.Maybe
import Ideas.Common.Library
import Ideas.Text.HTML

ruleToHTML :: Some Exercise -> Rule a -> HTMLBuilder
ruleToHTML ex r = mconcat
   [ rewriteRuleToHTML (not $ isBuggy r) ex rr
   | Some rr <- getRewriteRules (transformation r)
   ]

rewriteRuleToHTML :: Bool -> Some Exercise -> RewriteRule a -> HTMLBuilder
rewriteRuleToHTML sound ex r =
   showTerm ex lhs <> spaces 3 <>
   showLeadsTo sound <> spaces 3 <>
   showTerm ex rhs <> br
 where
   lhs :~> rhs = ruleSpecTerm r

showLeadsTo :: Bool -> HTMLBuilder
showLeadsTo sound = string (if sound then "\x21D2" else "\x21CF")

showTerm :: Some Exercise -> Term -> HTMLBuilder
showTerm (Some ex) = string . rec
 where
   rec term =
      case term of
         TVar s    -> s
         TNum i    -> show i
         TFloat a  -> show a
         TMeta n   -> showMeta ex n
         TCon s xs -> concatMap (either id recp) $
                      let txt = withSpaces (Left (show s) : map Right xs)
                      in fromMaybe txt (specialSymbol s xs)
         TList xs  -> "[" ++ intercalate ", " (map rec xs) ++ "]"

   recp term  = parIf (isCon term) (rec term)
   withSpaces = intersperse (Left " ")

   isCon (TCon _ xs) = not (null xs)
   isCon _           = False

   parIf b s = if b then "(" ++ s ++ ")" else s

specialSymbol :: Symbol -> [Term] -> Maybe [Either String Term]
-- constants
specialSymbol s []
   | sameSymbol s "logic1.true"     = con "T"
   | sameSymbol s "logic1.false"    = con "F"
   | sameSymbol s "relalg.universe" = con "V" -- universe
   | sameSymbol s "relalg.ident"    = con "I" -- identity
 where
   con x = return [Left x]
-- unary symbols
specialSymbol s [a]
   | sameSymbol s "logic1.not"         = pref "\172" -- "~"
   | sameSymbol s "arith1.unary_minus" = pref "-"
   | sameSymbol s "relalg.not"         = post "\x203E"
   | sameSymbol s "relalg.inv"         = post "~"
 where
   pref x  = return [Left x, Right a]
   post x = return [Right a, Left x]
-- binary symbols
specialSymbol s [a, b]
   | sameSymbol s "logic1.or"         = bin " \8744 " -- "||"
   | sameSymbol s "logic1.and"        = bin " \8743 " -- "&&"
   | sameSymbol s "logic1.implies"    = bin " \8594 " -- "->"
   | sameSymbol s "logic1.equivalent" = bin " \8596 " -- "<->"
   | sameSymbol s "relation1.eq"      = bin " = "
   | sameSymbol s "arith1.plus"       = bin "+"
   | sameSymbol s "arith1.minus"      = bin "-"
   | sameSymbol s "arith1.power"      = bin "^"
   | sameSymbol s "arith1.times"      = bin "\x00B7" -- "*"
   | sameSymbol s "arith1.divide"     = bin "/"
   | sameSymbol s "relalg.conj"       = bin " \x2229 " -- intersect
   | sameSymbol s "relalg.disj"       = bin " \x222A " -- union
   | sameSymbol s "relalg.comp"       = bin " ; " -- composition
   | sameSymbol s "relalg.add"        = bin " \x2020 " -- relative addition/dagger
 where
   bin x = return [Right a, Left x, Right b]

specialSymbol s1 [TCon s2 [x, a]]
   | sameSymbol s1 "calculus1.diff" && sameSymbol s2 "fns1.lambda" =
        return [Left "D(", Right x, Left ") ", Right a]
specialSymbol _ _ = Nothing

sameSymbol :: Symbol -> String -> Bool
sameSymbol = (==) . show

showMeta :: Exercise a -> Int -> String
showMeta ex n
   | listToMaybe (qualifiers ex) == Just "logic" = [ [c] | c <- ['p'..] ] !! n
   | listToMaybe (qualifiers ex) == Just "relationalgebra" = [ [c] | c <- ['r'..] ] !! n
   | otherwise = [ [c] | c <- ['a'..] ] !! n