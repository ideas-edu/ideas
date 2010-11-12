-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Documentation.RulePresenter (ruleToHTML) where

import Common.Library
import Control.Monad
import Common.Utils (Some(..), safeHead)
import Common.Rewriting.Term
import Data.List
import Text.HTML

ruleToHTML :: Some Exercise -> Rule a -> HTMLBuilder
ruleToHTML ex r = 
   forM_ (getRewriteRules r) $ \(Some rr, b) -> 
      rewriteRuleToHTML b ex rr

rewriteRuleToHTML :: Bool -> Some Exercise -> RewriteRule a -> HTMLBuilder
rewriteRuleToHTML sound ex r = do
   let lhs :~> rhs = ruleSpecTerm r
   -- showRuleName (unqualified r)
   -- spaces 3
   showTerm ex lhs
   spaces 3
   showLeadsTo sound
   spaces 3
   showTerm ex rhs
   br

{-     
showRuleName :: String -> HTMLBuilder
showRuleName s = text ("[" ++ s ++ "]")
-}

showLeadsTo :: Bool -> HTMLBuilder
showLeadsTo sound = text (if sound then "\x21D2" else "\x21CF")

showTerm :: Some Exercise -> Term -> HTMLBuilder
showTerm (Some ex) = text . rec
 where
   rec term =
      case term of
         Var s   -> s
         Num i   -> show i
         Float a -> show a
         Meta n  -> showMeta ex n
         _ -> concatMap (either id recp) $  
            case getSpine term of
               (Con s, xs) -> 
                  case specialSymbol s xs of
                     Just ys -> ys
                     Nothing -> spaced (Left (show s) : map Right xs)
               (x, xs) -> spaced (map Right (x:xs))
   
   recp term = parIf (isApply term) (rec term)
   spaced    = intersperse (Left " ")
      
   isApply (Apply _ _) = True
   isApply _           = False
      
   parIf b s = if b then "(" ++ s ++ ")" else s           
         
specialSymbol :: Symbol -> [Term] -> Maybe [Either String Term]
-- constants
specialSymbol s [] 
   | sameSymbol s "logic1.true"     = con "T"
   | sameSymbol s "logic1.false"    = con "F"
   | sameSymbol s "relalg.universe" = con "V" -- universe
   | sameSymbol s "relalg.ident"    = con "I" -- identity
 where
   con s = return [Left s]
-- unary symbols
specialSymbol s [a]
   | sameSymbol s "logic1.not"         = pre "\172" -- "~"
   | sameSymbol s "arith1.unary_minus" = pre "-"
   | sameSymbol s "relalg.not"         = post "\x203E"
   | sameSymbol s "relalg.inv"         = post "~"
 where
   pre s  = return [Left s, Right a]
   post s = return [Right a, Left s]
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
   bin s = return [Right a, Left s, Right b]
specialSymbol s1 [Apply (Apply (Con s2) x) a] 
   | sameSymbol s1 "calculus1.diff" && sameSymbol s2 "fns1.lambda" = 
        return [Left "D(", Right x, Left ") ", Right a] 
specialSymbol _ _ = Nothing

sameSymbol :: Symbol -> String -> Bool
sameSymbol = (==) . show 

showMeta :: Exercise a -> Int -> String
showMeta ex n
   | safeHead (qualifiers ex) == Just "logic" = [ [c] | c <- ['p'..] ] !! n
   | safeHead (qualifiers ex) == Just "relationalgebra" = [ [c] | c <- ['r'..] ] !! n
   | otherwise = [ [c] | c <- ['a'..] ] !! n