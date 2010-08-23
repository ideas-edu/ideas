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
   let lhs :~> rhs = rulePair r
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
   | s == newSymbol "logic1.true"  = con "T"
   | s == newSymbol "logic1.false" = con "F"
 where
   con s = return [Left s]
-- unary symbols
specialSymbol s [a]
   | s == newSymbol "logic1.not"         = pre "\172" -- "~"
   | s == newSymbol "arith1.unary_minus" = pre "-"
 where
   pre s = return [Left s, Right a]
-- binary symbols
specialSymbol s [a, b]
   | s == newSymbol "logic1.or"         = bin " \8744 " -- "||"
   | s == newSymbol "logic1.and"        = bin " \8743 " -- "&&"
   | s == newSymbol "logic1.implies"    = bin " \8594 " -- "->"
   | s == newSymbol "logic1.equivalent" = bin " \8596 " -- "<->"
   | s == newSymbol "relation1.eq"      = bin " = "
   | s == newSymbol "arith1.plus"       = bin "+"
   | s == newSymbol "arith1.minus"      = bin "-"
   | s == newSymbol "arith1.power"      = bin "^"
   | s == newSymbol "arith1.times"      = bin "\x00B7" -- "*"
 where
   bin s = return [Right a, Left s, Right b]
specialSymbol _ _ = Nothing

showMeta :: Exercise a -> Int -> String
showMeta ex n
   | safeHead (qualifiers ex) == Just "logic" = [ [c] | c <- ['p'..] ] !! n
   | otherwise = [ [c] | c <- ['a'..] ] !! n