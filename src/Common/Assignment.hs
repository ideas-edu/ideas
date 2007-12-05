module Common.Assignment where

import Common.Transformation
import Common.Strategy
import Test.QuickCheck

data Language = English | Dutch

data Assignment a = Assignment
   { parser        :: String -> a
   , prettyPrinter :: a -> String
   , equivalence   :: a -> a -> Bool
   , finalProperty :: a -> Bool
   , ruleset       :: [Rule a]
   , strategy      :: Strategy a
   , generator     :: Gen a
   , language      :: Language
   }
   
randomTerm :: Assignment a -> IO a
randomTerm = undefined

-- | Returns a text and the rule that is applicable
giveHint :: Assignment a -> a -> (Doc a, Rule a)
giveHint = undefined

-- | Returns a text, a sub-expression that can be rewritten, and the result
-- | of the rewriting
giveStep :: Assignment a -> a -> (Doc a, a, a)
giveStep = undefined

feedback :: Assignment a -> String -> Feedback a
feedback = undefined

data Feedback a = SyntaxError (Doc a) (Maybe a) {- corrected -}
                | Incorrect   (Doc a) (Maybe a)
                | Correct     (Doc a) Bool
                
data Doc a = DocText String
           | DocDomain a
           | DocConcat [Doc a]