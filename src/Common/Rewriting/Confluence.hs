module Common.Rewriting.Confluence 
   ( confluence, confluenceWith, confluentFunction
   , testConfluence, testConfluenceWith, testConfluentFunction
   ) where

import Common.Rewriting.MetaVar
import Common.Rewriting.AC
import Common.Rewriting.Substitution
import Common.Rewriting.Unification
import Common.Rewriting.RewriteRule
import Common.Uniplate (subtermsAt, applyAtM, somewhereM)
import Data.List
import Data.Maybe

----------------------------------------------------

superImpose :: Rewrite a => RewriteRule a -> RewriteRule a -> [([Int], a)]
superImpose r1 r2 =
   [ (loc, s |-> lhs2) | (loc, a) <- subtermsAt lhs2, s <- make a ]
 where
    lhs1 = lhs (rulePair r1 0)
    lhs2 = lhs (rulePair r2 (nrOfMetaVars r1))
    
    make a
       | isJust (isMetaVar a) = []
       | otherwise            = unifyM lhs1 a

criticalPairs :: (Rewrite a, Eq a) => [RewriteRule a] -> [(a, (RewriteRule a, a), (RewriteRule a, a))]
criticalPairs rs = 
   [ (a, (r1, b1), (r2, b2)) 
   | r1       <- rs
   , r2       <- rs
   , (loc, a) <- superImpose r1 r2
   , b1       <- rewriteM r1 a
   , b2       <- applyAtM loc (rewriteM r2) a
   , b1 /= b2 
   ]

noDiamondPairs :: (Rewrite a, Eq a) => (a -> a) -> [RewriteRule a] -> [(a, (RewriteRule a, a, a), (RewriteRule a, a, a))]
noDiamondPairs f rs =
   [ (a, (r1, e1, nf1), (r2, e2, nf2)) 
   | (a, (r1, e1), (r2, e2)) <- criticalPairs rs
   , let (nf1, nf2) = (f e1, f e2)
   , nf1 /= nf2
   ]

reportPairs :: (Show a, Eq a) => [(a, (RewriteRule a, a, a), (RewriteRule a, a, a))] -> IO ()
reportPairs = putStrLn . unlines . zipWith f [1::Int ..]
 where
   f i (a, (r1, e1, nf1), (r2, e2, nf2)) = unlines
      [ show i ++ ") " ++ show a
      , "  "   ++ show r1
      , "    " ++ show e1 ++ if e1==nf1 then "" else "   -->   " ++ show nf1
      , "  "   ++ show r2
      , "    " ++ show e2 ++ if e2==nf2 then "" else "   -->   " ++ show nf2
      ]

----------------------------------------------------

confluentFunction :: (Eq a, Show a, Rewrite a) => (a -> a) -> [RewriteRule a] -> IO ()
confluentFunction f rs = reportPairs $ noDiamondPairs f rs

confluenceWith :: (Ord a, Show a, Rewrite a) => [Operator a] -> [RewriteRule a] -> IO ()
confluenceWith ops rs = confluentFunction (normalFormWith ops rs) rs

confluence :: (Ord a, Show a, Rewrite a) => [RewriteRule a] -> IO ()
confluence = confluenceWith operators

----------------------------------------------------

testConfluentFunction :: (Eq a, Rewrite a) => (a -> a) -> [RewriteRule a] -> a -> Bool
testConfluentFunction f rs a = 
   case nub [ f b | r <- rs, b <- somewhereM (rewriteM r) a ] of
      _:_:_ -> False
      _     -> True
      
testConfluenceWith :: (Ord a, Rewrite a) => [Operator a] -> [RewriteRule a] -> a -> Bool
testConfluenceWith ops rs = testConfluentFunction (normalFormWith ops rs) rs

testConfluence :: (Ord a, Rewrite a) => [RewriteRule a] -> a -> Bool
testConfluence = testConfluenceWith operators