module Main (main) where

import Common.Exercise
import Common.Rewriting
import Common.Transformation
import Common.Utils
import Control.Monad
import Service.ExerciseList
import Data.Char
import Data.List
import System.Environment
import System.Time

main :: IO ()
main = do
   dir <- targetDirectory
   flip mapM_ exerciseList $ \(Some ex) -> do
      let rules = concatMap getRewriteRules (ruleset ex)
      unless (null rules) $ do
         doc <- makeDocument ex
         let filename = dir ++ "/" ++ show (exerciseCode ex) ++ ".lhs"
         putStrLn $ "Creating " ++ filename
         writeFile filename doc

targetDirectory :: IO String
targetDirectory = do
   args <- getArgs
   case args of
      []    -> return "."
      dir:_ -> return dir

{- main :: IO ()
main = do 
   let vs = map (Var . return) $ [ 'p', 'q', 'r' ] ++ [ 'a' .. 'o' ]
   mapM_ (doOneRule dnfExercise {prettyPrinter = logicToTeX} "tmp/Logic/" vs) (ruleset dnfExercise)
   let vs = map (RA.Var . return) $ [ 'q', 'r', 's' ] ++ [ 'a' .. 'o' ]
   mapM_ (doOneRule cnfExercise {prettyPrinter = relAlgToTeX} "tmp/RelationAlg/" vs) (ruleset cnfExercise)

doOneRule :: Rewrite a => Exercise a -> String -> [a] -> Rule (Context a) -> IO ()
doOneRule ex dir vs r = do
   let nr = filter (not . isSpace) (name r)
   putStrLn $ "rule " ++ nr
   writeFile (dir++nr++".tex") (ruleToTeX ex vs r) -}

rulesToTeX :: Exercise a -> String
rulesToTeX = unlines . map f . concatMap getRewriteRules . ruleset
 where
   f (Some r) = "RewriteRule " ++ filter isAlpha (ruleName r) ++ " (" ++ showRuleSpec r ++ ")"

{- ruleToTeX :: Rewrite a => Exercise a -> [a] -> Rule (Context a) -> String
ruleToTeX ex vs r = unlines
   [ "\\input fontch.tex.inc"
   , "\\fourteenpoint\\nopagenumbers\\noindent"
   , unlines $ map (transformationToTeX ex vs (name r)) (transformations r) 
   , "\\bye"
   ]
   
transformationToTeX :: Rewrite a => Exercise a -> [a] -> String -> Transformation (Context a) -> String
transformationToTeX ex vs n t =
   let (lhs, rhs) = case getPatternPair (inContext undefined) t of
                       Just (a, b) -> 
                          let list = IS.toList $ getMetaVars (fromContext a) `IS.union` getMetaVars (fromContext b)
                              sub  = listToSubst (zip list vs)
                          in ( prettyPrinter ex $ sub |-> fromContext a
                             , prettyPrinter ex $ sub |-> fromContext b
                             )
                       _           -> ("\\ldots", "\\ldots")
   in "\\noindent " ++ "{\\sc " ++ n ++ ":} $" ++ lhs ++ " = " ++ rhs ++ "$\\par" 

rewriteRuleToTeX :: (a -> String) -> RewriteRule a -> String
rewriteRuleToTeX f r = concat
   [ "\\noindent "
   , "{\\sc " ++ n ++ ":} $" 
   , f (lhs p) 
   , " = "
   , f (rhs p)
   , "$\\par"
   ]
 where
    n = ruleName r
    p = rulePair r 0

logicToTeX :: Logic -> String
logicToTeX = logicToTeXPrio 0

logicToTeXPrio :: Int -> Logic -> String
logicToTeXPrio n p = foldLogic (var, binop 3 "\\rightarrow", binop 0 "\\leftrightarrow", binop 2 "\\wedge", binop 1 "\\vee", nott, var "T", var "F") p n ""
 where
   binop prio op p q n = parIf (n > prio) (p (prio+1) . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   nott p n  = ("\\neg "++) . p 4
   parIf b f = if b then ("("++) . f . (")"++) else f
   
relAlgToTeX :: RelAlg -> String
relAlgToTeX = ppRelAlgPrio 0

ppRelAlgPrio :: Int -> RelAlg -> String 
ppRelAlgPrio n p = foldRelAlg (var, binop 5 ";", binop 4 "\\dagger", binop 3 "\\cap", binop 2 "\\cup", nott, inv, var "U", var "E") p n ""
 where
   binop prio op p q n = parIf (n > prio) (p (prio+1) . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   nott p n  = ("\\overline{"++) . p 6 . ("}"++) 
   inv  p n  = ("{"++) . p 6 . ("}^{\\smile}"++)
   parIf b f = if b then ("("++) . f . (")"++) else f -}
   
------------------------------------------------------

makeDocument :: Exercise a -> IO String
makeDocument ex = do
   time <- getClockTime
   return $ 
      texHeader (Just $ domain ex ++ ".fmt") ++ 
      texBody (Just $ show time) (texSectionRules ex)

------------------------------------------------------

texHeader :: Maybe String -> String
texHeader fmt = unlines
   [ "\\documentclass{article}"
   , ""
   , "%include lhs2TeX.fmt"
   , maybe "" ("%include "++) fmt
   , "" 
   , "\\newcommand{\\rewriterule}[2]{#1: #2}"
   , "\\newcommand{\\rulename}[1]{\\mbox{\\sc #1}}"
   , ""
   , "%format RewriteRule (a) (b) = a\": \"b"
   , "%format :~> = \"\\:\\leadsto\\:\""
   ]
   
texBody :: Maybe String -> String -> String
texBody date content = unlines
   [ "\\begin{document}"
   , maybe "" (\s -> "\\hfill@(generated on " ++ s ++ ")@") date
   , content
   , "\\end{document}"
   ]
   
texSectionRules :: Exercise a -> String
texSectionRules ex = unlines 
   [ "\\section*{Rewrite Rules}"
   , formats
   , "\\begin{code}"
   , filter (/= '"') $ rulesToTeX ex
   , "\\end{code}"
   ]
 where
   rules   = concatMap getRewriteRules (ruleset ex)
   names   = let f (Some r) = ruleName r 
             in nub (map f rules)
   formats = unlines (map make names)
   make  s = "%format " ++ s ++ " = \"\\rulename{" ++ s ++ "}\"" 