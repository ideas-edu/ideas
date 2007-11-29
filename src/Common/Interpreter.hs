-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Common.Interpreter 
   ( Interpreter(..), runInterpreter
   ) where

import Data.Char
import Data.List
import Control.Monad.State
import Common.History
import Common.Transformation
import Common.Strategy
import Common.Move
import Common.Utils
{- import Test.QuickCheck

checkInterpreter :: (Show a, Eq a, Move a, Arbitrary a) => Interpreter a -> IO ()
checkInterpreter interpr = do
   quickCheck (propParsePrettyPrint interpr)

 where
-- parser/pretty-printer
propParsePrettyPrint :: (Arbitrary a, Eq a) => Interpreter a -> a -> Bool
propParsePrettyPrint interpr p = p == q
 where q = parser interpr (prettyPrinter interpr p) -}

data Interpreter a = Interpreter
   { parser        :: String -> a
   , prettyPrinter :: a -> String
   , equivalence   :: a -> a -> Bool
   , finalProperty :: a -> Bool
   , ruleset       :: [Rule a]
   , strategy      :: Strategy a
   , term          :: a
   }

type M a  = HistoryT (St a) IO
data St a = St { currentTerm :: a, currentStrategy :: Strategy a  }

runInterpreter :: (Eq a, Move a) => Interpreter a -> IO ()
runInterpreter interpr = 
   evalHistoryT (mark >> step) start
 where   
   start = St
      { currentTerm     = term interpr
      , currentStrategy = strategy interpr
      }

   step = do
      -- present the current term
      current <- gets currentTerm 
      liftIO $ do 
         putStrLn "Current term:"
         putStrLn $ indent 3 $ prettyPrinter interpr current

      -- is the current term valid?
      hist <- history
      let ok = null hist || equivalence interpr current (currentTerm $ last hist)
      unless ok $ 
         liftIO $ putStrLn "WARNING: invalid formula (type :undo)"
               
      -- are we ready?
      if ok && finalProperty interpr current then 
         liftIO $ putStrLn "Congratulations!" else do
      
      -- which rules are applicable?
      let options = filter (`applicable` current) (ruleset interpr)
      unless (null options) $ liftIO $ do
         putStrLn "Applicable rules:"
         sequence_ [ putStrLn $ "   :" ++ show n ++ "  " ++ name r
                   | (n, r) <- zip [1..] options
                   ]
      
      -- ask user for command
      liftIO $ putStr "? "
      cmd <- liftIO getLine
      
      case map toLower (trim cmd) of
         ""       -> step
         ":quit"  -> return ()
         ":left"  -> moveWith ruleMoveLeft
         ":right" -> moveWith ruleMoveRight
         ":down"  -> moveWith ruleMoveDown
         ":up"    -> moveWith ruleMoveUp
         ":top"   -> moveWith ruleMoveTop
         ":undo"  -> undoMarked >> step
         ":redo"  -> redoMarked >> step
                  
         ":history" -> do
            xs <- historyMarked
            let list = unlines (map (prettyPrinter interpr . currentTerm) xs)
            liftIO $ putStrLn $ "History:\n" ++ indent 3 list 
            step
                  
         ":hint" -> do
            hint <- getHint
            case hint of
               Just rule -> do
                  current <- gets currentTerm
                  liftIO $ do 
                     putStrLn $ "Hint: apply the rule " ++ show (name rule) ++ " resulting in:"
                     putStrLn $ "   " ++ prettyPrinter interpr (applyD rule current) ++ "\n"  
               Nothing ->
                  liftIO $ putStrLn "Sorry, no hint available"
            step
 
         ':':digits | isNatural digits && nat > 0 && nat <= length options -> do
            strat <- gets currentStrategy
            let newStrategy = trackRulesWith checkRuleName therule strat current
            modify $ \s -> s {currentTerm = applyD therule current, currentStrategy = newStrategy}
            when (isFail newStrategy) $
               liftIO $ putStrLn $ "WARNING: Deviated from strategy (type :undo)"
            mark
            step
          where
            nat     = read digits 
            therule = options !! (nat-1)
         
         ':':this -> liftIO (putStrLn $ "ERROR: unknown command " ++ show this) >> step         
         _ -> do
            strat <- gets currentStrategy
            let new  = parser interpr cmd
                list = filter ((==new) . snd3) $ take 1000 $ intermediates strat current
            case list of
               (rules, _, newStrategy):_ 
                  | null rules ->
                       liftIO $ putStrLn "WARNING: same term"
                  | otherwise -> do
                     liftIO $ do
                        putStrLn "You probably applied the following rules:"
                        putStrLn $ "   " ++ commaList (reverse $ map name $ filter checkRuleName rules) ++ "\n"
                     modify $ \s -> s {currentTerm = new, currentStrategy = newStrategy}
                     mark
               _ -> do
                  liftIO $ putStrLn "I don't know which rule was applied"
                  modify $ \s -> s {currentTerm = new, currentStrategy = failS}
                  mark
            step
   
   moveWith rule = modify (\s -> s {currentTerm = applyD rule (currentTerm s)}) >> step

   getHint = do 
      strat   <- gets currentStrategy
      current <- gets (moveTop . currentTerm)
      case nextRulesWith checkRuleName strat current of
         (rules, _, _):_ | not (null rules) && checkRuleName (last rules) -> do
            let (moves, final) = (init rules, last rules)
                newValue = applyListD moves current
            modify $ \s -> s {currentTerm = newValue} -- don't update the strategy here
            return $ Just final
         _ -> 
            return Nothing
            
checkRuleName :: Rule a -> Bool
checkRuleName rule = 
   let s = name rule
   in not (null s) && not (take 1 s=="_") && not ("Move" `isPrefixOf` s) && "Check" /= s