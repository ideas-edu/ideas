-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Common.Interpreter (runInterpreter) where

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.State
import Common.History
import Common.Transformation
import Common.Strategy
import Common.Move
import Common.Utils
import Common.Assignment

type M a  = HistoryT (St a) IO
data St a = St { currentTerm :: a, currentStrategy :: Strategy a  }

runInterpreter :: Move a => Assignment a -> IO ()
runInterpreter interpr = do
   ct <- generateStd (generator interpr)
   evalHistoryT (mark >> step) $ St
      { currentTerm     = ct
      , currentStrategy = unlabel (strategy interpr)
      }
 where
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
            case parser interpr cmd of
               Left (doc, msug) -> liftIO $ do
                  putStrLn "ERROR: syntax error"
                  putStrLn (showDoc interpr doc)
                  case msug of
                     Just a -> putStrLn $ "Did you mean " ++ prettyPrinter interpr a
                     _      -> return ()
               Right new -> do
                  let list = filter (equality interpr new . snd3) $ take 1000 $ intermediates strat current
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
checkRuleName = not . isMinorRule