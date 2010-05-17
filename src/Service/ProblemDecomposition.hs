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
module Service.ProblemDecomposition 
   ( problemDecomposition
   , Reply(..), replyToXML, xmlToRequest
   , ReplyError(..), ReplyOk(..), ReplyIncorrect(..)
   , replyType
   ) where

import Common.Apply
import Common.Context
import Common.Exercise
import Common.Derivation
import Common.Strategy hiding (not, repeat, fail)
import Common.Transformation 
import Common.Utils
import Data.Char
import Data.Maybe
import Service.Definitions hiding (State)
import Service.ExercisePackage (ExercisePackage, fromOpenMath, exercise)
import Service.TypedAbstractService (State(..), stepsremaining)
import Text.XML hiding (name)
import qualified Text.XML as XML
import Control.Monad
import Text.OpenMath.Object

replyError :: String -> String -> Reply a
replyError kind = Error . ReplyError kind

problemDecomposition :: State a -> StrategyLocation -> Maybe a -> Reply a
problemDecomposition (State pkg mpr requestedTerm) sloc answer 
   | isNothing $ subStrategy sloc (strategy ex) =
        replyError "request error" "invalid location for strategy"
   | otherwise =
   let pr = fromMaybe (emptyPrefix $ strategy ex) mpr in
         case (runPrefixLocation sloc pr requestedTerm, maybe Nothing (Just . inContext ex) answer) of            
            ([], _) -> replyError "strategy error" "not able to compute an expected answer"
            (answers, Just answeredTerm)
               | not (null witnesses) ->
                    Ok ReplyOk
                       { repOk_Code     = pkg
                       , repOk_Location = nextTaskLocation sloc $ nextMajorForPrefix newPrefix (fst $ head witnesses)
                       , repOk_Context  = show newPrefix ++ ";" ++ 
                                          show (getEnvironment $ fst $ head witnesses)
                       , repOk_Steps    = fromMaybe 0 $ stepsremaining $ State pkg (Just newPrefix) (fst $ head witnesses)
                       }
                  where 
                    witnesses   = filter (similarityCtx ex answeredTerm . fst) $ take 1 answers
                    newPrefix   = snd (head witnesses)            
            ((expected, prefix):_, maybeAnswer) ->
                    Incorrect ReplyIncorrect
                       { repInc_Code       = pkg
                       , repInc_Location   = subTaskLocation sloc loc
                       , repInc_Expected   = fromJust (fromContext expected)
                       , repInc_Derivation = derivation
                       , repInc_Arguments  = args
                       , repInc_Steps      = fromMaybe 0 $ stepsremaining $ State pkg (Just pr) requestedTerm
                       , repInc_Equivalent = maybe False (equivalenceContext ex expected) maybeAnswer
                       }
             where
               (loc, args) = firstMajorInPrefix pr prefix requestedTerm
               derivation  = 
                  let len      = length $ prefixToSteps pr
                      rules    = stepsToRules $ drop len $ prefixToSteps prefix
                      f (s, a) = (s, fromJust (fromContext a))
                  in map f (makeDerivation requestedTerm rules)
 where
   ex = exercise pkg
   
similarityCtx :: Exercise a -> Context a -> Context a -> Bool
similarityCtx ex a b = fromMaybe False $
   liftM2 (similarity ex) (fromContext a) (fromContext b)

-- | Continue with a prefix until a certain strategy location is reached. At least one
-- major rule should have been executed
runPrefixLocation :: StrategyLocation -> Prefix a -> a -> [(a, Prefix a)]
runPrefixLocation loc p0 = 
   concatMap (check . f) . derivations . 
   cutOnStep (stop . lastStepInPrefix) . prefixTree p0
 where
   f d = (last (terms d), if isEmpty d then p0 else last (steps d))
   stop (Just (End is _)) = is==loc
   stop _ = False
 
   check result@(a, p)
      | null rules            = [result]
      | all isMinorRule rules = runPrefixLocation loc p a
      | otherwise             = [result]
    where
      rules = stepsToRules $ drop (length $ prefixToSteps p0) $ prefixToSteps p

firstMajorInPrefix :: Prefix a -> Prefix a -> a -> (StrategyLocation, Args)
firstMajorInPrefix p0 prefix a = fromMaybe (topLocation, []) $ do
   let steps = prefixToSteps prefix
       newSteps = drop (length $ prefixToSteps p0) steps
   is <- firstLocation newSteps
   return (is, argumentsForSteps a newSteps)
 where
   firstLocation :: [Step a] -> Maybe StrategyLocation
   firstLocation [] = Nothing
   firstLocation (Begin is _:Step r:_) | isMajorRule r = Just is
   firstLocation (_:rest) = firstLocation rest
 
argumentsForSteps :: a -> [Step a] -> Args
argumentsForSteps a = flip rec a . stepsToRules
 where
   rec [] _ = []
   rec (r:rs) a
      | isMinorRule r  = concatMap (rec rs) (applyAll r a)
      | applicable r a = let ds = map (\(Some d) -> labelArgument d) (getDescriptors r)
                         in maybe [] (zip ds) (expectedArguments r a)
      | otherwise      = []
 
nextMajorForPrefix :: Prefix a -> a -> StrategyLocation
nextMajorForPrefix p0 a = fromMaybe topLocation $ do
   (_, p1)  <- safeHead $ runPrefixMajor p0 a
   let steps = prefixToSteps p1
   rec (reverse steps)
 where
   rec [] = Nothing
   rec (Begin is _:_) = Just is
   rec (End is _:_)   = Just is
   rec (_:rest)       = rec rest 
  
makeDerivation :: a -> [Rule a] -> [(String, a)]
makeDerivation _ []     = []
makeDerivation a (r:rs) = 
   let new = applyD r a
   in [ (name r, new) | isMajorRule r ] ++ makeDerivation new rs 
   
-- Copied from TypedAbstractService: clean me up
runPrefixMajor :: Prefix a -> a -> [(a, Prefix a)]
runPrefixMajor p0 = 
   map f . derivations . cutOnStep (stop . lastStepInPrefix) . prefixTree p0
 where
   f d = (last (terms d), if isEmpty d then p0 else last (steps d))
   stop (Just (Step r)) = isMajorRule r
   stop _ = False

------------------------------------------------------------------------
-- Requests

extractString :: String -> XML -> Either String String
extractString s = liftM getData . findChild s

xmlToRequest :: XML -> ExercisePackage a -> Either String (State a, StrategyLocation, Maybe a)
xmlToRequest xml pkg = do
   let ex = exercise pkg
   unless (XML.name xml == "request") $
      fail "XML document is not a request" 
   loc     <- optional (extractLocation "location" xml)
   term    <- extractExpr "term" xml
   context <- optional (extractString "context" xml)
   answer  <- optional (extractExpr "answer" xml)
   t  <- maybe (fail "invalid omobj") return (fromOpenMath pkg term)
   mt <- case answer of
            Nothing -> return Nothing 
            Just o  -> return $ fromOpenMath pkg o
   return
      ( State
           { exercisePkg = pkg
           , prefix  = case context of
                          Just s  -> Just $ getPrefix2 s (strategy ex)
                          Nothing -> Just $ emptyPrefix (strategy ex)
           , context = case context of 
                          Just s  -> putInContext2 ex s t
                          Nothing -> inContext ex t
           }
      , fromMaybe topLocation loc
      , mt
      )

-----------------------------------------------------------
putInContext2 :: Exercise a -> String -> a -> Context a
putInContext2 ex s = fromMaybe (inContext ex) $ do
   (_, s2) <- splitAtElem ';' s
   env     <- parseContext s2
   return (makeContext ex env)

getPrefix2 :: String -> LabeledStrategy (Context a) -> Prefix (Context a)
getPrefix2 s ls = fromMaybe (emptyPrefix ls) $ do
   (s1, _) <- splitAtElem ';' s
   is <- readM s1
   makePrefix is ls

optional :: Either String a -> Either String (Maybe a)
optional = Right . either (const Nothing) Just

extractLocation :: String -> XML -> Either String StrategyLocation
extractLocation s xml = do
   c <- findChild s xml
   case parseStrategyLocation (getData c) of
      Just loc -> return loc
      _        -> fail "invalid location"

extractExpr :: String -> XML -> Either String OMOBJ
extractExpr n xml =
   case findChild n xml of 
      Just expr -> 
         case children expr of 
            [this] -> xml2omobj this
            _ -> fail $ "error in " ++ show (n, xml)
      _ -> fail $ "error in " ++ show (n, xml)

-- Legacy code: remove!
parseContext :: String -> Maybe Environment
parseContext s
   | all isSpace s = 
        return emptyEnv
   | otherwise = do
        pairs <- mapM (splitAtElem '=') (splitsWithElem ',' s)
        let env = foldr (uncurry storeEnv) emptyEnv pairs
        return env
        
------------------------------------------------------------------------
-- Data types for replies

-- There are three possible replies: ok, incorrect, or an error in the protocol (e.g., a parse error)
data Reply a = Ok (ReplyOk a) | Incorrect (ReplyIncorrect a) | Error ReplyError

data ReplyOk a = ReplyOk
   { repOk_Code     :: ExercisePackage a
   , repOk_Location :: StrategyLocation
   , repOk_Context  :: String
   , repOk_Steps    :: Int
   }
   
data ReplyIncorrect a = ReplyIncorrect
   { repInc_Code       :: ExercisePackage a
   , repInc_Location   :: StrategyLocation
   , repInc_Expected   :: a
   , repInc_Derivation :: [(String, a)]
   , repInc_Arguments  :: Args
   , repInc_Steps      :: Int
   , repInc_Equivalent :: Bool
   }
 
data ReplyError = ReplyError
   { repErr_Kind    :: String
   , repErr_Message :: String
   }

type Args = [(String, String)]

------------------------------------------------------------------------
-- Conversion functions to XML

replyToXML :: (a -> OMOBJ) -> Reply a -> XML
replyToXML toOpenMath reply =
   case reply of
      Ok r        -> replyOkToXML r
      Incorrect r -> replyIncorrectToXML toOpenMath r 
      Error r     -> replyErrorToXML r

replyOkToXML :: ReplyOk a -> XML
replyOkToXML r = makeReply "ok" $ do
   element "strategy" (text $ show $ exerciseCode $ exercise $ repOk_Code r)
   element "location" (text $ show $ repOk_Location r)
   element "context"  (text $ repOk_Context r)
   element "steps"    (text $ show $ repOk_Steps r)

replyIncorrectToXML :: (a -> OMOBJ) -> ReplyIncorrect a -> XML
replyIncorrectToXML toOpenMath r = makeReply "incorrect" $ do
   element "strategy"   (text $ show $ exerciseCode $ exercise $ repInc_Code r)
   element "location"   (text $ show $ repInc_Location r)
   element "expected"   (builder $ omobj2xml $ toOpenMath $ repInc_Expected r)
   element "steps"      (text $ show $ repInc_Steps r)
   element "equivalent" (text $ show $ repInc_Equivalent r)
   
   unless (null $ repInc_Arguments r) $
       let f (x, y) = element "elem" $ do 
              "descr" .=. x 
              text y
       in element "arguments" $ mapM_ f (repInc_Arguments r)

   unless (null $  repInc_Derivation r) $
      let f (x,y) = element "elem" $ do 
             "ruleid" .=. x 
             builder (omobj2xml (toOpenMath y))
      in element "derivation" $ mapM_ f (repInc_Derivation r)

replyErrorToXML :: ReplyError -> XML
replyErrorToXML r = makeReply (repErr_Kind r) (text $ repErr_Message r)
   
makeReply :: String -> XMLBuilder -> XML
makeReply kind body = makeXML "reply" $ do
   "result" .=. kind
   body

replyType :: Type a (Reply a)
replyType = useSynonym replyTypeSynonym

replyTypeSynonym :: TypeSynonym a (Reply a)
replyTypeSynonym = typeSynonym "DecompositionReply" to from tp
 where
   to (Left (a, b, c, d)) = 
      Ok (ReplyOk a b c d)
   to (Right (Left ((a, b, c), (d, e, f, g)))) =
      Incorrect (ReplyIncorrect a b c d e f g)
   to (Right (Right (a, b))) = 
      Error (ReplyError a b)
   
   from (Ok (ReplyOk a b c d)) = Left (a, b, c, d)
   from (Incorrect (ReplyIncorrect a b c d e f g)) =
      Right (Left ((a, b, c), (d, e, f, g)))
   from (Error (ReplyError a b)) = Right (Right (a, b))
   
   tp  =  tuple4 ExercisePkg StrategyLoc String Int
      :|: Pair (tuple3 ExercisePkg StrategyLoc Term) 
               (tuple4 derTp argsTp Int Bool)
      :|: Pair String String
   derTp  = List (Pair String Term)
   argsTp = List (Pair String String)