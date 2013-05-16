{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Services using XML notation
--
-----------------------------------------------------------------------------
module Ideas.Service.EncoderHTML (htmlEncoder) where

import Ideas.Common.Utils
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Ideas.Common.Library hiding (ready)
import Ideas.Documentation.RulePresenter
import Ideas.Text.XML
import Ideas.Text.HTML
import Ideas.Service.Diagnose
import Ideas.Service.DomainReasoner
import Ideas.Service.Evaluator
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Service.EncoderXML
import Ideas.Service.BasicServices
import Ideas.Service.StrategyInfo
import Ideas.Common.Strategy.Prefix

htmlEncoder :: DomainReasoner -> Exercise a -> TypedValue (Type a) -> HTML
htmlEncoder dr ex tv = htmlPage "EncoderHTML" (Just "ideas.css") $ do
   divClass "header" $ do
      divClass "ideas-logo" $ image "ideas.png"
      divClass "ounl-logo" $ image "ounl.png"
      spanClass "menuitem" $ linkToIndex lm $ text "Index"
      spanClass "menuitem" $ linkToExercises lm $ text "Exercises"
      spanClass "menuitem" $ linkToServices lm $ text "Services"
   divClass "content" $
      encodeType ex tv
   divClass "footer" $
      text (fullVersion dr)

encodeType :: Exercise a -> Encoder (Type a) XMLBuilderM ()
encodeType ex (val ::: tp) = 
   case tp of 
      Iso iso t  -> do
         f <- equalM (Tag "DomainReasoner" tp) typed
         encodeIndex (f val)
       `mplus` do
         f <- equalM tp typed
         htmlDiagnosis ex (f val)
       `mplus`
         encodeType ex (to iso val ::: t)
      Tag s t    -> encodeType ex (val ::: t)
      Pair t1 t2 -> do encodeType ex (fst val ::: t1)
                       br
                       encodeType ex (snd val ::: t2)
      t1 :|: t2  -> case val of
                       Left x  -> encodeType ex (x ::: t1)
                       Right x -> encodeType ex (x ::: t2)
      List (Const Service) -> encodeServiceList val
      List (Const SomeExercise) -> encodeExerciseList val
      List (Tag "RuleShortInfo" (Iso iso (Const Rule))) -> encodeRuleList ex (map (to iso) val)
      List (Pair (Tag "difficulty" t) (Const Context)) -> do
         f <- equalM (Tag "difficulty" t) typed
         encodeExampleList ex (map (first f) val)
      List (Const (Derivation (Pair (Const Rule) (Const Environment)) (Const Context))) ->
         encodeDerivationList ex val
      List t     -> do
         f <- equalM tp typed
         htmlAllFirsts ex (f val)
       `mplus` do
         f <- equalM tp typed
         htmlAllApplications ex (f val)
       `mplus`
         ul [ encodeType ex (x ::: t) | x <- val ]
      Const t    -> encodeConst ex (val ::: t)
      _ -> text $ "unknown: " ++ show tp
      
encodeConst :: Exercise a -> Encoder (Const a) XMLBuilderM ()
encodeConst ex tv@(val ::: tp) =
   case tp of 
      Service      -> encodeService val
      Exercise     -> encodeExercise val
      Strategy     -> encodeStrategy ex val
      Rule         -> text $ "ruleid: " ++ showId val
      Derivation (Pair (Const Rule) (Const Environment)) (Const Context) ->
         exerciseHeader ex >> h2 "Derivation" >> htmlDerivation ex val
      Derivation t1 t2 -> htmlDerivationWith mempty
                             (\s -> encodeType ex (s ::: t1))
                             (\a -> encodeType ex (a ::: t2)) val
      SomeExercise -> case val of
                         Some ex -> text $ "exerciseid: " ++ showId ex
      Location     -> text $ "location: " ++ show val
      Environment  -> text $ "environment: " ++ show val
      State        -> exerciseHeader ex >> htmlInteractiveState val
      Context      -> text $ prettyPrinterContext ex val
      String       -> text val
      _ -> text $ show tv

data LinkManager a = LinkManager
   { urlForIndex     :: String
   , urlForServices  :: String
   , urlForService   :: Service -> String
   , urlForExercises :: String
   , urlForExercise   :: Exercise a -> String
   , urlForExamples   :: Exercise a  -> String
   , urlForRandomExample :: Difficulty -> Exercise a -> String
   , urlForStrategy  :: Exercise a -> String
   , urlForRules     :: Exercise a -> String
   , urlForDerivations :: Exercise a -> String
--   , urlForRule      :: Exercise a -> Rule (Context a) -> String
   , urlForState      :: State a -> String
   , urlForAllFirsts  :: State a -> String
   , urlForAllApplications :: State a -> String
   , urlForDerivation :: State a -> String
   }
   
linkToIndex :: LinkManager a -> HTMLBuilder -> HTMLBuilder
linkToIndex = link . escapeInURL . urlForIndex

linkToService :: LinkManager a -> Service -> HTMLBuilder -> HTMLBuilder
linkToService lm = link . escapeInURL . urlForService lm
 
linkToServices :: LinkManager a -> HTMLBuilder -> HTMLBuilder
linkToServices = link . escapeInURL . urlForServices

linkToExercises :: LinkManager a -> HTMLBuilder -> HTMLBuilder
linkToExercises = link . escapeInURL . urlForExercises
 
linkToExercise :: LinkManager a -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToExercise lm = link . escapeInURL . urlForExercise lm

linkToStrategy :: LinkManager a -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToStrategy lm = link . escapeInURL . urlForStrategy lm

linkToRules :: LinkManager a -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToRules lm = link . escapeInURL . urlForRules lm

linkToDerivations :: LinkManager a -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToDerivations lm = link . escapeInURL . urlForDerivations lm

linkToExamples :: LinkManager a -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToExamples lm = link . escapeInURL . urlForExamples lm
   
linkToRandomExample :: LinkManager a -> Difficulty -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToRandomExample lm d = link . escapeInURL . urlForRandomExample lm d
   
linkToState :: LinkManager a -> State a -> HTMLBuilder -> HTMLBuilder
linkToState lm = link . escapeInURL . urlForState lm
   
linkToAllFirsts :: LinkManager a -> State a -> HTMLBuilder -> HTMLBuilder
linkToAllFirsts lm = link . escapeInURL . urlForAllFirsts lm

linkToAllApplications :: LinkManager a -> State a -> HTMLBuilder -> HTMLBuilder
linkToAllApplications lm = link . escapeInURL . urlForAllApplications lm

linkToDerivation :: LinkManager a -> State a -> HTMLBuilder -> HTMLBuilder
linkToDerivation lm = link . escapeInURL . urlForDerivation lm

lm :: LinkManager a
lm = LinkManager 
   { urlForIndex = 
        url  ++ show indexRequest
   , urlForServices =
        url ++ show serviceListRequest
   , urlForService = \srv ->
        url ++ show (serviceInfoRequest srv)
   , urlForExercises = 
        url ++ show exerciseListRequest
   , urlForExercise = \ex ->
        url ++ show (exerciseInfoRequest ex)
   , urlForStrategy = \ex ->
        url ++ show (strategyInfoRequest ex)
   , urlForRules = \ex ->
        url ++ show (rulelistRequest ex)
   , urlForExamples = \ex ->
        url ++ show (examplesRequest ex)
   , urlForDerivations = \ex -> 
        url ++ show (exampleDerivationsRequest ex)
   , urlForRandomExample = \d ex ->
        url ++ show (generateRequest d ex)
   , urlForState = \state -> 
        url ++ show (stateInfoRequest state)
   , urlForAllFirsts = \state ->
        url ++ show (allFirstsRequest state)
   , urlForAllApplications = \state -> 
        url ++ show (allApplicationsRequest state)
   , urlForDerivation = \state ->
        url ++ show (derivationRequest state)
   }
 where
   url = "http://localhost/ideas.cgi?input="

encodeIndex :: DomainReasoner -> HTMLBuilder
encodeIndex dr = do
   h1 $ "Domain reasoner " ++ showId dr
   htmlDescription dr
   keyValueTable
      [ ("version", text $ fullVersion dr)
      , ("exercises", text $ show $ length $ exercises dr)
      , ("services", text $ show $ length $ services dr)
      ]
   unless (null $ aliases dr) $ do
      h2 "Exercise aliases"
      table True $ 
         [ text "alias", text "exercise"] :
         [ [text (showId a), text (showId b)]
         | (a, b) <- aliases dr
         ]
   unless (null $ scripts dr) $ do
      h2 "Feedback scripts"
      table True $ 
         [ text "exercise", text "script"] :
         [ [text (showId a), text file]
         | (a, file) <- scripts dr
         ]
   
encodeServiceList :: [Service] -> HTMLBuilder
encodeServiceList list = do
   h1 "Services"
   forM_ (groupById list) $ \(i, s, xs) -> do
      h2 $ show i ++ ". " ++ s
      table False (map make xs)
 where
 
   make s = [ linkToService lm s (text (showId s)) >> 
              when (serviceDeprecated s) (italic (text " (deprecated)"))
            , text (description s)
            ]

encodeExerciseList :: [Some Exercise] -> HTMLBuilder
encodeExerciseList list = do 
   h1 "Exercises"
   forM_ (groupsWith f list) $ \(i, dom, xs) -> do
      h2 (show i ++ ". " ++ dom)
      table False (map make xs)
 where
   f :: Some Exercise -> String
   f (Some ex) = fromMaybe "" (listToMaybe (qualifiers (getId ex)))
 
   make :: Some Exercise -> [HTMLBuilder]
   make (Some ex) = 
      [ linkToExercise lm ex $ text $ showId ex
      , text $ map toLower $ show $ status ex
      , text $ description ex
      ]

groupById :: HasId a => [a] -> [(Int, String, [a])]
groupById = groupsWith (fromMaybe "" . listToMaybe . qualifiers . getId)

groupsWith :: (a -> String) -> [a] -> [(Int, String, [a])]
groupsWith = orderedGroupsWith id 

orderedGroupsWith :: Ord b => (b -> String) -> (a -> b) -> [a] -> [(Int, String, [a])]
orderedGroupsWith showf get = 
   zipWith f [1..] . groupBy eq . sortBy (comparing get)
 where
   eq x y = get x == get y
   f i xs = (i, showf (get (head xs)), xs)

encodeService :: Service -> HTMLBuilder
encodeService srv = do
   h1 $ "Service " ++ showId srv
   when (serviceDeprecated srv) $ 
      para $ spanClass "warning" $ text "Warning: this service is deprecated"
   htmlDescription srv
   case serviceFunction srv of
      _ ::: tp -> do 
         let (xs, ys) = inputOutputTypes tp
             f :: Some (Type a) -> HTMLBuilder
             f (Some (t :|: Unit)) = text (show t) >> italic (text " (optional)")
             f (Some tp) = text (show tp)
         unless (null xs) $ para $ do 
            bold $ text "Input"
            ul (map f xs)
         unless (null ys) $ para $ do
            bold $ text "Output"
            ul (map f ys)

inputOutputTypes :: Type a t -> ([Some (Type a)], [Some (Type a)])
inputOutputTypes tp =
   case tp of
      Iso _ t   -> inputOutputTypes t
      t1 :-> t2 -> let (xs, ys) = inputOutputTypes t2 
                   in (productType t1 ++ xs, ys)
      Const String :|: t -> ([], productType t)
      _         -> ([], productType tp)

productType :: Type a t -> [Some (Type a)]
productType tp = 
   case tp of
      Iso _ t    -> productType t
      Pair t1 t2 -> productType t1 ++ productType t2
      Unit       -> []
      _          -> [Some tp]

encodeExercise :: Exercise a -> HTMLBuilder
encodeExercise ex = do
   exerciseHeader ex
   generalInfo
   h2 "Example exercises"
   ul $ [ para $ linkToExamples lm ex $ text "list of examples"
        | not (null (examples ex))
        ] ++
        [ para $ do
             text "generate exercise: "
             sequence_ $ intersperse (text ", ")
                [ linkToRandomExample lm d ex $ text $ show d
                | d <- [VeryEasy .. VeryDifficult]
                ]
        | isJust (randomExercise ex)
        ] ++
        [ para $ do 
             myForm (text "other exercise: ")
             submitScript ex
        ]
 where
   generalInfo = keyValueTable
      [ ("Code",   ttText (showId ex))
      , ("Status", text (show $ status ex))
      , ("Strategy", linkToStrategy lm ex $ text (showId $ strategy ex))
      , ("Rules", text (show nrOfSoundRules))
      , ("Buggy rules", text (show nrOfBuggyRules))
      , ("OpenMath support", text $ showBool $ isJust $ hasTermView ex)
      , ("Restartable strategy", text $ showBool $ canBeRestarted ex)
      , ("Exercise generator", text $ showBool $ isJust $ randomExercise ex)
      , ("Examples", text $ show $ length $ examples ex)
      ]
  
   (nrOfBuggyRules, nrOfSoundRules) = 
      mapBoth length (partition isBuggy (ruleset ex))

exerciseHeader :: Exercise a -> HTMLBuilder
exerciseHeader ex = do
   exerciseMenu ex
   h1 $ "Exercise " ++ showId ex
   htmlDescription ex

keyValueTable :: [(String, HTMLBuilder)] -> HTMLBuilder
keyValueTable = 
   let f (s, a) = [bold (text s), a]
   in para . table False . map f 

exerciseMenu :: Exercise a -> HTMLBuilder
exerciseMenu ex = divClass "menubox" $ do
   bold (text "Exercise")
   ul [ linkToExercise lm ex    $ text "information"
      , linkToStrategy lm ex    $ text "strategy"
      , linkToRules lm ex       $ text "rules"
      , linkToExamples lm ex    $ text "examples"
      , linkToDerivations lm ex $ text "derivations"
      ]

encodeStrategy :: Exercise a -> Strategy (Context a) -> HTMLBuilder 
encodeStrategy ex s = do
   exerciseHeader ex
   h2 "Strategy"
   highlightXML True (strategyToXML s)
   h2 "Locations"
   let f (loc, a) =
          [text (show loc), indent (length loc) >> text (showId a)]
       indent n = text (replicate (3*n) '.')
   table True
      ( [text "Location", text "Label"]
      : map f (strategyLocations (strategy ex))
      )

showBool :: Bool -> String
showBool b = if b then "yes" else "no"

encodeRuleList :: Exercise a -> [Rule (Context a)] -> HTMLBuilder
encodeRuleList ex rs = do
   exerciseHeader ex
   h2 $ "Rules for " ++ showId ex
   table True (header:map f rs2)
   h2 $ "Buggy rules for " ++ showId ex
   table True (header:map f rs1)
 where
   header = [ text "Rule name", text "Args"
            , text "Used", text "Rewrite rule"
            ]
   (rs1, rs2) = partition isBuggy rs
   used = rulesInStrategy (strategy ex)
   f r  = [ ttText (showId r)
          , text $ show $ length $ getRefs r
          , text $ showBool $ r `elem` used
          , when (isRewriteRule r) $
               ruleToHTML (Some ex) r
          ]

encodeExampleList :: Exercise a -> [(Difficulty, Context a)] -> HTMLBuilder
encodeExampleList ex list = do
   exerciseHeader ex
   h2 "Examples"
   forM_ (orderedGroupsWith show fst list) $ \(_, s, xs) -> do
      h3 $ s ++ " (" ++ show (length xs) ++ ")"
      forM_ xs $ \(_, x) -> para $ do
         let st = emptyStateContext ex x
         spanClass "statelink" $ linkToState lm st $ element "img" $ do
            "src" .=. "external.png"
            "width" .=. "15"
         text (prettyPrinterContext ex x) 

encodeDerivationList :: Exercise a -> [Derivation (Rule (Context a), Environment) (Context a)] -> HTMLBuilder
encodeDerivationList ex ds = do
   exerciseHeader ex
   h2 "Derivations"
   forM_ (zip [1..] ds) $ \(i, d) -> do
      h3 $ show i ++ "."
      htmlDerivation ex d
      
htmlDerivation :: Exercise a -> Derivation (Rule (Context a), Environment) (Context a) -> HTMLBuilder
htmlDerivation ex d = 
   htmlDerivationWith (before d) forStep forTerm (derivationDiffEnv d)
 where
   before d = do
      let st = emptyStateContext ex (firstTerm d)
      spanClass "derivation-statelink" $ linkToState lm st $ element "img" $ do
            "src" .=. "external.png"
            "width" .=. "15"
   forStep ((r, env1), env2) = do 
      spanClass "derivation-step" $ do
         unescaped "&#8658; "
         text $ unqualified r
         let showEnv e = unless (noBindings e) $ text $ ", " ++ show e
         showEnv env1 -- local environment
         showEnv env2 -- global environment (diff)
   forTerm a = do 
      divClass "term" $ text $ prettyPrinterContext ex a

htmlState :: State a -> HTMLBuilder
htmlState state = do
   para $ divClass "state" $ do
      spanClass "derivation-statelink" $ linkToState lm state $ element "img" $ do
            "src" .=. "external.png"
            "width" .=. "15"
      divClass "term" $ text $ prettyPrinterContext (exercise state) (stateContext state)
      text $ "ready: " ++ showBool (ready state)

htmlInteractiveState :: State a -> HTMLBuilder
htmlInteractiveState state = do
   htmlState state
   h2 "Feedback"
   submitScript2 state
   myForm mempty
   ul [ linkToAllFirsts lm state $ text "allfirsts"
      , linkToAllApplications lm state $ text "allapplications"
      , linkToDerivation lm state $ text "derivation"
      ]
   unless (noBindings state) $ do
      h2 "Environment"
      text $ show $ environment state
   forM_ (zip [1..] (statePrefixes state)) $ \(i, pr) -> do
      h2 $ "Prefix " ++ show i
      let steps   = prefixToSteps pr
          count p = text $ show $ length $ filter p steps
          enter   = spanClass "step-enter" . text . show
      keyValueTable 
         [ ("steps", count (const True))
         , ("rules", count isRuleStep)
         , ("major rules", count isMajor)
         , ("active labels", ul $ map enter $ activeLabels pr)
         ]
      sequence_ $ intersperse (text ", ") $ map htmlStep steps
   
isRuleStep :: Step l a -> Bool
isRuleStep (RuleStep _ _) = True
isRuleStep _ = False
   
htmlStep :: Show l => Step l a -> HTMLBuilder
htmlStep (Enter l)      = spanClass "step-enter" $ text $ "enter " ++ show l
htmlStep (Exit  l)      = spanClass "step-exit"  $ text $ "exit " ++ show l
htmlStep (RuleStep _ r) = let s = if isMinor r then "minor" else "major"
                          in spanClass ("step-"++s) $ text $ showId r

htmlDerivationWith :: HTMLBuilder -> (s -> HTMLBuilder) -> (a -> HTMLBuilder) -> Derivation s a -> HTMLBuilder
htmlDerivationWith before forStep forTerm d = 
   divClass "derivation" $ do 
      before
      forTerm (firstTerm d)
      forM_ (triples d) $ \(_, s, a) -> do
         forStep s
         forTerm a

htmlAllFirsts :: Exercise a -> [(StepInfo a, State a)] -> HTMLBuilder
htmlAllFirsts ex xs = do
   exerciseHeader ex
   h2 "All firsts"
   ul (map (uncurry make) xs) 
 where
   make (r, loc, env) s = do
      keyValueTable 
         [ ("Rule", text $ showId r)
         , ("Location", text $ show loc)
         , ("Environment", text $ show env)
         ]
      htmlState s

htmlAllApplications :: Exercise a -> [(Rule (Context a), Location, State a)] -> HTMLBuilder
htmlAllApplications ex xs = do
   exerciseHeader ex
   h2 "All applications"
   ul (map make xs) 
 where
   make (r, loc, s) = do
      keyValueTable 
         [ ("Rule", text $ showId r)
         , ("Location", text $ show loc)
         -- , ("Environment", text $ show env)
         ]
      unless (isBuggy r) (htmlState s)

htmlDiagnosis :: Exercise a -> Diagnosis a -> HTMLBuilder
htmlDiagnosis ex diagnosis = do
   exerciseHeader ex
   case diagnosis of 
      Buggy _ r -> do 
         spanClass "error" $ text $ "Not equivalent: buggy rule " ++ show r
      NotEquivalent -> 
         spanClass "error" $ text "Not equivalent"
      Similar _ s -> do
         h2 "Similar term"
         htmlInteractiveState s
      Expected _ s r -> do
         h2 $ "Expected (" ++ show r ++ ")"
         htmlInteractiveState s
      Detour _ s _ r -> do
         h2 $ "Detour (" ++ show r ++ ")"
         htmlInteractiveState s
      Correct _ s -> do
         h2 "Correct"
         htmlInteractiveState s

stateToXML :: State a -> XMLBuilder
stateToXML st = encodeState False enc st
 where
   enc = element "expr" . text . prettyPrinter (exercise st)

htmlDescription :: HasId a => a -> HTMLBuilder
htmlDescription a = unless (null (description a)) $ 
   para $ do
      bold $ text "Description"
      br
      spanClass "description" $ text $ description a
   

indexRequest :: XML
indexRequest = makeXML "request" $ do
   "service"    .=. "index"
   "encoding"   .=. "html"

exerciseListRequest :: XML
exerciseListRequest = makeXML "request" $ do
   "service"    .=. "exerciselist"
   "encoding"   .=. "html"

serviceListRequest :: XML
serviceListRequest = makeXML "request" $ do
   "service"    .=. "servicelist"
   "encoding"   .=. "html"

serviceInfoRequest :: Service -> XML
serviceInfoRequest srv = makeXML "request" $ do
   "service"    .=. "serviceinfo"
   "encoding"   .=. "html"
   element "location" $ text $ showId srv

rulelistRequest :: Exercise a -> XML
rulelistRequest ex = makeXML "request" $ do
   "service"    .=. "rulelist"
   "exerciseid" .=. showId ex
   "encoding"   .=. "html"

examplesRequest :: Exercise a -> XML
examplesRequest ex = makeXML "request" $ do
   "service"    .=. "examples"
   "exerciseid" .=. showId ex
   "encoding"   .=. "html"

generateRequest :: Difficulty -> Exercise a -> XML
generateRequest d ex = makeXML "request" $ do
   "service"    .=. "generate"
   "exerciseid" .=. showId ex
   "difficulty" .=. show d -- !!
   "encoding"   .=. "html"

allFirstsRequest :: State a -> XML
allFirstsRequest state = makeXML "request" $ do
   "service"    .=. "allfirsts"
   "exerciseid" .=. showId (exercise state)
   "encoding"   .=. "html"
   stateToXML state

allApplicationsRequest :: State a -> XML
allApplicationsRequest state = makeXML "request" $ do
   "service"    .=. "allapplications"
   "exerciseid" .=. showId (exercise state)
   "encoding"   .=. "html"
   stateToXML state

derivationRequest :: State a -> XML
derivationRequest state = makeXML "request" $ do
   "service"    .=. "derivation"
   "exerciseid" .=. showId (exercise state)
   "encoding"   .=. "html"
   stateToXML state

stateInfoRequest :: State a -> XML
stateInfoRequest state = makeXML "request" $ do
   "service"    .=. "stateinfo"
   "exerciseid" .=. showId (exercise state)
   "encoding"   .=. "html"
   stateToXML state

exerciseInfoRequest :: Exercise a -> XML
exerciseInfoRequest ex = makeXML "request" $ do
   "service"    .=. "exerciseinfo"
   "exerciseid" .=. showId ex
   "encoding"   .=. "html"

strategyInfoRequest :: Exercise a -> XML
strategyInfoRequest ex = makeXML "request" $ do
   "service"    .=. "strategyinfo"
   "exerciseid" .=. showId ex
   "encoding"   .=. "html"

exampleDerivationsRequest :: Exercise a -> XML
exampleDerivationsRequest ex = makeXML "request" $ do
   "service"    .=. "examplederivations"
   "exerciseid" .=. showId ex
   "encoding"   .=. "html"

-- http://www.blooberry.com/indexdot/html/topics/urlencoding.htm
escapeInURL :: String -> String
escapeInURL = concatMap f
 where
   f '+' = "%2B"
   f '>' = "%3E"
   f '&' = "%26"
   f c   = [c]

myForm :: HTMLBuilder -> HTMLBuilder
myForm this = element "form" $ do
   "name"     .=. "myform" 
   "onsubmit" .=. "return submitTerm()" 
   "method"   .=. "post"
   this
   element "input" $ do
      "type" .=. " text"
      "name" .=. "myterm"
   element "input" $ do
      "type"  .=. "submit"
      "value" .=. "Submit"

-- stateinfo service
submitScript :: Exercise a -> HTMLBuilder
submitScript ex = element "script" $ do
   "type" .=. "text/javascript"
   unescaped (getTerm ++ code)
 where
   code    = "function submitTerm() {document.myform.action = \"" ++ action ++ "\");}"
   action  = "ideas.cgi?input=\" + encodeURIComponent(\"" ++ request
   request = "<request service='stateinfo' exerciseid='" ++ showId ex 
          ++ "' encoding='html'><state><expr>\" + getTerm() + \"</expr></state></request>"

          
          
-- diagnose service
submitScript2 :: State a -> HTMLBuilder
submitScript2 st = element "script" $ do
   "type" .=. "text/javascript"
   unescaped (getTerm ++ code)
 where
   code    = "function submitTerm() {document.myform.action = \"" ++ action ++ "\");}"
   action  = "ideas.cgi?input=\" + encodeURIComponent(\"" ++ request
   request = "<request service='diagnose' exerciseid='" ++ showId (exercise st)
          ++ "' encoding='html'>" ++ ststr ++ "<expr>\"  + getTerm() + \"</expr></request>"
          
   ststr   = case fromBuilder (stateToXML st) of
                Just el -> concatMap f (show el)
                Nothing -> ""
                
   f '\\' = "\\\\"
   f c = [c]
 
getTerm :: String  
getTerm = 
   "function getTerm() {\
   \   var s = document.myform.myterm.value;\
   \   var result = \"\";\
   \   for (var i=0;i<s.length;i++) {\
   \      if (s[i]=='<') result+=\"&lt;\";\
   \      else if (s[i]=='&') result+=\"&amp;\";\
	\      else result+=s[i];\
   \   }\
   \   return result;\
   \}"