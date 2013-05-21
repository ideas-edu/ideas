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
-- Encoding in HTML
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
import Ideas.Service.LinkManager
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Service.BasicServices
import Ideas.Service.StrategyInfo
import Ideas.Common.Strategy.Prefix
import Ideas.Text.OpenMath.Object
import Ideas.Text.OpenMath.FMP
import Ideas.Service.RulesInfo

htmlEncoder :: LinkManager -> DomainReasoner -> Exercise a -> TypedValue (Type a) -> HTML
htmlEncoder lm dr ex tv = 
   htmlPage "EncoderHTML" (Just (resource "ideas.css")) $ do
      divClass "header" $ do
         divClass  "ideas-logo" $ image $ resource "ideas.png"
         divClass  "ounl-logo"  $ image $ resource "ounl.png"
         spanClass "menuitem"   $ linkToIndex lm $ text "Index"
         spanClass "menuitem"   $ linkToExercises lm $ text "Exercises"
         spanClass "menuitem"   $ linkToServices lm $ text "Services"
      divClass "content" $
         encodeType lm ex tv
      divClass "footer" $
         text (fullVersion dr)
 where
   resource = urlForResource lm

encodeType :: LinkManager -> Exercise a -> Encoder (Type a) XMLBuilderM ()
encodeType lm ex (val ::: tp) = 
   case tp of 
      Iso iso t  -> do
         f <- equalM (Tag "DomainReasoner" tp) typed
         encodeIndex (f val)
       `mplus` do
         f <- equalM tp typed
         htmlDiagnosis lm ex (f val)
       `mplus`
         encodeType lm ex (to iso val ::: t)
      Tag _ t    -> encodeType lm ex (val ::: t)
      Pair t1 t2 -> do encodeType lm ex (fst val ::: t1)
                       br
                       encodeType lm ex (snd val ::: t2)
      t1 :|: t2  -> case val of
                       Left x  -> encodeType lm ex (x ::: t1)
                       Right x -> encodeType lm ex (x ::: t2)
      List (Const Service) -> encodeServiceList lm val
      List (Const SomeExercise) -> encodeExerciseList lm val
      List (Tag "RuleShortInfo" (Iso iso (Const Rule))) -> encodeRuleList lm ex (map (to iso) val)
      List (Pair (Tag "difficulty" t) (Const Context)) -> do
         f <- equalM (Tag "difficulty" t) typed
         encodeExampleList lm ex (map (first f) val)
      List (Const (Derivation (Pair (Const Rule) (Const Environment)) (Const Context))) ->
         encodeDerivationList lm ex val
      List t     -> do
         f <- equalM tp typed
         htmlAllFirsts lm ex (f val)
       `mplus` do
         f <- equalM tp typed
         htmlAllApplications lm ex (f val)
       `mplus`
         ul [ encodeType lm ex (x ::: t) | x <- val ]
      Const t    -> encodeConst lm ex (val ::: t)
      _ -> text $ "unknown: " ++ show tp

encodeConst :: LinkManager -> Exercise a -> Encoder (Const a) XMLBuilderM ()
encodeConst lm ex tv@(val ::: tp) =
   case tp of 
      Service      -> encodeService val
      Exercise     -> encodeExercise lm val
      Strategy     -> encodeStrategy lm ex val
      Rule         -> encodeRule ex val
      Derivation (Pair (Const Rule) (Const Environment)) (Const Context) ->
         exerciseHeader lm ex >> h2 "Derivation" >> htmlDerivation lm ex val
      Derivation t1 t2 -> htmlDerivationWith mempty
                             (\s -> encodeType lm ex (s ::: t1))
                             (\a -> encodeType lm ex (a ::: t2)) val
      Location     -> text $ "location: " ++ show val
      Environment  -> text $ "environment: " ++ show val
      State        -> exerciseHeader lm ex >> htmlInteractiveState lm val
      Context      -> text $ prettyPrinterContext ex val
      String       -> text val
      _ -> text $ show tv

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
   
encodeServiceList :: LinkManager -> [Service] -> HTMLBuilder
encodeServiceList lm srvs = do
   h1 "Services"
   forM_ (groupById srvs) $ \(i, s, xs) -> do
      h2 $ show i ++ ". " ++ s
      table False (map make xs)
 where
 
   make s = [ linkToService lm s (text (showId s)) >> 
              when (serviceDeprecated s) (italic (text " (deprecated)"))
            , text (description s)
            ]

encodeExerciseList :: LinkManager -> [Some Exercise] -> HTMLBuilder
encodeExerciseList lm exs = do 
   h1 "Exercises"
   forM_ (groupsWith f exs) $ \(i, dom, xs) -> do
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
             f (Some t) = text (show t)
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

encodeExercise :: LinkManager -> Exercise a -> HTMLBuilder
encodeExercise lm ex = do
   exerciseHeader lm ex
   generalInfo
   h2 "Example exercises"
   ul $ [ para $ linkToExamples lm ex $ text "list of examples"
        | not (null (examples ex))
        ] ++
        [ para $ do
             text "generate exercise: "
             sequence_ $ intersperse (text ", ")
                [ linkToRandomExample lm ex d $ text $ show d
                | d <- [VeryEasy .. VeryDifficult]
                ]
        | isJust (randomExercise ex)
        ] ++
        [ para $ submitStateInfo lm ex
        | not (isStatic lm)
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

exerciseHeader :: LinkManager -> Exercise a -> HTMLBuilder
exerciseHeader lm ex = do
   exerciseMenu lm ex
   h1 $ "Exercise " ++ showId ex
   htmlDescription ex

keyValueTable :: [(String, HTMLBuilder)] -> HTMLBuilder
keyValueTable = 
   let f (s, a) = [bold (text s), a]
   in para . table False . map f 

exerciseMenu :: LinkManager -> Exercise a -> HTMLBuilder
exerciseMenu lm ex = divClass "menubox" $ do
   bold (text "Exercise")
   ul [ linkToExercise lm ex    $ text "information"
      , linkToStrategy lm ex    $ text "strategy"
      , linkToRules lm ex       $ text "rules"
      , linkToExamples lm ex    $ text "examples"
      , linkToDerivations lm ex $ text "derivations"
      ]

encodeStrategy :: LinkManager -> Exercise a -> Strategy (Context a) -> HTMLBuilder 
encodeStrategy lm ex s = do
   exerciseHeader lm ex
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

encodeRuleList :: LinkManager -> Exercise a -> [Rule (Context a)] -> HTMLBuilder
encodeRuleList lm ex rs = do
   exerciseHeader lm ex
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
   f r  = [ linkToRule lm ex r $ ttText (showId r)
          , text $ show $ length $ getRefs r
          , text $ showBool $ r `elem` used
          , when (isRewriteRule r) $
               ruleToHTML (Some ex) r
          ]

encodeRule :: Exercise a -> Rule (Context a) -> HTMLBuilder
encodeRule ex r = do
   h1 $ "Rule " ++ showId r
   htmlDescription r
   let commas  = text . intercalate ", "
       idList  = commas . map showId
       refList = commas . map show . getRefIds
   para $ keyValueTable
      [ ("Parameters", refList r)
      , ("Buggy", text $ showBool (isBuggy r))
      , ("Rewrite rule", text $ showBool (isRewriteRule r))
      , ("Siblings", idList $ ruleSiblings r)
      ]
   when (isRewriteRule r) $ do
      h2 "Rewrite rule"
      ruleToHTML (Some ex) r

   -- FMPS
   let xs = getRewriteRules (transformation r)
   unless (null xs) $ do
      h2 "Formal Mathematical Properties"
      forM_ xs $ \(Some rr) -> para $ do
         let fmp = rewriteRuleToFMP (not $ isBuggy r) rr
         highlightXML False $ makeXML "FMP" $
            builder (omobj2xml (toObject fmp))

encodeExampleList :: LinkManager -> Exercise a -> [(Difficulty, Context a)] -> HTMLBuilder
encodeExampleList lm ex pairs = do
   exerciseHeader lm ex
   h2 "Examples"
   forM_ (orderedGroupsWith show fst pairs) $ \(_, s, xs) -> do
      h3 $ s ++ " (" ++ show (length xs) ++ ")"
      (if isStatic lm then ul else sequence_) (map make xs)
 where
   make (_, x) = para $ do
      unless (isStatic lm) $ do
         let st = emptyStateContext ex x
         spanClass "statelink" $ linkToState lm st $ element "img" $ do
            "src" .=. "external.png"
            "width" .=. "15"
      text (prettyPrinterContext ex x) 

encodeDerivationList :: LinkManager -> Exercise a -> [Derivation (Rule (Context a), Environment) (Context a)] -> HTMLBuilder
encodeDerivationList lm ex ds = do
   exerciseHeader lm ex
   h2 "Derivations"
   forM_ (zip [1::Int ..] ds) $ \(i, d) -> do
      h3 $ show i ++ "."
      htmlDerivation lm ex d
      
htmlDerivation :: LinkManager -> Exercise a -> Derivation (Rule (Context a), Environment) (Context a) -> HTMLBuilder
htmlDerivation lm ex d = 
   htmlDerivationWith before forStep forTerm (derivationDiffEnv d)
 where
   before = do
      stateLink lm (emptyStateContext ex (firstTerm d))
   forStep ((r, env1), env2) = do 
      spanClass "derivation-step" $ do
         unescaped "&#8658; "
         text $ unqualified r
         let showEnv e = unless (noBindings e) $ text $ ", " ++ show e
         showEnv env1 -- local environment
         showEnv env2 -- global environment (diff)
   forTerm a = do 
      divClass "term" $ text $ prettyPrinterContext ex a

htmlState :: LinkManager -> State a -> HTMLBuilder
htmlState lm state = do
   para $ divClass "state" $ do
      stateLink lm state
      divClass "term" $ text $ prettyPrinterContext (exercise state) (stateContext state)
      text $ "ready: " ++ showBool (ready state)

stateLink :: LinkManager -> State a -> HTMLBuilder
stateLink lm st = unless (isStatic lm) $ do
   spanClass "derivation-statelink" $ linkToState lm st $ 
      element "img" $ do
         "src" .=. urlForResource lm "external.png"
         "width" .=. "15"

htmlInteractiveState :: LinkManager -> State a -> HTMLBuilder
htmlInteractiveState lm state = do
   htmlState lm state
   h2 "Feedback"
   submitDiagnose lm state
   ul [ linkToFirsts lm state $ text "allfirsts"
      , linkToApplications lm state $ text "allapplications"
      , linkToDerivation lm state $ text "derivation"
      ]
   unless (noBindings state) $ do
      h2 "Environment"
      text $ show $ environment state
   forM_ (zip [1::Int ..] (statePrefixes state)) $ \(i, pr) -> do
      h2 $ "Prefix " ++ show i
      let prSteps = prefixToSteps pr
          count p = text $ show $ length $ filter p prSteps
          enter   = spanClass "step-enter" . text . show
      keyValueTable 
         [ ("steps", count (const True))
         , ("rules", count isRuleStep)
         , ("major rules", count isMajor)
         , ("active labels", ul $ map enter $ activeLabels pr)
         ]
      sequence_ $ intersperse (text ", ") $ map htmlStep prSteps
   
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

htmlAllFirsts :: LinkManager -> Exercise a -> [(StepInfo a, State a)] -> HTMLBuilder
htmlAllFirsts lm ex xs = do
   exerciseHeader lm ex
   h2 "All firsts"
   ul (map (uncurry make) xs) 
 where
   make (r, loc, env) s = do
      keyValueTable 
         [ ("Rule", text $ showId r)
         , ("Location", text $ show loc)
         , ("Environment", text $ show env)
         ]
      htmlState lm s

htmlAllApplications :: LinkManager -> Exercise a -> [(Rule (Context a), Location, State a)] -> HTMLBuilder
htmlAllApplications lm ex xs = do
   exerciseHeader lm ex
   h2 "All applications"
   ul (map make xs) 
 where
   make (r, loc, s) = do
      keyValueTable 
         [ ("Rule", text $ showId r)
         , ("Location", text $ show loc)
         -- , ("Environment", text $ show env)
         ]
      unless (isBuggy r) (htmlState lm s)

htmlDiagnosis :: LinkManager -> Exercise a -> Diagnosis a -> HTMLBuilder
htmlDiagnosis lm ex diagnosis = do
   exerciseHeader lm ex
   case diagnosis of 
      Buggy _ r -> do 
         spanClass "error" $ text $ "Not equivalent: buggy rule " ++ show r
      NotEquivalent -> 
         spanClass "error" $ text "Not equivalent"
      Similar _ s -> do
         h2 "Similar term"
         htmlInteractiveState lm s
      Expected _ s r -> do
         h2 $ "Expected (" ++ show r ++ ")"
         htmlInteractiveState lm s
      Detour _ s _ r -> do
         h2 $ "Detour (" ++ show r ++ ")"
         htmlInteractiveState lm s
      Correct _ s -> do
         h2 "Correct"
         htmlInteractiveState lm s

htmlDescription :: HasId a => a -> HTMLBuilder
htmlDescription a = unless (null (description a)) $ 
   para $ do
      bold $ text "Description"
      br
      spanClass "description" $ text $ description a

submitForm :: HTMLBuilder -> HTMLBuilder
submitForm this = element "form" $ do
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
submitStateInfo :: LinkManager -> Exercise a -> HTMLBuilder
submitStateInfo lm ex = do
   submitForm (text "other exercise: ")
   submitRequest lm request
 where
   request = "<request service='stateinfo' exerciseid='" ++ showId ex 
          ++ "' encoding='html'><state><expr>\" + getTerm() + \"</expr></state></request>"

-- diagnose service
submitDiagnose :: LinkManager -> State a -> HTMLBuilder
submitDiagnose lm st = do
   submitForm mempty
   submitRequest lm request
 where
   request = "<request service='diagnose' exerciseid='" ++ showId (exercise st)
          ++ "' encoding='html'>" ++ ststr ++ "<expr>\"  + getTerm() + \"</expr></request>"
          
   ststr   = case fromBuilder (stateToXML st) of
                Just el -> concatMap f (show el)
                Nothing -> ""
                
   f '\\' = "\\\\"
   f c = [c]
 
submitRequest :: LinkManager -> String -> HTMLBuilder
submitRequest lm request = submitURL $ 
   quote (urlForRequest lm) ++ "+encodeURIComponent(" ++ quote request ++ ")"

quote :: String -> String 
quote s = '"' : s ++ ['"']

-- Inject two JavaScript functions for handling the input form
submitURL :: String -> HTMLBuilder
submitURL url = element "script" $ do
   "type" .=. "text/javascript"
   unescaped $ 
      "function getTerm() {\
      \   var s = document.myform.myterm.value;\
      \   var result = '';\
      \   for (var i=0;i<s.length;i++) {\
      \      if (s[i]=='<') result+='&lt;';\
      \      else if (s[i]=='&') result+='&amp;';\
	   \      else result+=s[i];\
      \   }\
      \   return result;\
      \}\
      \function submitTerm() {\
      \   document.myform.action = " ++ url ++ ";\
      \}"