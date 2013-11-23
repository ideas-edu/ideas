{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
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
module Ideas.Encoding.EncoderHTML (htmlEncoder) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Ideas.Common.Library hiding (ready)
import Ideas.Common.Utils
import Ideas.Encoding.Evaluator
import Ideas.Encoding.LinkManager
import Ideas.Encoding.RulePresenter
import Ideas.Encoding.RulesInfo
import Ideas.Encoding.StrategyInfo
import Ideas.Service.BasicServices
import Ideas.Service.Diagnose
import Ideas.Service.DomainReasoner
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Text.HTML
import Ideas.Text.OpenMath.FMP
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML

type HTMLEncoder a t = EncoderState (HTMLEncoderState a) t HTMLBuilder

data HTMLEncoderState a = HTMLEncoderState
   { getLinkManager :: LinkManager
   , getExercise    :: Exercise a
   }

htmlEncoder :: LinkManager -> DomainReasoner -> Exercise a -> TypedValue (Type a) -> HTMLPage
htmlEncoder lm dr ex tv =
   addCSS (urlForCSS lm "ideas.css") $
   htmlPage "Ideas: documentation pages" $ mconcat
      [ divClass "page-header" $ mconcat
           [ divClass  "ideas-logo" space
           , divClass  "ounl-logo"  space
           , spanClass "menuitem"   $ linkToIndex lm $ string "Index"
           , spanClass "menuitem"   $ linkToExercises lm $ string "Exercises"
           , spanClass "menuitem"   $ linkToServices lm $ string "Services"
           ]
      , divClass "page-content" $
           let hes = HTMLEncoderState lm ex in
           case runEncoderState (encodeType lm ex) hes tv of
              Left err -> string err
              Right ok -> ok
      , divClass "page-footer" $
           string (fullVersion dr)
      ]

encodeType :: LinkManager -> Exercise a -> HTMLEncoder a (TypedValue (Type a))
encodeType lm ex = msum
   [ encodeTyped encodeIndex
   , encodeTyped (exerciseHeader // () <> htmlDiagnosis)
   , encodeTyped (exerciseHeader // () <> encodeExampleList lm ex)
   , encodeTyped (exerciseHeader // () <> htmlAllFirsts)
   , encodeTyped (exerciseHeader // () <> htmlAllApplications)
   , encodeTyped (exerciseHeader // () <> encodeDerivation lm ex)
   , encodeTyped (exerciseHeader // () <> encodeDerivationList lm ex)
   , encoderFor $ \(val ::: tp) ->
        case tp of
           Iso iso t  -> encodeType lm ex // (to iso val ::: t)
           Tag _ t    -> encodeType lm ex // (val ::: t)
           Pair t1 t2 -> encodeType lm ex // (fst val ::: t1) <>
                         encodeType lm ex // (snd val ::: t2)
           t1 :|: t2  -> case val of
                            Left x  -> encodeType lm ex // (x ::: t1)
                            Right x -> encodeType lm ex // (x ::: t2)
           List (Const Service) -> encodeServiceList lm // val
           List (Const SomeExercise) -> encodeExerciseList lm // val
           List (Const Rule) -> (exerciseHeader // ()) <> encodeRuleList lm ex // val
           List t -> ul [ encodeType lm ex // (x ::: t) | x <- val ]
           Const t -> encodeConst lm ex // (val ::: t)
           _ -> string $ "unknown: " ++ show tp
   ]

encodeConst :: LinkManager -> Exercise a -> HTMLEncoder a (TypedValue (Const a))
encodeConst lm ex = encoderFor $ \tv@(val ::: tp) ->
   case tp of
      Service     -> encodeService // val
      Exercise    -> (exerciseHeader // ()) <> encodeExercise lm // val
      Strategy    -> (exerciseHeader // ()) <> encodeStrategy ex // val
      Rule        -> encodeRule ex // val
      State       -> (exerciseHeader // ()) <> (encodeState // val)
      Location    -> text val
      Environment -> text val
      Context     -> string $ prettyPrinterContext ex val
      String      -> string val
      _           -> text tv

encodeIndex :: HTMLEncoder a DomainReasoner
encodeIndex = simpleEncoder $ \dr -> mconcat
   [ h1 $ "Domain reasoner " ++ showId dr
   , htmlDescription dr
   , keyValueTable
        [ ("version", string $ fullVersion dr)
        , ("exercises", text $ length $ exercises dr)
        , ("services", text $ length $ services dr)
        ]
   , munless (null $ aliases dr) $
        h2 "Exercise aliases" <>
        table True (
           [ string "alias", string "exercise"] :
           [ [string (showId a), string (showId b)]
           | (a, b) <- aliases dr
           ])
   , munless (null $ scripts dr)
        h2 "Feedback scripts" <>
        table True (
           [ string "exercise", string "script"] :
           [ [string (showId a), string file]
           | (a, file) <- scripts dr
           ])
   ]

encodeServiceList :: LinkManager -> HTMLEncoder a [Service]
encodeServiceList lm = simpleEncoder $ \srvs ->
   h1 "Services" <>
   mconcat
      [ h2 (show i ++ ". " ++ s) <> table False (map make xs)
      | (i, s, xs) <- groupById srvs
      ]
 where
   make s = [ linkToService lm s (string (showId s)) <>
              mwhen (serviceDeprecated s) (italic (string " (deprecated)"))
            , string (description s)
            ]

encodeExerciseList :: LinkManager -> HTMLEncoder a [Some Exercise]
encodeExerciseList lm = simpleEncoder $ \exs ->
   h1 "Exercises" <>
   mconcat
      [ h2 (show i ++ ". " ++ dom) <> table False (map make xs)
      | (i, dom, xs) <- groupsWith f exs
      ]
 where
   f :: Some Exercise -> String
   f (Some ex) = fromMaybe "" (listToMaybe (qualifiers (getId ex)))

   make :: Some Exercise -> [HTMLBuilder]
   make (Some ex) =
      [ linkToExercise lm ex $ string $ showId ex
      , string $ map toLower $ show $ status ex
      , string $ description ex
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

encodeService :: HTMLEncoder a Service
encodeService = simpleEncoder $ \srv -> mconcat
   [ h1 $ "Service " ++ showId srv
   , mwhen (serviceDeprecated srv) $
        para $ spanClass "warning" $ string "Warning: this service is deprecated"
   , htmlDescription srv
   , case serviceFunction srv of
        _ ::: tp ->
           let (xs, ys) = inputOutputTypes tp
               f :: Some (Type a) -> HTMLBuilder
               f (Some (t :|: Unit)) = text t <> italic (string " (optional)")
               f (Some t) = text t
           in
              munless (null xs) (para $
                 bold (string "Input") <> ul (map f xs))
              <>
              munless (null ys) (para $
                 bold (string "Output") <> ul (map f ys))
   ]

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

encodeExercise :: LinkManager -> HTMLEncoder a (Exercise a)
encodeExercise lm = simpleEncoder $ \ex -> mconcat
   [ generalInfo ex
   , h2 "Example exercises"
   , ul $ [ para $ linkToExamples lm ex $ string "list of examples"
          | not (null (examples ex))
          ] ++
          [ para $ mconcat $
               string "generate exercise: " :
               intersperse (string ", ")
                  [ linkToRandomExample lm ex d $ text d
                  | d <- [VeryEasy .. VeryDifficult]
                  ]
          | isJust (randomExercise ex)
          ] ++
          [ para $ submitStateInfo lm ex
          | not (isStatic lm)
          ]
   ]
 where
   generalInfo ex = keyValueTable
      [ ("Code",   ttText (showId ex))
      , ("Status", text $ status ex)
      , ("Strategy", linkToStrategy lm ex $ string (showId $ strategy ex))
      , ("Rules", text nrOfSoundRules)
      , ("Buggy rules", text nrOfBuggyRules)
      , ("OpenMath support", bool $ isJust $ hasTermView ex)
      , ("Restartable strategy", bool $ canBeRestarted ex)
      , ("Exercise generator", bool $ isJust $ randomExercise ex)
      , ("Examples", text $ length $ examples ex)
      ]
    where
      (nrOfBuggyRules, nrOfSoundRules) =
         mapBoth length (partition isBuggy (ruleset ex))

exerciseHeader :: HTMLEncoder a ()
exerciseHeader = do
   ex <- withState getExercise
   mconcat
      [ exerciseMenu
      , h1 $ "Exercise " ++ showId ex
      , return $ htmlDescription ex
      ]

exerciseMenu :: HTMLEncoder a ()
exerciseMenu = divClass "menubox" $
   bold (string "Exercise") <>
   ul [ with linkToExercise    "information"
      , with linkToStrategy "   strategy"
      , with linkToRules       "rules"
      , with linkToExamples    "examples"
      , with linkToDerivations "derivations"
      ]
 where
   with f s = do
      lm <- withState getLinkManager
      ex <- withState getExercise
      return $ f lm ex $ string s

encodeStrategy :: Exercise a -> HTMLEncoder a (Strategy (Context a))
encodeStrategy ex = simpleEncoder $ \s -> mconcat
   [ h2 "Strategy"
   , highlightXML True (strategyToXML s)
   , h2 "Locations"
   , let f :: HasId a => ([Int], a) -> [HTMLBuilder]
         f (loc, a) = [text loc, indent (length loc) <> string (showId a)]
         indent n = string (replicate (3*n) '.')
     in table True
           ( [string "Location", string "Label"]
           : map f (strategyLocations (strategy ex))
           )
   ]

bool :: Bool -> HTMLBuilder
bool b = string (if b then "yes" else "no")

encodeRuleList :: LinkManager -> Exercise a -> HTMLEncoder a [Rule (Context a)]
encodeRuleList lm ex = simpleEncoder $ \rs ->
   let (rs1, rs2) = partition isBuggy rs
   in mconcat
         [ h2 $ "Rules for " ++ showId ex
         , table True (header:map f rs2)
         , h2 $ "Buggy rules for " ++ showId ex
         , table True (header:map f rs1)
         ]
 where
   header = [ string "Rule name", string "Args"
            , string "Used", string "Rewrite rule"
            ]
   used = rulesInStrategy (strategy ex)
   f r  = [ linkToRule lm ex r $ ttText (showId r)
          , text $ length $ getRefs r
          , bool $ r `elem` used
          , mwhen (isRewriteRule r) $
               ruleToHTML (Some ex) r
          ]

encodeRule :: Exercise a -> HTMLEncoder a (Rule (Context a))
encodeRule ex = simpleEncoder $ \r -> mconcat
   [ h1 $ "Rule " ++ showId r
   , htmlDescription r
   , let commas  = string . intercalate ", "
         idList  = commas . map showId
         refList = commas . map show . getRefIds
     in para $ keyValueTable
           [ ("Parameters", refList r)
           , ("Buggy", bool (isBuggy r))
           , ("Rewrite rule", bool (isRewriteRule r))
           , ("Siblings", idList $ ruleSiblings r)
           ]
   , mwhen (isRewriteRule r) $
        h2 "Rewrite rule" <> ruleToHTML (Some ex) r
   -- FMPS
   , let xs = getRewriteRules (transformation r)
     in munless (null xs) $ mconcat $
           h2 "Formal Mathematical Properties" :
           [ para $
              let fmp = rewriteRuleToFMP (not $ isBuggy r) rr
              in highlightXML False $ makeXML "FMP" $
                    builder $ omobj2xml $ toObject fmp
           | Some rr <- xs
           ]
   ]

encodeExampleList :: LinkManager -> Exercise a -> HTMLEncoder a [(Difficulty, Context a)]
encodeExampleList lm ex = simpleEncoder $ \pairs -> mconcat $
   h2 "Examples" :
   [ h3 (s ++ " (" ++ show (length xs) ++ ")")
       <> (if isStatic lm then ul else mconcat) (map make xs)
   | (_, s, xs) <- orderedGroupsWith show fst pairs
   ]
 where
   make (_, x) = para $
      munless (isStatic lm) (
         let st = emptyStateContext ex x
         in spanClass "statelink" $ linkToState lm st $ external lm)
      <> spanClass "term" (string (prettyPrinterContext ex x))

external :: BuildXML a => LinkManager -> a
external lm = element "img"
   ["src" .=. urlForImage lm "external.png", "width" .=. "15"]

encodeDerivation :: LinkManager -> Exercise a -> HTMLEncoder a (Derivation (Rule (Context a), Environment) (Context a))
encodeDerivation lm ex =
   h2 "Derivation" <> htmlDerivation lm ex

encodeDerivationList :: LinkManager -> Exercise a -> HTMLEncoder a [Derivation (Rule (Context a), Environment) (Context a)]
encodeDerivationList lm ex = encoderFor $ \ds ->
   h2 "Derivations"
   <> mconcat
      [ h3 (show i ++ ".") <> htmlDerivation lm ex // d
      | (i, d) <- zip [1::Int ..] ds
      ]

htmlDerivation :: LinkManager -> Exercise a -> HTMLEncoder a (Derivation (Rule (Context a), Environment) (Context a))
htmlDerivation lm ex = encoderFor $ \d ->
   arr diffEnvironment
   >>> htmlDerivationWith (before d) forStep forTerm
 where
   before d =
      stateLink lm (emptyStateContext ex (firstTerm d))
      <> case fmap (isReady ex) (fromContext (lastTerm d)) of
            Just True -> mempty
            _ -> spanClass "error" (string "Final term is not finished")
   forStep ((r, env1), env2) =
      let showEnv e = munless (noBindings e) $ string $ ", " ++ show e in
      spanClass "derivation-step" $ mconcat
         [ unescaped "&#8658; "
         , linkToRule lm ex r $ string $ showId r
         , showEnv env1 -- local environment
         , showEnv env2 -- global environment (diff)
         ]
   forTerm a =
      divClass "term" $ string $ prettyPrinterContext ex a

htmlState :: HTMLEncoder a (State a)
htmlState = do
   lm <- withState getLinkManager
   simpleEncoder $ \state ->
      para $ divClass "state" $
         stateLink lm state
         <> divClass "term" (string $ prettyPrinterContext (exercise state) (stateContext state))
         <> string "ready: " <> bool (ready state)

stateLink :: LinkManager -> State a -> HTMLBuilder
stateLink lm st = munless (isStatic lm) $
   spanClass "derivation-statelink" $ linkToState lm st $ external lm

encodeState :: HTMLEncoder a (State a)
encodeState = do
   lm <- withState getLinkManager
   htmlState <> simpleEncoder (\state -> mconcat
      [ h2 "Feedback"
      , submitDiagnose lm state
      , ul [ linkToFirsts lm state $ string "allfirsts"
           , linkToApplications lm state $ string "allapplications"
           , linkToDerivation lm state $ string "derivation"
           ]
      , munless (noBindings state) $
           h2 "Environment" <> text (environment state)
      , encodePrefixes (statePrefixes state)
      ])

encodePrefixes :: [Prefix (Context a)] -> HTMLBuilder
encodePrefixes = mconcat . zipWith make [1::Int ..]
 where
   make i pr = mconcat
      [ h2 $ "Prefix " ++ show i
      , let count p = text $ length $ filter p prSteps
            enter   = spanClass "step-enter" . text
        in keyValueTable
              [ ("steps", count (const True))
              , ("rules", count isRuleStep)
              , ("major rules", count isMajor)
              , ("active labels", ul $ map enter $ activeLabels pr)
              ]
      , mconcat $ intersperse (string ", ") $ map htmlStep prSteps
      ]
    where
      prSteps = prefixToSteps pr

isRuleStep :: Step l a -> Bool
isRuleStep (RuleStep _ _) = True
isRuleStep _ = False

htmlStep :: Show l => Step l a -> HTMLBuilder
htmlStep (Enter l)      = spanClass "step-enter" $ string $ "enter " ++ show l
htmlStep (Exit  l)      = spanClass "step-exit"  $ string $ "exit " ++ show l
htmlStep (RuleStep _ r) = let s = if isMinor r then "minor" else "major"
                          in spanClass ("step-"++s) $ string $ showId r

htmlDerivationWith :: HTMLBuilder -> (s -> HTMLBuilder) -> (t -> HTMLBuilder) -> HTMLEncoder a (Derivation s t)
htmlDerivationWith before forStep forTerm = simpleEncoder $ \d ->
   divClass "derivation" $ mconcat $
      before : forTerm (firstTerm d) :
         [ forStep s <> forTerm a | (_, s, a) <- triples d ]

htmlAllFirsts :: HTMLEncoder a [(StepInfo a, State a)]
htmlAllFirsts = encoderFor $ \xs ->
   h2 "All firsts" <>
   ul [ keyValueTable
           [ ("Rule", string $ showId r)
           , ("Location", text loc)
           , ("Environment", text env)
           ] <> htmlState // s
      | ((r, loc, env), s) <- xs
      ]

htmlAllApplications :: HTMLEncoder a [(Rule (Context a), Location, State a)]
htmlAllApplications = encoderFor $ \xs ->
   h2 "All applications" <>
   ul [ keyValueTable
           [ ("Rule", string $ showId r)
           , ("Location", text loc)
           ] <> (if isBuggy r then mempty else htmlState // s)
      | (r, loc, s) <- xs
      ]

htmlDiagnosis :: HTMLEncoder a (Diagnosis a)
htmlDiagnosis = encoderFor $ \diagnosis ->
   case diagnosis of
      Buggy _ r ->
         spanClass "error" $ string $ "Not equivalent: buggy rule " ++ show r
      NotEquivalent s ->
         spanClass "error" $ string $ if null s then "Not equivalent" else s
      Similar _ s ->
         h2 "Similar term" <> encodeState // s
      WrongRule _ s mr -> 
         h2 ("WrongRule " ++ maybe "" showId mr)
         <> encodeState // s
      Expected _ s r ->
         h2 ("Expected (" ++ show r ++ ")")
         <> encodeState // s
      Detour _ s _ r ->
         h2 ("Detour (" ++ show r ++ ")")
         <> encodeState // s
      Correct _ s ->
         h2 "Correct" <> encodeState // s
      Unknown _ s ->
         h2 "Unknown" <> encodeState // s

htmlDescription :: HasId a => a -> HTMLBuilder
htmlDescription a = munless (null (description a)) $
   para $
      bold (string "Description") <> br
      <> spanClass "description" (string (description a))

submitForm :: HTMLBuilder -> HTMLBuilder
submitForm this = element "form"
   [ "name"     .=. "myform"
   , "onsubmit" .=. "return submitTerm()"
   , "method"   .=. "post"
   , this
   , element "input" ["type" .=. "text", "name" .=. "myterm"]
   , element "input" ["type"  .=. "submit", "value" .=. "Submit"]
   ]

-- stateinfo service
submitStateInfo :: LinkManager -> Exercise a -> HTMLBuilder
submitStateInfo lm ex =
   submitForm (string "other exercise: ")
   <> submitRequest lm request
 where
   request = "<request service='stateinfo' exerciseid='" ++ showId ex
          ++ "' encoding='html'><state><expr>\" + getTerm() + \"</expr></state></request>"

-- diagnose service
submitDiagnose :: LinkManager -> State a -> HTMLBuilder
submitDiagnose lm st = submitForm mempty <> submitRequest lm request
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
quote s = '"' : s ++ "\""

-- Inject two JavaScript functions for handling the input form
submitURL :: String -> HTMLBuilder
submitURL url = tag "script" $
   ("type" .=. "text/javascript")
   <> unescaped (
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
      \}")
