{-# LANGUAGE GADTs, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Encoding in HTML
--
-----------------------------------------------------------------------------

module Ideas.Encoding.EncoderHTML (HTMLEncoder, htmlEncoder) where

import Data.Char
import Data.List
import Data.Maybe
import Ideas.Common.Examples (isEmpty, size, allRandoms)
import Ideas.Common.Library hiding (alternatives, isEmpty, left, right, collapse, Medium)
import Ideas.Common.Strategy.Symbol
import Ideas.Encoding.Encoder
import Ideas.Encoding.LinkManager
import Ideas.Encoding.Logging
import Ideas.Encoding.Request
import Ideas.Encoding.RulePresenter
import Ideas.Encoding.RulesInfo
import Ideas.Encoding.StrategyInfo
import Ideas.Service.BasicServices
import Ideas.Service.Diagnose
import Ideas.Service.DomainReasoner
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Text.HTML hiding (table, keyValueTable)
import Ideas.Text.HTML.Templates
import Ideas.Text.HTML.W3CSS hiding (tag, ul, top, table, content)
import Ideas.Text.OpenMath.FMP
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import Ideas.Utils.Prelude (munless, mwhen)
import Ideas.Utils.TestSuite
import System.IO.Unsafe
import qualified Ideas.Text.HTML.W3CSS as W3

type HTMLEncoder a = EncoderX a HTMLBuilder

htmlEncoder :: DomainReasoner -> TypedEncoder a HTMLPage
htmlEncoder dr tv = do
   lm <- getLinkManager
   ex <- getExercise
   makePage lm dr ex <$> encodeType dr tv

getLinkManager :: EncoderX a LinkManager
getLinkManager = do
   req  <- getRequest
   base <- getBaseUrl
   return $ makeLinkManager base (fromMaybe "" (cgiBinary req))

makePage :: LinkManager -> DomainReasoner -> Exercise a -> HTMLBuilder -> HTMLPage
makePage lm dr ex body =
   (if hasLatexEncoding ex then addScript mathJaxUrl else id) $
   addCSS (urlForCSS lm "2007-chili-pepper.css") $
   webpage myWebPage
 where
   mathJaxUrl =
      "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"

   myWebPage = WebPage
      { title       = "Ideas: documentation pages"
      , menuButtons =
           [ Button "https://ideas.science.uu.nl/" (theme L1) (fontAwesome "lightbulb-o" <> tag "span" (fontSize Large " I") <> tag "span" (fontSize Medium "DEAS"))
           , Button (urlForIndex lm)     (hover White) "Index"
           , Button (urlForExercises lm) (hover White) $ "Exercises " <> nrBadge (length (exercises dr))
           , Button (urlForServices lm)  (hover White) $ "Services " <> nrBadge (length (services dr))
           ]
      , menuStyle   = theme_ . fontSize Large
      , iconBarsStyle = hover White . fontSize Large . theme L1
      , sideWidth = 150
      , sideHeader  = tag "h4" $ barItem $ bold $ textTheme "Exercise"
      , sideButtons =
           let mk f = Button (f lm ex) (hover Black)
           in if getId ex == mempty then [] else
              [ mk urlForExercise    "Information"
              , mk urlForStrategy    "Strategy"
              , mk urlForRules       "Rules"
              , mk urlForConstraints "Constraints"
              , mk urlForExamples    "Examples"
              , mk urlForDerivations "Derivations"
              , mk urlForTestReport  "Test report"
              ]
      , sideStyle = theme L3 . fontSize Medium
      , iconCloseStyle = fontSize XL . hover Black
      , content = body
      , footer = italic $ string $ fullVersion dr
      , footerStyle = theme L1
      }

nrBadge :: BuildXML a => Int -> a
nrBadge = badge . theme L1 . fontSize Small . text

table :: BuildXML a => Bool -> [[a]] -> a
table hasHeader = tableAll . mconcat . zipWith f (hasHeader : repeat False)
 where
   f header = tag "tr" . mconcat . map makeCell
    where
      makeCell | header    = tag "th"
               | otherwise = tag "td"

keyValueTable :: BuildXML a => [(String, a)] -> a
keyValueTable = table False . map (\(s, a) -> [string s, a])

encodeType :: DomainReasoner -> TypedValue (Type a) -> HTMLEncoder a
encodeType dr =
   (encodeIndex, tDomainReasoner) <?>
   (exerciseHeader . htmlDiagnosis dr, tDiagnosis) <?>
   (exerciseHeader . encodeExampleList, tList (tPair tDifficulty tContext)) <?>
   (exerciseHeader . htmlFirsts, tList (Tag "first" (tPair tStepInfo tState))) <?>
   (exerciseHeader . htmlAllApplications, tList (tTuple3 tRule tLocation tState)) <?>
   (exerciseHeader . encodeDerivation, tDerivation tStepInfo tContext) <?>
   (exerciseHeader . encodeDerivationList, tList (tDerivation tStepInfo tContext)) <?>
   \(val ::: tp) ->
        case tp of
           Iso iso t  -> encodeType dr (to iso val ::: t)
           Tag _ t    -> encodeType dr (val ::: t)
           Pair t1 t2 -> encodeType dr (fst val ::: t1) <>
                         encodeType dr (snd val ::: t2)
           t1 :|: t2  -> case val of
                            Left x  -> encodeType dr (x ::: t1)
                            Right x -> encodeType dr (x ::: t2)
           List (Const Service) -> encodeServiceList val
           List (Const SomeExercise) -> encodeExerciseList val
           List (Const Rule) -> exerciseHeader (encodeRuleList val)
           List t -> ul [ encodeType dr (x ::: t) | x <- val ]
           Const t -> encodeConst dr (val ::: t)
           _ -> string $ "unknown: " ++ show tp

encodeConst :: DomainReasoner -> TypedValue (Const a) -> HTMLEncoder a
encodeConst dr tv@(val ::: tp) =
   case tp of
      Service     -> encodeService val
      Exercise    -> exerciseHeader (encodeExercise val)
      Strategy    -> exerciseHeader (encodeStrategy val)
      Rule        -> encodeRule val
      State       -> exerciseHeader (encodeState dr val)
      Location    -> text val
      Environment -> text val
      Term        -> text val
      Context     -> encodeContext val
      String      -> string val
      Result      -> exerciseHeader (encodeResult val)
      _           -> text tv

encodeContext :: Context a -> HTMLEncoder a
encodeContext ctx = (\ex -> string (prettyPrinterContext ex ctx)) <$> getExercise

encodeIndex :: DomainReasoner -> HTMLEncoder a
encodeIndex dr = return $ mconcat
   [ htmlDescription "Domain reasoner" dr
   , panel $ right $ string "Logging: " <> bool logEnabled
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

encodeServiceList :: [Service] -> HTMLEncoder a
encodeServiceList srvs = do
   lm <- getLinkManager
   return $
      h1 "Services" <>
      mconcat
         [ h2 (show i ++ ". " ++ s) <> table False (map (make lm) xs)
         | (i, s, xs) <- groupById srvs
         ]
 where
   make lm s = [ linkToService lm s (string (showId s)) <>
                 mwhen (serviceDeprecated s) (italic (string " (deprecated)"))
               , string (description s)
               ]

encodeExerciseList :: [Some Exercise] -> HTMLEncoder a
encodeExerciseList exs = do
   lm <- getLinkManager
   return $
      h1 "Exercises" <>
      mconcat
         [ h2 (show i ++ ". " ++ dom) <> table False (map (make lm) xs)
         | (i, dom, xs) <- groupsWith f exs
         ]
 where
   f :: Some Exercise -> String
   f (Some ex) = fromMaybe "" (listToMaybe (qualifiers (getId ex)))

   make :: LinkManager -> Some Exercise -> [HTMLBuilder]
   make lm (Some ex) =
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
   zipWith f [1..] . groupBy eq . sortOn get
 where
   eq x y = get x == get y
   f i xs = (i, showf (get (head xs)), xs)

encodeService :: Service -> HTMLEncoder a
encodeService srv = return $ mconcat
   [ htmlDescription "Service" srv
   , mwhen (serviceDeprecated srv) $
        para $ spanClass "warning" $ string "Warning: this service is deprecated"
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

encodeExercise :: Exercise a -> HTMLEncoder a
encodeExercise ex = do
   lm <- getLinkManager
   return $ mconcat
      [ generalInfo lm
      , h2 "Example exercises"
      , ul $ [ para $ linkToExamples lm ex $ string "list of examples"
             | isEmpty (examples ex)
             ] ++
             [ para $ mconcat $
                  string "generate exercise: " :
                  intersperse (string ", ")
                     [ linkToRandomExample lm ex d $ text d
                     | d <- [VeryEasy .. VeryDifficult]
                     ]
             | not $ null $ allRandoms $ examples ex
             ] ++
             [ para $ submitStateInfo lm ex ]
      ]
 where
   generalInfo lm = container $ panel $ left $ keyValueTable
      [ ("Code",   string $ showId ex)
      , ("Status", text $ status ex)
      , ("Strategy", linkToStrategy lm ex $ string (showId $ strategy ex))
      , ("Rules", text nrOfSoundRules)
      , ("Constraints", text (length (constraints ex)))
      , ("Buggy rules", text nrOfBuggyRules)
      , ("OpenMath support", bool $ isJust $ hasTermView ex)
      , ("Restartable strategy", bool $ canBeRestarted ex)
      , ("Exercise generator", bool $ not $ null $ allRandoms $ examples ex)
      , ("Examples", text $ size $ examples ex)
      ]
    where
      (nrOfBuggyRules, nrOfSoundRules) =
         mapBoth length (partition isBuggy (ruleset ex))

exerciseHeader :: HTMLEncoder a -> HTMLEncoder a
exerciseHeader body = withExercise $ \ex ->
   tag "div" $ mconcat
      [ return (htmlDescription "Exercise" ex)
      , body
      ]

encodeStrategy :: Strategy (Context a) -> HTMLEncoder a
encodeStrategy s = withExercise $ \ex ->
   return $ mconcat
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

encodeResult :: Result -> HTMLEncoder a
encodeResult tests = getLinkManager >>= \lm -> mconcat
   [ h2 "Test report"
   , divClass "test-summary" $ mconcat
        [ divClass "test-status" (statusImg lm tests 32)
        , keyValueTable
             [ ("Tests",    text (nrOfTests tests))
             , ("Errors",   text (nrOfErrors tests))
             , ("Warnings", text (nrOfWarnings tests))
             , ("Time",     string (show (timeInterval tests) ++ "s"))
             , ("Rating",   showRating lm $ fromMaybe 10 $ rating tests)
             ]
        , h3 "Suites"
        , ul [ string s <> space <> text t
             | (s, t) <- subResults tests
             ]
        ]
   , mwhen (isError tests) $
        mconcat (h2 "Errors" : map (makeItem lm) errors)
   , mwhen (isWarning tests) $
        mconcat (h2 "Warnings" : map (makeItem lm) warnings)
   , h2 "Tests"
   , make lm tests
   ]
 where
   msgs     = allMessages tests
   errors   = filter (isError . snd) msgs
   warnings = filter (isWarning . snd) msgs

   make lm t = mconcat $
      map (makeGroup lm) (subResults t) ++
      map (makeItem lm) (topMessages t)

   makeGroup lm (s, t) = divClass "test-group" $
      divClass "test-title" (string (s ++ " " ++ show t))
      <> make lm t

   makeItem lm (s, m) = divClass "test-item" $
      statusImg lm m 16 <> spaces 3 <> string s <> msg
    where
      msg | isOk m      = mempty
          | otherwise   = string ": " <> string (intercalate "," (messageLines m))

statusImg :: (HasStatus a, BuildXML b) => LinkManager -> a -> Int -> b
statusImg lm a n = element "img"
   [ "src"    .=. urlForImage lm (statusSrc a)
   , "height" .=. show n
   , "width"  .=. show n
   ]

statusSrc :: HasStatus a => a -> String
statusSrc a
   | isError a   = "stop.png"
   | isWarning a = "flagblue.png"
   | otherwise   = "ok.png"

showRating :: BuildXML a => LinkManager -> Int -> a
showRating lm = rec (5::Int)
 where
   rec 0 _ = mempty
   rec n a = element "img"
      [ "src"    .=. urlForImage lm png
      , "height" .=. "16"
      , "width"  .=. "16"
      ] <> rec (n-1) (a-2)
    where
      png | a >= 2    = "star.png"
          | a == 1    = "star_2.png"
          | otherwise = "star_3.png"

encodeRuleList :: [Rule (Context a)] -> HTMLEncoder a
encodeRuleList rs = withExercise $ \ex -> do
   lm <- getLinkManager
   let (rs1, rs2) = partition isBuggy rs

       header = [ string "Rule name", string "Args"
                , string "Used", string "Siblings", string "Rewrite rule"
                ]
       used = rulesInStrategy (strategy ex)
       f r  = [ linkToRule lm ex r $ string $ showId r
              , text $ length $ getRefs r
              , bool $ r `elem` used
              , string $ intercalate ", " $ map show $ ruleSiblings r
              , mwhen (isRewriteRule r) $
                   ruleToHTML (Some ex) r
              ]

   return $ mconcat
         [ h2 $ "Rules for " ++ showId ex
         , table True (header:map f rs2)
         , h2 $ "Buggy rules for " ++ showId ex
         , table True (header:map f rs1)
         ]

encodeRule :: Rule (Context a) -> HTMLEncoder a
encodeRule r = withExercise $ \ex ->
   return $ mconcat
      [ htmlDescription "Rule" r
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

encodeExampleList :: [(Difficulty, Context a)] -> HTMLEncoder a
encodeExampleList pairs = withExercise $ \ex -> do
   lm <- getLinkManager
   return $ mconcat $
      h2 "Examples" :
      [  container . third $
          h3 (s ++ " (" ++ show (length xs) ++ ")")
          <> W3.ulWith hoverable (map ((fontAwesome "hand-o-right" <>) . padding Small . make lm ex) xs)
      | (_, s, xs) <- orderedGroupsWith show fst pairs
      ]
 where
   make lm ex (_, x) =
      let st = emptyStateContext ex x
      in button (escapeInURL (urlForState lm st)) (htmlContext False ex x)

encodeDerivation :: Derivation (StepInfo a) (Context a) -> HTMLEncoder a
encodeDerivation d =
   h2 "Derivation" <> htmlDerivation d

encodeDerivationList :: [Derivation (StepInfo a) (Context a)] -> HTMLEncoder a
encodeDerivationList ds =
   h2 "Derivations"
   <> mconcat
      [ h3 (show i ++ ".") <> htmlDerivation d
      | (i, d) <- zip [1::Int ..] ds
      ]

htmlDerivation :: Derivation (StepInfo a) (Context a) -> HTMLEncoder a
htmlDerivation d = withExercise $ \ex -> do
   lm <- getLinkManager
   let before =
          stateLink lm (emptyStateContext ex (firstTerm d))
          <> case fmap (isReady ex) (fromContext (lastTerm d)) of
                Just True -> mempty
                _ -> spanClass "error" (string "Final term is not finished")
       forStep ((r, _, env1), env2) =
          let showEnv e = munless (noBindings e) $ string $ "," ++ show e in
          container $ marginPos CenterLeft $ mconcat
             [ string [chr 8658, ' ']
             , linkToRule lm ex r $ string $ showId r
             , showEnv env1 -- local environment
             , showEnv env2 -- global environment (diff)
             ]
       forTerm = htmlContext True ex
   return $ htmlDerivationWith before forStep forTerm (diffEnvironment d)

htmlState :: State a -> HTMLEncoder a
htmlState state = do
   lm <- getLinkManager
   return $ para $ container $ background LightGray $ para $
      stateLink lm state
      <> htmlContext True (exercise state) (stateContext state)
      <> string "ready: " <> bool (finished state)

htmlContext :: Bool -> Exercise a -> Context a -> HTMLBuilder
htmlContext useDiv ex = f "term" . textLines . printer
 where
   textLines = mconcat . intersperse br . map string . lines
   f = if useDiv then divClass else spanClass

   inline s = "\\(" ++ s ++ "\\)" :: String
   printer
      | hasLatexEncoding ex = inline . show . latexPrinterContext ex
      | otherwise = prettyPrinterContext ex

stateLink :: LinkManager -> State a -> HTMLBuilder
stateLink lm st =
   container $ right $ linkToState lm st $
   tag "span" $ textTheme $ fontAwesome "external-link"

encodeState :: DomainReasoner -> State a -> HTMLEncoder a
encodeState dr state = do
   lm <- getLinkManager
   htmlState state <>
      let xs = useAllFirsts dr state
          n  = either (const 0) length xs
      in return (mconcat
         [ h2 "Feedback"
         , submitDiagnose lm state
         , tag "p" $ padding Small  $ spaced [ case xs of
                   Right (hd:_) -> linkToState lm (snd hd) $ serviceButton $ string "onefirst"
                   _ -> string "(no onefirst)"
              , linkToFirsts lm state $ serviceButton $ string $ "allfirsts (" ++ show n ++ ")"
              , linkToApplications lm state $ serviceButton $ string "allapplications"
              , linkToDerivation lm state $ serviceButton $ string "derivation"
              , linkToMicrosteps lm state $ serviceButton $ string "microsteps"
           ]
         , munless (noBindings state) $
              h2 "Environment" <> text (environment state)
         , encodePrefix state (statePrefix state)
         ])
     where
       serviceButton = theme L1  . W3.w3class "w3-button"

-- use allfirsts service of domain reasoner, instead of calling the service
-- directly. Note that the service can be redefined (e.g. for the Ask-Elle tutor)
useAllFirsts :: DomainReasoner -> State a -> Either String [(StepInfo a, State a)]
useAllFirsts dr = unsafePerformIO . useAllFirstsIO dr

useAllFirstsIO :: DomainReasoner -> State a -> IO (Either String [(StepInfo a, State a)])
useAllFirstsIO dr st = do
   srv <- either fail return $ findService dr (newId ("allfirsts" :: String))
   case serviceFunction srv of
      f ::: tp ->
         case equalM tp (tState .-> tError (tList (Tag "first" (tPair tStepInfo tState)))) of
            Left msg   -> fail msg
            Right conv -> return (conv f st)

encodePrefix :: State a -> Prefix (Context a) -> HTMLBuilder
encodePrefix st =
   mconcat . zipWith3 make [1::Int ..] (stateLabels st) . prefixPaths
 where
   make i ls path = mconcat
      [ h2 $ "Path " ++ show i
      , let count p = text $ length $ filter p prSteps
            enter   = spanClass "step-enter" . text
            comma c = if c == ',' then ", " else [c]
        in keyValueTable
              [ ("path", string $ concatMap comma $ show path)
              , ("steps", count (const True))
              , ("major rules", count isMajor)
              , ("active labels", ul $ map enter ls)
              ]
      , mconcat $ intersperse (string ", ") $ map htmlStep prSteps
      ]
    where
      ex  = exercise st
      ctx = stateContext st
      prSteps = fst $ replayPath path (strategy ex) ctx

htmlStep :: Rule a -> HTMLBuilder
htmlStep r =
   case (isEnterRule r, isExitRule r) of
      (Just l, _) -> spanClass "step-enter" $ string $ "enter " ++ show l
      (_, Just l) -> spanClass "step-exit"  $ string $ "exit " ++ show l
      _ -> let s = if isMinor r then "minor" else "major"
           in spanClass ("step-"++s) $ string $ showId r

htmlDerivationWith :: HTMLBuilder -> (s -> HTMLBuilder) -> (t -> HTMLBuilder) -> Derivation s t -> HTMLBuilder
htmlDerivationWith before forStep forTerm d =
   container $ background LightGray $ para $ mconcat $
      before : forTerm (firstTerm d) :
         [ forStep s <> forTerm a | (_, s, a) <- triples d ]

htmlFirsts :: [(StepInfo a, State a)] -> HTMLEncoder a
htmlFirsts xs =
   h2 "Firsts" <>
   ul [ keyValueTable
           [ ("Rule", string $ showId r)
           , ("Location", text loc)
           , ("Term", text $ show $ currentTerm (top $ stateContext s) )
           , ("Focus", text $ show $ currentTerm (stateContext s) )
           , ("Environment", text env)
           ] <> htmlState s
      | ((r, loc, env), s) <- xs
      ]

htmlAllApplications :: [(Rule (Context a), Location, State a)] -> HTMLEncoder a
htmlAllApplications xs =
   h2 "All applications" <>
   ul [ keyValueTable
           [ ("Rule", string $ showId r)
           , ("Location", text loc)
           ] <> (if isBuggy r then mempty else htmlState s)
      | (r, loc, s) <- xs
      ]

htmlDiagnosis :: DomainReasoner -> Diagnosis a -> HTMLEncoder a
htmlDiagnosis dr diagnosis =
   case diagnosis of
      SyntaxError s ->
         spanClass "error" $ string s
      Buggy _ r ->
         spanClass "error" $ string $ "Not equivalent: buggy rule " ++ show r
      NotEquivalent s ->
         spanClass "error" $ string $ if null s then "Not equivalent" else s
      Similar _ s mr ->
         h2 ("Similar term " ++ maybe "" showId mr) <> encodeState dr s
      WrongRule _ s mr ->
         h2 ("WrongRule " ++ maybe "" showId mr)
         <> encodeState dr s
      Expected _ s r ->
         h2 ("Expected (" ++ show r ++ ")")
         <> encodeState dr s
      Detour _ s _ r ->
         h2 ("Detour (" ++ show r ++ ")")
         <> encodeState dr s
      Correct _ s ->
         h2 "Correct" <> encodeState dr s
      Unknown _ s ->
         h2 "Unknown" <> encodeState dr s

htmlDescription :: HasId a => String -> a -> HTMLBuilder
htmlDescription tp a = munless (null (description a)) $
   para $ panel $ barPos CenterLeft $ borderTheme $ background LightGray $ para $ mconcat
      [ bold $ string $ tp ++ " " ++ showId a
      , br
      , italic $ string $ description a
      ]

submitForm :: HTMLBuilder -> HTMLBuilder
submitForm this = form . padding Tiny $ mconcat
  [ para $ mconcat [termLabel, termInput mempty]
  , this
  , para $ submitBtn mempty
  ]
 where
   form      = tag "form"  . ("name" .=. "myform" <>) . ("method" .=. "post" <>) . ("onsubmit" .=. "return submitTerm()" <>) . w3class "w3-container"
   termLabel = tag "label" $ tag "b" "Input:"
   termInput = tag "input" . ("name" .=. "myterm" <>) . ("type" .=. "text" <>) . border . W3.input
   submitBtn = tag "input" . ("value" .=. "Submit" <>) . ("type" .=. "submit" <>) . theme L1 . W3.w3class "w3-button"

-- stateinfo service
submitStateInfo :: LinkManager -> Exercise a -> HTMLBuilder
submitStateInfo lm ex =
   submitForm (string "other exercise: ")
   <> submitRequest lm request
 where
   request = "<request service='stateinfo' exerciseid='" ++ showId ex
          ++ "' encoding='html'><state><expr>\" + getTerm() + \"</expr></state></request>"

-- diagnose service *here*
submitDiagnose :: LinkManager -> State a -> HTMLBuilder
submitDiagnose lm st = submitForm mempty <> submitRequest lm request
 where
   request = "<request service='diagnose' exerciseid='" ++ showId (exercise st)
          ++ "' encoding='html'>" ++ ststr ++ "<expr>\"  + getTerm() + \"</expr></request>"

   ststr   = case fromBuilder (stateToXML st) of
                Just el -> concatMap f (compactXML el)
                Nothing -> ""

   f '\\' = "\\\\"
   f '"'  = "\\\""
   f c    = [c]

submitRequest :: LinkManager -> String -> HTMLBuilder
submitRequest lm request = submitURL $
   quote (urlForRequest lm) ++ "+encodeURIComponent(" ++ quote request ++ ")"

quote :: String -> String
quote s = '"' : s ++ "\""

-- Inject two JavaScript functions for handling the input form
submitURL :: String -> HTMLBuilder
submitURL url = tag "script" $
   ("type" .=. "text/javascript")
   <> string ( -- script is not to be escaped
      "function getTerm() {\
      \   var s = document.myform.myterm.value;\
      \   var result = '';\
      \   for (var i=0;i<s.length;i++) {\
      \      if (s[i]=='<') result+='&lt;';\
      \      else if (s[i]=='>') result+='&gt;';\
      \      else if (s[i]=='&') result+='&amp;';\
      \      else if (s[i]=='\"') result+='&quot;';\
      \      else if (s[i]==\"'\") result+='&apos;';\
	    \      else result+=s[i];\
      \   }\
      \   return result;\
      \}\
      \function submitTerm() {\
      \   document.myform.action = " ++ url ++ ";\
      \}")