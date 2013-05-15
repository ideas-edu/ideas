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
import Ideas.Service.DomainReasoner
import Ideas.Service.Evaluator
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Service.EncoderXML
import Ideas.Service.BasicServices

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
      List t     -> ul [ encodeType ex (x ::: t) | x <- val ]
      Const t    -> encodeConst ex (val ::: t)
      _ -> text $ "unknown: " ++ show tp
      
encodeConst :: Exercise a -> Encoder (Const a) XMLBuilderM ()
encodeConst ex tv@(val ::: tp) =
   case tp of 
      Service      -> encodeService val
      Exercise     -> encodeExercise val
      Rule         -> text $ "ruleid: " ++ showId val
      Derivation t1 t2 -> htmlDerivation ex t1 t2 val
      SomeExercise -> case val of
                         Some ex -> text $ "exerciseid: " ++ showId ex
      Context      -> do h2 " context" 
                         htmlState (empyStateContext ex val)
      Location     -> text $ "location: " ++ show val
      Environment  -> text $ "environment: " ++ show val
      State        -> htmlState val
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
   --, urlForStrategy  :: Exercise a -> String
   , urlForRules     :: Exercise a -> String
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
linkToStrategy _ _ = id

linkToRules :: LinkManager a -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToRules lm = link . escapeInURL . urlForRules lm

linkToDerivations :: LinkManager a -> Exercise a -> HTMLBuilder -> HTMLBuilder
linkToDerivations _ _ = id

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
   , urlForRules = \ex ->
        url ++ show (rulelistRequest ex)
   , urlForExamples = \ex ->
        url ++ show (examplesRequest ex)
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
groupsWith get = zipWith f [1..] . groupBy eq . sortBy (comparing get)
 where
   eq x y = get x == get y
   f i xs = (i, get (head xs), xs)

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
   exerciseMenu ex
   h1 $ "Exercise " ++ showId ex
   htmlDescription ex
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
      , ("Strategy", link "" $ text (showId $ strategy ex))
      , ("Rules", text (show nrOfSoundRules))
      , ("Buggy rules", text (show nrOfBuggyRules))
      , ("OpenMath support", text $ showBool $ isJust $ hasTermView ex)
      , ("Restartable strategy", text $ showBool $ canBeRestarted ex)
      , ("Exercise generator", text $ showBool $ isJust $ randomExercise ex)
      , ("Examples", text $ show $ length $ examples ex)
      ]
  
   (nrOfBuggyRules, nrOfSoundRules) = 
      mapBoth length (partition isBuggy (ruleset ex))

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

showBool :: Bool -> String
showBool b = if b then "yes" else "no"

encodeRuleList :: Exercise a -> [Rule (Context a)] -> HTMLBuilder
encodeRuleList ex rs = do
   exerciseMenu ex
   h1 $ "Rules for " ++ showId ex
   table True (header:map f rs2)
   h1 $ "Buggy rules for " ++ showId ex
   table True (header:map f rs1)
 where
   header = [ text "Rule name", text "Args"
            , text "Used", text "Rewrite rule"
            ]
   (rs1, rs2) = partition isBuggy rs
   used = rulesInStrategy (strategy ex)
   f r  = [ link "" $ ttText (showId r)
          , text $ show $ length $ getRefs r
          , text $ showBool $ r `elem` used
          , when (isRewriteRule r) $
               ruleToHTML (Some ex) r
          ]

htmlState :: State a -> HTMLBuilder
htmlState state = do
   h2 "state"
   text $ "state: " ++ show state
   br
   text $ " ready: " ++ show (ready state)
   br
   submitScript2 state
   myForm mempty
   parens $ linkToAllFirsts lm state $ text "allfirsts"
   parens $ linkToAllApplications lm state $ text "allapplications"
   parens $ linkToDerivation lm state $ text "derivation"

htmlDerivation :: Exercise a -> Type a t1 -> Type a t2 -> Derivation t1 t2 -> HTMLBuilder
htmlDerivation ex t1 t2 d = do 
   forTerm (firstTerm d)
   mapM_ make (triples d)
 where
   make (_, s, a) = forStep s >> forTerm a
   forTerm a = encodeType ex (a ::: t2)
   forStep s = do
      h1 "Step"
      br
      encodeType ex (s ::: t1)
      br

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

-- http://www.blooberry.com/indexdot/html/topics/urlencoding.htm
escapeInURL :: String -> String
escapeInURL = concatMap f
 where
   f '+' = "%2B"
   f '>' = "%3E"
   f '&' = "%26"
   f c   = [c]
   
parens :: HTMLBuilder -> HTMLBuilder
parens s = text " (" >> s >> text ") "


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