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
-- Abstract syntax for feedback scripts
--
-----------------------------------------------------------------------------
module Service.FeedbackScript where

import Common.Id
import Common.Utils (safeHead)
import Control.Monad
import Common.Transformation
import Data.Char
import Data.Maybe
import Data.Monoid

type Script = [Decl]

data Environment a = Env
   { oldReady   :: Maybe Bool
   , recognized :: Maybe (Rule a)
   , expected   :: Maybe (Rule a)
   , diffPair   :: Maybe (String, String)
   }
   
emptyEnvironment :: Environment a
emptyEnvironment = Env 
   { oldReady   = Nothing
   , recognized = Nothing
   , expected   = Nothing
   , diffPair   = Nothing
   }

data Decl = NameSpace Id
          | Decl DeclType Id (Maybe Condition) Text

data DeclType = RuleText | StringDecl | Feedback
   deriving Eq

type Text = [TextItem]
        
data TextItem = TextString String  
              | TextRef Id
          
data Condition = RecognizedIs Id
               | CondRef Id

showScript :: Script -> String
showScript = unlines . map show

showText :: Text -> String
showText = concatMap show

instance Show Decl where 
   show (NameSpace a)   = "namespace " ++ show a
   show (Decl dt a c t) = show dt ++ " " ++ show a ++ maybe "" (\x -> " | "  ++ show x) c ++ " = " ++ showText t

instance Show DeclType where
   show RuleText   = "text"
   show StringDecl = "string"
   show Feedback   = "feedback"

instance Show Condition where
   show (RecognizedIs a) = "recognize " ++ show a
   show (CondRef a)      = "@" ++ show a 

instance Show TextItem where
   show (TextString s) = s
   show (TextRef a)    = "@" ++ show a

toString :: Environment a -> Script -> Text -> String
toString env script = fromMaybe "" . eval env script . Right

ruleToString :: Environment a -> Script -> Rule a -> String
ruleToString env script = fromMaybe "" . eval env script . Left . getId

eval :: Environment a -> Script -> Either Id Text -> Maybe String
eval env script = fmap normalize . either (return . findIdRef) recs
 where
   recs = liftM concat . mapM rec
   
   rec (TextString s)           = return s
   rec (TextRef a)              
      | a == newId "expected"   = fmap findIdRef (expected env)
      | a == newId "recognized" = fmap findIdRef (recognized env)
      | a == newId "diffbefore" = fmap fst (diffPair env)
      | a == newId "diffafter"  = fmap snd (diffPair env)
      | a `elem` feedbackIds    = findRef (==a)
      | otherwise               = findRef (==a)

   evalBool (RecognizedIs a) = maybe False ((==a) . getId) (recognized env)
   evalBool (CondRef a)
      | a == newId "oldready"    = fromMaybe False (oldReady env)
      | a == newId "hasexpected" = isJust (expected env)
      | otherwise                = False

   namespaces = mempty : [ a | NameSpace a <- script ]

   findIdRef :: HasId b => b -> String
   findIdRef x = 
      let a = getId x
          p z = any (\n -> n#z == a) namespaces
      in fromMaybe (show a) (findRef p)
        
   findRef p = safeHead $ catMaybes
      [ recs t
      | Decl _ a cond t <- script
      , p a 
      , maybe True evalBool cond
      ]

normalize :: String -> String
normalize = interpunction . unwords . words
 where
   special = (`elem` ".,:;?!")
   interpunction xs =
      case xs of
         a:b:ys | special a && isAlpha b -> a : ' ' : b : interpunction ys
         y:ys -> y:interpunction ys
         []   -> []
   

feedbackSame, feedbackNotEq, feedbackUnknown, feedbackOk, feedbackBuggy, 
   feedbackDetour :: Environment a -> Script -> String
feedbackSame    = make "same"
feedbackNotEq   = make "noteq"
feedbackUnknown = make "unknown"
feedbackOk      = make "ok"
feedbackBuggy   = make "buggy"
feedbackDetour  = make "detour"

feedbackHint :: Bool -> Environment a -> Script -> String
feedbackHint b = make (if b then "hint" else "step")

make :: String -> Environment a -> Script -> String
make s env script = toString env script [TextRef (newId s)]

feedbackIds :: [Id]
feedbackIds = map newId ["same", "noteq", "unknown", "ok", "buggy", "detour", "hint"]