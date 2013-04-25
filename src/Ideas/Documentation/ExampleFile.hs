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
-- An example file contains a collection of examples for a certain exercise id,
-- encoded in XML. The examples can be for the diagnose and ready services.
--
-----------------------------------------------------------------------------
module Ideas.Documentation.ExampleFile
   ( ExampleFile, makeExampleFile, items, Item(..)
   , readExampleFile, writeExampleFile
   ) where

import Ideas.Common.Id
import Ideas.Common.Utils (readM)
import Control.Monad
import Data.Maybe
import Ideas.Text.XML

data ExampleFile = EF { fileId :: Id, items :: [Item] }

instance Show ExampleFile where
   show a = "Example file for " ++ showId a ++
            " (" ++ show (length (items a)) ++ " items)"

makeExampleFile :: Id -> [Item] -> ExampleFile
makeExampleFile = EF

instance HasId ExampleFile where
   getId = fileId
   changeId f a = a { fileId = f (fileId a) }

data Item = Diagnose String String String
          | Ready String (Maybe Bool) String

readExampleFile :: FilePath -> IO ExampleFile
readExampleFile file = do
   txt <- readFile file
   xml <- either fail return (parseXML txt)
   guard (name xml == "examples")
   exid <- findAttribute "exerciseid" xml
   xs   <- mapM getItem (children xml)
   return $ EF (newId exid) xs

getItem :: XML -> IO Item
getItem xml = do
   guard (name xml == "diagnose")
   before <- findAttribute "before" xml
   after  <- findAttribute "after"  xml
   let descr = fromMaybe "" $ findAttribute "description" xml
   return $ Diagnose before after descr
 `mplus` do
   guard (name xml == "ready")
   term <- findAttribute "term" xml
   let expected = findAttribute "expected" xml >>= readM
       descr = fromMaybe "" $ findAttribute "description" xml
   return $ Ready term expected descr

writeExampleFile :: FilePath -> ExampleFile -> IO ()
writeExampleFile file ex = writeFile file $ showXML $
   makeXML "examples" $ do
      "exerciseid" .=. showId ex
      mapM_ buildItem (items ex)

buildItem :: Item -> XMLBuilder
buildItem item =
   case item of
      Diagnose before after descr ->
         element "diagnose" $ do
            "before"      .=. before
            "after"       .=. after
            "description" .=. descr
      Ready term expected descr ->
         element "ready" $ do
            "term"        .=. term
            maybe (return ()) (("expected" .=.) . show) expected
            "description" .=. descr