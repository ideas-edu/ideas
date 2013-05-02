----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Subversion revision information
--
-----------------------------------------------------------------------------
module Ideas.Main.Revision 
   ( version, revision, lastChanged
   ) where

import Data.Char

version :: String
version = "1.0.11"

revision :: Int
revision =
   case reads $ takeWhile isDigit $ dropWhile (not . isDigit) svnRev of
      (n,_):_ -> n
      _       -> 0

lastChanged :: String 
lastChanged = unwords $ init $ drop 2 $ words svnId

svnRev, svnId :: String
svnRev = "$Revision$"
svnId  = "$Id$"