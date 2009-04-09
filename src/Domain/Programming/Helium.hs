module Domain.Programming.Helium 
   (compile, module UHA_Syntax) where

import Compile hiding (compile)
import UHA_Syntax

compile :: String -> Either String Module
compile = undefined