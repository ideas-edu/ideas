module SupplyArguments (argumentWindow) where

import Common.Utils (Some (..))
import Common.Transformation
import Data.Maybe
import Data.IORef
import Graphics.UI.Gtk

argumentWindow :: IORef (Maybe [String]) -> Rule a -> IO Window
argumentWindow result rule = do
   let args = getArguments rule
   w <- windowNew
   windowSetModal w True
   windowResize w 300 ((1 + length args) * 40)
   windowSetTitle w $ name rule
   let lightBlue = Color (235*256) (244*256) (255*256)
   widgetModifyBg w StateNormal lightBlue
        
   box <- vBoxNew True 10
   buffers <- mapM (argumentBox box) args
      
   buttonBox    <- hBoxNew True 20
   applyButton  <- buttonNew
   cancelButton <- buttonNew
   buttonSetLabel applyButton "Apply rule"
   buttonSetLabel cancelButton "Cancel"
   set buttonBox [containerChild := cancelButton]
   set buttonBox [containerChild := applyButton]
   set box [containerChild := buttonBox]
   set w [containerChild := box]
   
   onClicked applyButton $ do 
      list <- mapM (flip get textBufferText) buffers
      let check (Some arg, s) = isJust (parseArgument arg s)
      case all check $ zip args list of
         False -> return ()
         _ -> do
            writeIORef result (Just list)
            widgetDestroy w
   
   onClicked cancelButton $ do
      widgetDestroy w
 
   widgetShowAll w      
   return w
   
argumentBox :: ContainerClass a => a -> Some Argument -> IO TextBuffer
argumentBox parent (Some arg) = do
   box  <- hBoxNew True 20
   lab  <- labelNew (Just $ argumentDescription arg)
   view <- textViewNew
   flip mapM_ [TextWindowLeft, TextWindowRight, TextWindowTop, TextWindowBottom] $ \tp -> 
      textViewSetBorderWindowSize view tp 2
   
   buffer <- textViewGetBuffer view 
   textBufferSetText buffer $ maybe "" (showArgument arg) (argumentDefault arg)
   
   set box [containerChild := lab]
   set box [containerChild := view]
   set parent [containerChild := box]
   
   onBufferChanged  buffer $ do
      txt <- get buffer textBufferText
      let ok = isJust (parseArgument arg txt)
      labelSetText lab $ argumentDescription arg ++ if ok then " (ok)" else ""
   
   return buffer