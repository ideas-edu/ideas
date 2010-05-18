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
-- Simple tool for regression testing (black-box)
--
-----------------------------------------------------------------------------
module Main (main) where

import Configuration
import System.Directory
import Data.List
import Data.Maybe
import Control.Monad hiding (when)
import System.Process
import System.FilePath
import System.Time
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Control)
import Observable

main :: IO ()
main = start $ do
   -- References
   initCfg  <- readConfiguration
   itemsRef <- createControl []
   workLoad <- createControl 0
   details  <- createControl False

   -- Top frame, status bar, and icon
   f      <- frame [text := "QuickTest", bgcolor := white]
   field1 <- statusField []
   field2 <- statusField [statusWidth := 100]
   field3 <- statusField [statusWidth := 100]
   iconCreateFromFile "quicktest.ico" sizeNull >>= topLevelWindowSetIcon f
   set f [statusBar := [field1, field2, field3]] 
   
   -- Create widgets
   controlPanel <- makeControlPanel initCfg (itemsRef, workLoad, details) f 
   itemPanel    <- makeItemPanel initCfg itemsRef f
   
   -- Event handlers
   addObserver itemsRef $ \xs ->
      set field3 [text := "Errors: " ++ show (length xs)]
   addObserver workLoad $ \n ->
      set field2 [text := "Tests: " ++ show n]
   addObserver details $ \b -> do
      set itemPanel [visible := b]
      set f $ if b 
         then [ layout := margin 15 $ column 30 
                   [hfill $ widget controlPanel, fill $ widget itemPanel] 
              , size := sz 480 700 ] 
         else [ layout := margin 15 $ fill $ widget controlPanel
              , size := sz 480 150 ]
   
   notifyObservers itemsRef
   notifyObservers details

type ControlArgs = (Control [Item], Control Int, Control Bool)

makeControlPanel :: Configuration -> ControlArgs -> Window a -> IO (Panel ())
makeControlPanel cfg (itemsRef, workLoad, details) w = do
   -- References
   startTime <- createControl Nothing
   done      <- createControl 0
   workList  <- createControl []
   testDir   <- createControl (testDirectory cfg)
   
   -- Create widgets
   p   <- panel w []
   st1 <- staticText p []
   st2 <- staticText p []
   st3 <- staticText p []
   st4 <- staticText p []
   hg  <- hgauge p 0 []
   bGo <- button p [text := "Go!"]
   bSt <- button p [text := "Stop", enabled := False]
   bSh <- button p [text := "Show details"]
   bCh <- button p [text := "Change"]
   tmr <- timer  p [enabled := False, interval := 100]
   
   -- Event handlers
   set p   [on idle    := onIdle done workList]
   set tmr [on command := notifyObservers startTime]
   set bGo [on command := getValue testDir >>= onCommandGo startTime done workList]
   set bSt [on command := setValue workList []]
   set bSh [on command := changeValue details not]
   set bCh [on command := onCommandChangeDir testDir p]
   addObserver workList $ \xs -> do
      set bGo [enabled := null xs]
      set bCh [enabled := null xs]
      set bSt [enabled := not (null xs)]
      case xs of 
         []       -> do d <- getValue testDir
                        set st1 [text := d]
                        set st2 [text := ""]
                        setValue startTime Nothing
         (d, f):_ -> do set st1 [text := d]
                        set st2 [text := f]
   addObserver itemsRef $ \xs ->
      set st3 [text := "Errors: " ++ show (length xs)]
   addObserver startTime $ \mct -> do
      updateTime st4 mct
      set tmr [enabled := isJust mct ]
   addObserver details $ \b -> 
      set bSh [text := (if b then "Hide" else "Show") ++ " details"]
   addObserver testDir $ \d -> do
      set st1 [text := d]
      setValue startTime Nothing
      setValue done 0
   addObserver workLoad (gaugeSetRange hg)
   addObserver done $ \n -> 
      set hg [selection := n]
   notifyObservers workList
   
   -- Return panel
   set p [layout := column 10 
      [ row 10 [ column 10
           [ row 10 [label "Directory:", hfill $ widget st1]
           , row 10 [label "File: ", hfill $ widget st2]
           , row 10 [hfill $ widget st3, hfill $ widget st4]
           ], widget bCh ]
      , hfill $ widget hg
      , hfill $ vspace 5
      , row 0 [hglue, widget bGo, hspace 30, widget bSt, hglue, widget bSh]
      ]]
   return p
 where
   onCommandGo startTime done workList dir = do
      testFiles <- findTestFiles (testExtensions cfg) dir
      clockTime <- getClockTime
      setValue workLoad (length testFiles)
      setValue itemsRef []
      setValue startTime (Just clockTime)
      setValue done 0
      setValue workList testFiles

   onIdle done workList = do 
      work <- getValue workList
      case work of
         (d, file):rest -> do
            mItem <- testFile cfg d file
            case mItem of 
               Just item -> changeValue itemsRef (++ [item])
               Nothing   -> return ()
            setValue workList rest
            changeValue done (+1)
            return True
         _ ->
            return False -- no more work to do

   onCommandChangeDir testDir w = do 
      let msg = "Select directory with test files"
      oldDir <- getValue testDir
      result <- dirOpenDialog w False msg oldDir
      case result of
         Just d  -> setValue testDir d
         Nothing -> return ()

makeItemPanel :: Configuration -> Control [Item] -> Window a -> IO (Panel ())
makeItemPanel cfg itemsRef w = do
   -- References
   selected <- createControl (Difference, Nothing)
   
   -- Create widgets
   p  <- panel w []
   lc <- singleListBox p []
   rb <- radioBox p Horizontal (map show modes) [selection := 3]
   tc <- textCtrl p []
   b  <- button p [text := "Create", enabled := False]
   textCtrlSetEditable tc False
   
   -- Event handlers
   set rb [on select  := get rb selection  >>= onSelectRadioBox selected]
   set lc [on select  := get lc selection  >>= onSelectList selected]
   set b  [on command := getValue selected >>= onCreate]
   addObserver selected $ \pair@(_, m) -> do
      onShowItem tc pair
      set b [enabled := isJust m]
   addObserver itemsRef $ \xs -> do
      when (null xs) $
         changeValue selected (\(m, _) -> (m, Nothing))
      i <- get lc selection
      let f item = inputFile item ++ extra (isJust (expectedText item))
          extra b = if b then "" else " (no expected)" 
      set lc [items := map f xs, selection := i `max` 0]
      when (not (null xs) && i<0) $
         changeValue selected (\(m, _) -> (m, Just (head xs))) 
   
   -- Return panel
   set p [layout := column 10
      [ fill $ widget lc
      , fill $ widget tc
      , row 10 [widget rb, hglue, vfloatCenter $ widget b] 
      ]]
   return p
 where
   onSelectRadioBox ctrl i =
      let f (_, m) = (modes !! i, m)
      in changeValue ctrl f

   onSelectList ctrl i = do
      items <- getValue itemsRef
      let f (mode, _) =
             case drop i items of
                hd:_ | i >= 0 -> (mode, Just hd)
                _ -> (mode, Nothing)
      changeValue ctrl f

   onCreate (_, Nothing)   = return ()
   onCreate (_, Just item) = do
      writeFile (expectedFile item) (normalizeNewlines (outputText item))
      changeValue itemsRef $ filter (/= item)

   onShowItem w (mode, maybeItem) = do
      msg <- maybe (return "") (showItem cfg mode) maybeItem
      set w [text := msg]

updateTime :: Textual c => c -> Maybe ClockTime -> IO ()
updateTime w (Just t0) = do
   t1 <- getClockTime
   let diff   = diffClockTimes t1 t0
       (a, b) = (tdPicosec diff `div` 1000000000) `divMod` 1000
       secs   = a + toInteger (tdSec diff + 60 * tdMin diff)
       mils   = let s = show b in replicate (3 - length s) '0' ++ s
   set w [text := "Time: " ++ show secs ++ "." ++ mils ++ " secs"]
updateTime _ _ = 
   return ()

type TestFiles = [(String, String)]

findTestFiles :: [String] -> String -> IO TestFiles
findTestFiles exts = find
 where
   full dir x = dir ++ "/" ++ x
   isTestFile x = any (`isSuffixOf` x) exts
   isDirectory dir x
      | x `elem` [".", ".."] = return False 
      | otherwise            = doesDirectoryExist (full dir x)
   find dir = do
      xs <- getDirectoryContents dir 
               `catch` const (return [])
      let (files, rest) = partition isTestFile xs
      dirs <- filterM (isDirectory dir) rest
      list <- mapM (find . full dir) dirs
      return $
         [ (dir, file) | file <- files ] ++ concat list

testFile :: Configuration -> String -> String -> IO (Maybe Item)
testFile cfg dir file = do
   out <- readProcess (commandText cfg) (commandArgs sourceFile cfg) []
             `catch` (return . show)
   exp <- liftM Just (readFile expFile)
             `catch` (const $ return Nothing)
   let ok = maybe False (areEquivalent cfg out) exp && 
            maybe 0 length exp >= 0 -- trick to force evaluation
   if ok then return Nothing else do
      inp <- readFile sourceFile
      return $ Just $ Item sourceFile expFile inp out exp
 where
   base       = takeBaseName file
   sourceFile = dir ++ "/" ++ file
   expFile    = dir ++ "/" ++ base ++ expExtension cfg
   
normalizeNewlines :: String -> String
normalizeNewlines = rec 
 where
   rec [] = []
   rec ('\r' : '\n' : xs) = '\n' : rec xs
   rec (x:xs) = x : rec xs

difference :: Configuration -> FilePath -> String -> IO String
difference cfg expFile out = do
   (_, txt, _) <- readProcessWithExitCode (diffCommand cfg) 
                     (diffArgs cfg ++ [expFile, "-"]) out
   return txt
 `catch` 
   \e -> return (show e)
   
areEquivalent :: Configuration -> String -> String -> Bool
areEquivalent cfg s1 s2 = 
   let f = filter (diffFilter cfg) . lines . normalizeNewlines
   in f s1 == f s2

data Item = Item
   { inputFile    :: FilePath
   , expectedFile :: FilePath
   , inputText    :: String
   , outputText   :: String
   , expectedText :: Maybe String
   }
 deriving Eq
    
data Mode = Input | Output | Expected | Difference deriving Show

modes :: [Mode]
modes = [Input, Output, Expected, Difference]

showItem :: Configuration -> Mode -> Item -> IO String
showItem cfg mode item = 
   case mode of
      Input      -> return (inputText item)
      Output     -> return (outputText item)
      Expected   -> return (fromMaybe msg $ expectedText item)
      Difference 
         | hasExp    -> difference cfg (expectedFile item) (outputText item)
         | otherwise -> return msg
 where 
   hasExp = isJust (expectedText item)
   msg    = "No expected file for " ++ show (inputFile item) 