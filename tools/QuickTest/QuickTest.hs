module Main (main) where

import Configuration
import System.Directory
import Data.List
import Data.Maybe
import Control.Monad
import System.Process
import System.FilePath
import System.Time
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Control)
import Observable

main :: IO ()
main = start $ do
   initCfg <- readConfiguration
 
   r1 <- createControl (0, [])
   r2 <- createControl []
   r3 <- createControl initCfg
   r4 <- createControl 0
   r6 <- createControl Nothing
   r7 <- createControl False
   let state = State
          { workList=r1, workLoad=r4, itemRef=r2
          , startTime=r6
          , configuration=r3
          }
   

   -- Top frame and icon
   f <- frame [text := "QuickTest", bgcolor := white]
   iconCreateFromFile "quicktest.ico" sizeNull >>= topLevelWindowSetIcon f
         
   -- Status bar
   field1 <- statusField []
   field2 <- statusField [statusWidth := 100]
   field3 <- statusField [statusWidth := 100]
   set f [statusBar := [field1, field2, field3]] 
   
   panelUp   <- panel f []
   panelDown <- makePanelDown state f
   
   hg  <- hgauge panelUp 0 []
   st1 <- staticText panelUp []
   st2 <- staticText panelUp []
   st3 <- staticText panelUp []
   st5 <- staticText panelUp []
   b1  <- button panelUp [text := "Go!"]
   b9  <- button panelUp [text := "Show details"]
   bd  <- button panelUp [text := "Change"]
   
   timeTimer <- timer f 
      [enabled := False, interval := 100, on command := notifyObservers r6]
   
   addObserver r1 $ \(len, xs) -> do
      cfg   <- getValue (configuration state)
      total <- getValue r4
      set hg [selection := total-len]
      case xs of 
         []       -> do set st1 [text := testDirectory cfg]
                        set st2 [text := ""]
                        setValue r6 Nothing
         (d, f):_ -> do set st1 [text := d]
                        set st2 [text := f]
   
   addObserver r2 $ \xs -> do
      let msg = "Errors: " ++ show (length xs)
      set field3 [text := msg]
      set st3    [text := msg]

   addObserver r4 $ \n -> do
      gaugeSetRange hg n
      set field2 [text := "Tests: " ++ show n]
 
   addObserver r6 $ \mct -> do
      updateTime st5 mct
      set timeTimer [enabled := isJust mct ]
   
   addObserver r7 $ \b -> do
      set b9 [text := (if b then "Hide" else "Show") ++ " details"]
      set panelDown [visible := b]
      set f $ if b 
         then [ layout := margin 15 $ column 30 
                   [hfill $ widget panelUp, fill $ widget panelDown] 
              , size := sz 480 700 ] 
         else [ layout := margin 15 $ fill $ widget panelUp
              , size := sz 480 150 ]
   
   notifyObservers r1
   notifyObservers r2

   set b1 [on command := onGo state]
   set b9 [on command := changeValue r7 not]
   set f  [on idle    := onIdle state]
   
   set bd [on command := do 
      let msg = "Select directory with test files"
      cfg    <- getValue (configuration state)
      result <- dirOpenDialog f False msg (testDirectory cfg)
      case result of
         Just d  -> changeValue (configuration state)
                                (\cfg -> cfg {testDirectory = d})
         Nothing -> return () ]
   
   set panelUp [layout := column 10 
      [ row 10 [ column 10
           [ row 10 [label "Directory:", hfill $ widget st1]
           , row 10 [label "File: ", hfill $ widget st2]
           , row 10 [hfill $ widget st3, hfill $ widget st5]
           ], widget bd ]
      , hfill $ widget hg
      , hfill $ vspace 5
      , row 0 [hglue, widget b1, hglue, widget b9]
      ]]
   
   notifyObservers r7

makePanelDown :: State -> Window a -> IO (Panel ())
makePanelDown state w = do
   selected <- createControl (Difference, Nothing)
   -- Create widgets
   p  <- panel w []
   lc <- singleListBox p []
   rb <- radioBox p Horizontal (map show modes) [selection := 3]
   tc <- textCtrl p []
   b  <- button p [text := "Create"]
   textCtrlSetEditable tc False
   -- Event handlers
   set rb [on select := get rb selection >>= onSelectRadioBox selected]
   set lc [on select := get lc selection >>= onSelectList selected]
   set b  [on command := getValue selected >>= onCreate]
   addObserver selected (onShowItem tc)
   addObserver (itemRef state) $ \xs -> do
      i <- get lc selection 
      set lc [items := map show xs, selection := i] 
   -- Returning panel
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
      items <- getValue (itemRef state)
      let f (mode, _) =
             case drop i items of
                hd:_ | i >= 0 -> (mode, Just hd)
                _ -> (mode, Nothing)
      changeValue ctrl f

   onCreate (_, Just item) = do
      cfg <- getValue (configuration state)
      let (file, out) = case item of
                           NoExpected file _ out    -> (file, out)
                           Different file _ _ out _ -> (file, out)
          expFile = takeDirectory file ++ "/" ++ takeBaseName file ++ expExtension cfg
      writeFile expFile (normalizeNewlines out)
      xs <- getValue (itemRef state)
      let new = filter (/= item) xs
      setValue (itemRef state) new
   onCreate _ = return ()

   onShowItem w (mode, maybeItem) = do
      cfg <- getValue (configuration state)
      msg <- case maybeItem of
                Just (NoExpected file inp out) -> return $
                   case mode of
                      Input  -> inp
                      Output -> out
                      _      -> "No expected file for " ++ show file
                Just (Different _ expFile inp out exp) ->
                   case mode of
                      Input      -> return inp
                      Output     -> return out
                      Expected   -> return exp
                      Difference -> difference cfg expFile out
                _ -> return ""
      set w [text := msg]
     
onGo :: State -> IO ()
onGo state = do
   cfg       <- getValue (configuration state)
   testFiles <- findTestFiles (testExtensions cfg) (testDirectory cfg)
   clockTime <- getClockTime
   let len = length testFiles
   setValue (workLoad state) len
   setValue (itemRef state) []
   setValue (startTime state) (Just clockTime)
   setValue (workList state) (len, testFiles)

onIdle :: State -> IO Bool
onIdle state = do 
   (len, work) <- getValue (workList state)
   case work of
      (d, file):rest -> do
         testFile state d file
         setValue (workList state) (len-1, rest)
         return True
      _ ->
         return False -- no more work to do

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

testFile :: State -> String -> String -> IO ()
testFile state dir file = do
   cfg <- getValue (configuration state)
   let base = takeBaseName file
       sourceFile = dir ++ "/" ++ file
       expFile    = dir ++ "/" ++ base ++ expExtension cfg
   out <- readProcess (commandText cfg) (commandArgs cfg sourceFile) []
   do exp <- readFile expFile
      -- length<0 is simple trick to force reading whole file 
      -- (and close it afterwards)
      unless (areEquivalent cfg out exp || length exp < 0) $ do
         inp <- readFile sourceFile
         let item = Different (dir ++ "/" ++ file) expFile inp out exp
         changeValue (itemRef state) (++ [item])
    `catch` \_ -> do
       inp <- readFile sourceFile
       let item = NoExpected (dir ++ "/" ++ file) inp out
       changeValue (itemRef state) (++ [item])

normalizeNewlines :: String -> String
normalizeNewlines = rec 
 where
   rec [] = []
   rec ('\r' : '\n' : xs) = '\n' : rec xs
   rec (x:xs) = x : rec xs

difference :: Configuration -> FilePath -> String -> IO String
difference cfg expFile out = do
   (_, txt, _) <- readProcessWithExitCode (diffTool cfg) [expFile, "-"] out
   return txt
 `catch` 
   \e -> return (show e)
   
areEquivalent :: Configuration -> String -> String -> Bool
areEquivalent cfg s1 s2 = 
   let f = filter (diffFilter cfg) . lines . normalizeNewlines
   in f s1 == f s2
   
data State = State
   { workList      :: Control (Int, TestFiles)
   , workLoad      :: Control Int
   , itemRef       :: Control [Item]
   , startTime     :: Control (Maybe ClockTime)
   , configuration :: Control Configuration
   } 

data Item = NoExpected String String String | Different FilePath FilePath String String String
   deriving Eq

instance Show Item where
   show (NoExpected file _ _)    = file ++ " (no expected)"
   show (Different file _ _ _ _) = file
      
data Mode = Input | Output | Expected | Difference deriving Show

modes :: [Mode]
modes = [Input, Output, Expected, Difference]