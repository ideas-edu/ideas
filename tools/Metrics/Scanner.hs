module Scanner (scanSystem, scanModule) where

import Control.Monad
import Data.Char
import Data.List
import Data.Monoid
import qualified Data.Map as M
import Model
import System.Directory

scanSystem :: FilePath -> IO System
scanSystem path = do
   files <- findFiles path
   ms    <- mapM scanModule files
   return (System path (organize (qualifiers . qname) ms))

findFiles :: FilePath -> IO [FilePath]
findFiles path 
   | ".hs" `isSuffixOf` path = do
        b <- doesFileExist path
        return [path | b]
   | otherwise = do
        cont <- getDirectoryContents path
        let fs = filter ((/= ".") . take 1) cont
        rest <- mapM (findFiles . ((path++"/")++)) fs
        return $ concat rest
      `catch` 
        \_ -> return []

scanModule :: FilePath -> IO Module
scanModule file = do
   src <- readFile file
   let xs   = map noTrailingSpace . noBlankLines . lines . stripComment $ src
       txt  = unlines xs
   return Module
      { moduleFile      = file
      , moduleQName     = getModuleName txt
      , moduleImports   = getImports xs
      , moduleFunctions = getFunctions xs
      , moduleSize      = sizeOf (unlines xs)
      , moduleOverall   = sizeOf src
      }

getModuleName :: String -> QName
getModuleName s
   | "module" `isPrefixOf` xs && length ys > 0 && length n > 0
               = makeQName n
   | otherwise = makeQName "Main"
 where
   xs       = dropWhile isSpace s
   (ys, zs) = span isSpace (drop 6 xs)
   n        = takeWhile (\x -> not (isSpace x) && x /= '(') zs

getImports :: [String] -> [QName]
getImports = nub . map (makeQName . getName . dropWhile isSpace . drop 6 . unlines) 
           . filter isImport . tails 
 where
   isImport (x:_) = "import" `isPrefixOf` x
   isImport _ = False
   
   getName xs 
      | "qualified" `isPrefixOf` xs = 
           takeWhile (\x -> not (isSpace x) && x /= '(') $ dropWhile isSpace $ drop 9 xs
      | otherwise = 
           takeWhile (\x -> not (isSpace x) && x /= '(') xs

getFunctions :: [String] -> [Function]
getFunctions = map (uncurry Function) . M.toList . foldr getInfo M.empty . filter isFunction . decls
 where
   isFunction xs = 
      let isReserved s = s `isPrefixOf` xs && 
                         all isSpace (take 1 (drop (length s) xs))
      in all (not . isReserved) reserved
   reserved = 
      [ "data", "type", "newtype", "class", "instance"
      , "import", "module", "default", "infix", "infixr", "infixl"
      ]
   getInfo s = case getName s of
                  Just n  -> M.insertWith mappend n (makeSize s)
                  Nothing -> id
   getName xs -- simplified
      | isAlpha (head xs) || take 1 xs == "_" = 
           let (ident, rest) = span (\c -> isAlphaNum c || c `elem` "_'") xs
           in case getOp (dropWhile isSpace rest) of
                 Just op -> Just op
                 Nothing -> Just ident
      | "(" `isPrefixOf` xs =
           getName (drop 1 xs) `mplus` getOp (drop 1 xs)
      | otherwise = Nothing
   
   getOp :: String -> Maybe String
   getOp (x:xs) | x `elem` syms && op `notElem` reservedOp = 
      Just op
    where
      syms = "!#$%&*+./<=>?@\\^|-~:"
      op   = x : takeWhile (`elem` syms) xs
      reservedOp = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]
   getOp _ = Nothing
   
   makeSize s = sizeOf s
   
decls :: [String] -> [String]
decls [] = []
decls (x:xs)
   | isDecl x  = unlines (x:ys) : decls zs
   | otherwise = decls xs
 where
   (ys, zs) = break isDecl xs
   isDecl (c:_) = not (isSpace c)
   isDecl _     = False

stripComment :: String -> String
stripComment [] = []
stripComment list@(x:xs) 
   | len >= 2               = stripComment (dropWhile (/= '\n') bs)
   | "{-" `isPrefixOf` list = rec 0 (tail xs)
   | x == '"'               = x : scanStr xs
   | x == '\''              = x : scanChar xs
   | isAlpha x              = x : scanIdent xs
   | otherwise              = x : stripComment xs
 where
   (as, bs) = span (== '-') list
   len = length as
   
   rec :: Int -> String -> String
   rec n ('{' : '-' : ys) = rec (n+1) ys
   rec n ('-' : '}' : ys) 
      | n == 0    = stripComment ys
      | otherwise = rec (n-1) ys
   rec n (y:ys) 
      | y == '\n' = '\n' : rec n ys
      | otherwise = rec n ys
   rec _ [] = 
      error $ "unterminated comment"
   
   scanChar :: String -> String
   scanChar (y:ys)
      | y == '\'' = ' ' : stripComment ys
      | y == '\\' = ' ' : take 1 ys ++ scanChar (drop 1 ys)
      | otherwise = ' ' : scanChar ys
   scanChar [] = 
      error $ "unterminated character" ++ show list
   
   scanStr :: String -> String
   scanStr (y:ys)
      | y == '"'  = ' ' : stripComment ys
      | y == '\\' = ' ' : take 1 ys ++ scanStr (drop 1 ys)
      | otherwise = ' ' : scanStr ys
   scanStr [] = 
      error "unterminated string" 

   scanIdent :: String -> String
   scanIdent ys = ident ++ stripComment rest
    where
      (ident, rest) = span (\c -> isAlphaNum c || c `elem` "_'") ys 
 
noBlankLines :: [String] -> [String]
noBlankLines = filter (not . all isSpace)

noTrailingSpace :: String -> String
noTrailingSpace = reverse . dropWhile isSpace . reverse  