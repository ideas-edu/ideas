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
-- Support for Unicode
--
-----------------------------------------------------------------------------

module Ideas.Text.XML.Unicode
   ( isExtender, isLetter, isDigit, isCombiningChar
   , decoding
   ) where

import Control.Arrow
import Data.Char (chr, ord)
import Data.List
import qualified Ideas.Text.UTF8 as UTF8

-- optimize for ascii characters
isLetter, isExtender, isDigit, isCombiningChar :: Char -> Bool
isLetter        c = inMap (ord c) letterMap
isExtender      c = inMap (ord c) extenderMap
isDigit         c = inMap (ord c) digitMap
isCombiningChar c = inMap (ord c) combiningCharMap

inMap :: Int -> [(Int, Int)] -> Bool
inMap n = rec
 where
   rec [] = False
   rec ((x, y):zs)
      | n < x     = False
      | n <= y    = True
      | otherwise = rec zs

f :: Char -> (Char, Char)
f c = (c, c)

intpairs :: [(Char, Char)] -> [(Int, Int)]
intpairs = map (ord *** ord)

letterMap :: [(Int, Int)]
letterMap = baseCharMap `merge` ideographicMap -- `merge` controlMap `merge` extraMap

merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
merge (x:xs) (y:ys)
   | fst x <= fst y = x:merge xs (y:ys)
   | otherwise      = y:merge (x:xs) ys
merge xs ys = xs++ys

{-
extraMap :: [(Char, Char)]
extraMap = map f "\161\170\184\185"

controlMap :: [(Char, Char)]
controlMap = [ ('\x7F', '\x84'), ('\x86', '\x9F'), ('\xFDD0', '\xFDDF'),
   ('\x1FFFE', '\x1FFFF'), ('\x2FFFE', '\x2FFFF'), ('\x3FFFE', '\x3FFFF'),
   ('\x4FFFE', '\x4FFFF'), ('\x5FFFE', '\x5FFFF'), ('\x6FFFE', '\x6FFFF'),
   ('\x7FFFE', '\x7FFFF'), ('\x8FFFE', '\x8FFFF'), ('\x9FFFE', '\x9FFFF'),
   ('\xAFFFE', '\xAFFFF'), ('\xBFFFE', '\xBFFFF'), ('\xCFFFE', '\xCFFFF'),
   ('\xDFFFE', '\xDFFFF'), ('\xEFFFE', '\xEFFFF'), ('\xFFFFE', '\xFFFFF'),
   ('\x10FFFE', '\x10FFFF')] -}

baseCharMap :: [(Int, Int)]
baseCharMap = intpairs
   [ ('\x0041','\x005A'), ('\x0061','\x007A'), ('\x00C0','\x00D6'),
   ('\x00D8','\x00F6'), ('\x00F8','\x00FF'), ('\x0100','\x0131'),
   ('\x0134','\x013E'), ('\x0141','\x0148'), ('\x014A','\x017E'),
   ('\x0180','\x01C3'), ('\x01CD','\x01F0'), ('\x01F4','\x01F5'),
   ('\x01FA','\x0217'), ('\x0250','\x02A8'), ('\x02BB','\x02C1'), f '\x0386' ,
   ('\x0388','\x038A'), f '\x038C' , ('\x038E','\x03A1'), ('\x03A3','\x03CE'),
   ('\x03D0','\x03D6'), f '\x03DA' , f '\x03DC' , f '\x03DE' , f '\x03E0' ,
   ('\x03E2','\x03F3'), ('\x0401','\x040C'), ('\x040E','\x044F'),
   ('\x0451','\x045C'), ('\x045E','\x0481'), ('\x0490','\x04C4'),
   ('\x04C7','\x04C8'), ('\x04CB','\x04CC'), ('\x04D0','\x04EB'),
   ('\x04EE','\x04F5'), ('\x04F8','\x04F9'), ('\x0531','\x0556'), f '\x0559' ,
   ('\x0561','\x0586'), ('\x05D0','\x05EA'), ('\x05F0','\x05F2'),
   ('\x0621','\x063A'), ('\x0641','\x064A'), ('\x0671','\x06B7'),
   ('\x06BA','\x06BE'), ('\x06C0','\x06CE'), ('\x06D0','\x06D3'), f '\x06D5' ,
   ('\x06E5','\x06E6'), ('\x0905','\x0939'), f '\x093D' , ('\x0958','\x0961'),
   ('\x0985','\x098C'), ('\x098F','\x0990'), ('\x0993','\x09A8'),
   ('\x09AA','\x09B0'), f '\x09B2' , ('\x09B6','\x09B9'), ('\x09DC','\x09DD'),
   ('\x09DF','\x09E1'), ('\x09F0','\x09F1'), ('\x0A05','\x0A0A'),
   ('\x0A0F','\x0A10'), ('\x0A13','\x0A28'), ('\x0A2A','\x0A30'),
   ('\x0A32','\x0A33'), ('\x0A35','\x0A36'), ('\x0A38','\x0A39'),
   ('\x0A59','\x0A5C'), f '\x0A5E' , ('\x0A72','\x0A74'), ('\x0A85','\x0A8B'),
   f '\x0A8D' , ('\x0A8F','\x0A91'), ('\x0A93','\x0AA8'), ('\x0AAA','\x0AB0'),
   ('\x0AB2','\x0AB3'), ('\x0AB5','\x0AB9'), f '\x0ABD' , f '\x0AE0' ,
   ('\x0B05','\x0B0C'), ('\x0B0F','\x0B10'), ('\x0B13','\x0B28'),
   ('\x0B2A','\x0B30'), ('\x0B32','\x0B33'), ('\x0B36','\x0B39'), f '\x0B3D' ,
   ('\x0B5C','\x0B5D'), ('\x0B5F','\x0B61'), ('\x0B85','\x0B8A'),
   ('\x0B8E','\x0B90'), ('\x0B92','\x0B95'), ('\x0B99','\x0B9A'), f '\x0B9C' ,
   ('\x0B9E','\x0B9F'), ('\x0BA3','\x0BA4'), ('\x0BA8','\x0BAA'),
   ('\x0BAE','\x0BB5'), ('\x0BB7','\x0BB9'), ('\x0C05','\x0C0C'),
   ('\x0C0E','\x0C10'), ('\x0C12','\x0C28'), ('\x0C2A','\x0C33'),
   ('\x0C35','\x0C39'), ('\x0C60','\x0C61'), ('\x0C85','\x0C8C'),
   ('\x0C8E','\x0C90'), ('\x0C92','\x0CA8'), ('\x0CAA','\x0CB3'),
   ('\x0CB5','\x0CB9'), f '\x0CDE' , ('\x0CE0','\x0CE1'), ('\x0D05','\x0D0C'),
   ('\x0D0E','\x0D10'), ('\x0D12','\x0D28'), ('\x0D2A','\x0D39'),
   ('\x0D60','\x0D61'), ('\x0E01','\x0E2E'), f '\x0E30' , ('\x0E32','\x0E33'),
   ('\x0E40','\x0E45'), ('\x0E81','\x0E82'), f '\x0E84' , ('\x0E87','\x0E88'),
   f '\x0E8A' , f '\x0E8D' , ('\x0E94','\x0E97'), ('\x0E99','\x0E9F'),
   ('\x0EA1','\x0EA3'), f '\x0EA5' , f '\x0EA7' , ('\x0EAA','\x0EAB'),
   ('\x0EAD','\x0EAE'), f '\x0EB0' , ('\x0EB2','\x0EB3'), f '\x0EBD' ,
   ('\x0EC0','\x0EC4'), ('\x0F40','\x0F47'), ('\x0F49','\x0F69'),
   ('\x10A0','\x10C5'), ('\x10D0','\x10F6'), f '\x1100' , ('\x1102','\x1103'),
   ('\x1105','\x1107'), f '\x1109' , ('\x110B','\x110C'), ('\x110E','\x1112'),
   f '\x113C' , f '\x113E' , f '\x1140' , f '\x114C' , f '\x114E' , f '\x1150' , ('\x1154','\x1155') ,
   f '\x1159' , ('\x115F','\x1161'), f '\x1163' , f '\x1165' , f '\x1167' , f '\x1169' ,
   ('\x116D','\x116E'), ('\x1172','\x1173'), f '\x1175' , f '\x119E' , f '\x11A8' ,
   f '\x11AB' , ('\x11AE','\x11AF'), ('\x11B7','\x11B8'), f '\x11BA' ,
   ('\x11BC','\x11C2'), f '\x11EB' , f '\x11F0' , f '\x11F9' , ('\x1E00','\x1E9B'),
   ('\x1EA0','\x1EF9'), ('\x1F00','\x1F15'), ('\x1F18','\x1F1D'),
   ('\x1F20','\x1F45'), ('\x1F48','\x1F4D'), ('\x1F50','\x1F57'), f '\x1F59' ,
   f '\x1F5B' , f '\x1F5D' , ('\x1F5F','\x1F7D'), ('\x1F80','\x1FB4'),
   ('\x1FB6','\x1FBC'), f '\x1FBE' , ('\x1FC2','\x1FC4'), ('\x1FC6','\x1FCC'),
   ('\x1FD0','\x1FD3'), ('\x1FD6','\x1FDB'), ('\x1FE0','\x1FEC'),
   ('\x1FF2','\x1FF4'), ('\x1FF6','\x1FFC'), f '\x2126' , ('\x212A','\x212B'),
   f '\x212E' , ('\x2180','\x2182'), ('\x3041','\x3094'), ('\x30A1','\x30FA'),
   ('\x3105','\x312C'), ('\xAC00','\xD7A3') ]

ideographicMap :: [(Int, Int)]
ideographicMap = intpairs
   [ ('\x4E00','\x9FA5'),
   f '\x3007' , ('\x3021','\x3029') ]

combiningCharMap :: [(Int, Int)]
combiningCharMap = intpairs
   [('\x0300','\x0345'), ('\x0360','\x0361'), ('\x0483','\x0486'), ('\x0591','\x05A1'),
   ('\x05A3','\x05B9'), ('\x05BB','\x05BD'),  f '\x05BF' , ('\x05C1','\x05C2'),
   f '\x05C4' , ('\x064B','\x0652'), f '\x0670' , ('\x06D6','\x06DC'),
   ('\x06DD','\x06DF'), ('\x06E0','\x06E4'), ('\x06E7','\x06E8'),
   ('\x06EA','\x06ED'), ('\x0901','\x0903'), f '\x093C' , ('\x093E','\x094C'),
   f '\x094D' , ('\x0951','\x0954'), ('\x0962','\x0963'), ('\x0981','\x0983'),
   f '\x09BC' , f '\x09BE' , f '\x09BF' , ('\x09C0','\x09C4'), ('\x09C7','\x09C8'),
   ('\x09CB','\x09CD'), f '\x09D7' , ('\x09E2','\x09E3'), f '\x0A02' , f '\x0A3C' ,
   f '\x0A3E' , f '\x0A3F' , ('\x0A40','\x0A42'), ('\x0A47','\x0A48'),
   ('\x0A4B','\x0A4D'), ('\x0A70','\x0A71'), ('\x0A81','\x0A83'), f '\x0ABC' ,
   ('\x0ABE','\x0AC5'), ('\x0AC7','\x0AC9'), ('\x0ACB','\x0ACD'),
   ('\x0B01','\x0B03'), f '\x0B3C' , ('\x0B3E','\x0B43'), ('\x0B47','\x0B48'),
   ('\x0B4B','\x0B4D'), ('\x0B56','\x0B57'), ('\x0B82','\x0B83'),
   ('\x0BBE','\x0BC2'), ('\x0BC6','\x0BC8'), ('\x0BCA','\x0BCD'), f '\x0BD7' ,
   ('\x0C01','\x0C03'), ('\x0C3E','\x0C44'), ('\x0C46','\x0C48'),
   ('\x0C4A','\x0C4D'), ('\x0C55','\x0C56'), ('\x0C82','\x0C83'),
   ('\x0CBE','\x0CC4'), ('\x0CC6','\x0CC8'), ('\x0CCA','\x0CCD'),
   ('\x0CD5','\x0CD6'), ('\x0D02','\x0D03'), ('\x0D3E','\x0D43'),
   ('\x0D46','\x0D48'), ('\x0D4A','\x0D4D'), f '\x0D57' , f '\x0E31' ,
   ('\x0E34','\x0E3A'), ('\x0E47','\x0E4E'), f '\x0EB1' , ('\x0EB4','\x0EB9'),
   ('\x0EBB','\x0EBC'), ('\x0EC8','\x0ECD'), ('\x0F18','\x0F19'), f '\x0F35' ,
   f '\x0F37' , f '\x0F39' , f '\x0F3E' , f '\x0F3F' , ('\x0F71','\x0F84'),
   ('\x0F86','\x0F8B'), ('\x0F90','\x0F95'), f '\x0F97' , ('\x0F99','\x0FAD'),
   ('\x0FB1','\x0FB7'), f '\x0FB9' , ('\x20D0','\x20DC'), f '\x20E1' ,
   ('\x302A','\x302F'), f '\x3099' , f '\x309A' ]

digitMap :: [(Int, Int)]
digitMap = intpairs [ ('\x0030','\x0039'),
   ('\x0660','\x0669'), ('\x06F0','\x06F9'), ('\x0966','\x096F'),
   ('\x09E6','\x09EF'), ('\x0A66','\x0A6F'), ('\x0AE6','\x0AEF'),
   ('\x0B66','\x0B6F'), ('\x0BE7','\x0BEF'), ('\x0C66','\x0C6F'),
   ('\x0CE6','\x0CEF'), ('\x0D66','\x0D6F'), ('\x0E50','\x0E59'),
   ('\x0ED0','\x0ED9'), ('\x0F20','\x0F29')]

extenderMap ::  [(Int, Int)]
extenderMap = intpairs [f '\x00B7' , f '\x02D0' ,
   f '\x02D1' , f '\x0387' , f '\x0640' , f '\x0E46' , f '\x0EC6' , f '\x3005' , ('\x3031','\x3035')
   , ('\x309D','\x309E'), ('\x30FC','\x30FE') ]

decoding :: String -> Maybe String
decoding xs
   | "\255\254" `isPrefixOf` xs =
        return (decode16 $ drop 2 xs)
   | "\254\255" `isPrefixOf` xs =
        return (decode16X $ drop 2 xs)
   | "\239\187\191" `isPrefixOf` xs =
        UTF8.decode (drop 3 xs)
   | otherwise =
        UTF8.decode xs

decode16 :: String -> String
decode16 []  = []
decode16 [x] = [x]
decode16 (a:b:rest) = chr (ord b * 256 + ord a) : decode16 rest

decode16X :: String -> String
decode16X []  = []
decode16X [x] = [x]
decode16X (a:b:rest) = chr (ord b + ord a * 256) : decode16X rest