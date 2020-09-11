module Char where

import Data.Char

-- equal      :: String -> String -> Bool
-- isNumeral  :: String -> Bool
-- isBlank    :: String -> Bool
-- fromDigit  :: Char -> Int
-- toDigit    :: Int -> Char
-- shift      :: Int -> Char -> Char

msg :: String
msg = "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ JHLJBZ KPJABT HYJUBT LZA ULBAYVU"

equal :: String -> String -> Bool
equal a b = map toLower a == map toLower b

-- isNumeral :: String -> Bool
-- isNumeral s = and (map isDigit s)

isNumeral :: String -> Bool
isNumeral s = all isDigit s

isBlank :: String -> Bool
isBlank s = all isSpace s

toDigit :: Int -> Char
toDigit i = intToDigit i

fromDigit :: Char -> Int
fromDigit c = digitToInt c

shift :: Int -> Char -> Char
shift i c
  | isSpace c = c
  | isDigit c = c
  | otherwise = chr ((ord c - ord 'A' + i) `mod` 26  + ord 'A')

ceasarShift :: String -> Int -> String
ceasarShift s i
  | isNumeral s = s
  | isBlank s = s
  | otherwise = map (shift i) s

cypher :: String -> String -> Int -> String
cypher out s i
  | i > 27 = "End\n"
  | otherwise = cypher (show i ++ " is " ++ (ceasarShift s i) ++ " \n") s (i+1) ++ out