module Types where

import Data.Char

(<:=) :: [a] -> (Int, a) -> [a]
(<:=) = undefined -- The actual implementation is not relevant

f1 :: Eq a => [a] -> a -> Int -> Int
f1 x y z
  | z < 0 || z >= length x = -1
  | x !! z == y = z
  | otherwise = f1 x y (z + 1)

f2 :: Eq a => [a] -> a -> Int
f2 x y = f1 x y 0

-- f3 :: [Char] -> Int -> [Char]
-- f4 :: [Char] -> [Char]
-- f5 :: [a] -> [Int] -> [Int] -> [a]
-- f6 :: [a] -> [a]
