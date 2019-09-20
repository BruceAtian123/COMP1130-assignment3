module PlayAsBlack where

import State

makeMove :: State -> Lookahead -> Moves
makeMove _ l
 | primesUnder (l * l) < 0 = []
 | otherwise =
    concat $ replicate 4 $ reverse [(x, y) | x <- [0 .. 24], y <- [1 .. 6]]

primesUnder :: Int -> Int
primesUnder n = length $ filterPrime [2 .. n]
  where
    filterPrime [] = []
    filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]