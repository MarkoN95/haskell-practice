module NQueens where

import Data.List

dot :: Int -> Int -> [[Int]]
dot n 1 = [[e] | e <- [1..n]]
dot n d = [low ++ [a] | a <- [1..n], low <- lows]
  where
    lows = dot n (d - 1)

generate :: Int -> [[Int]]
generate n = dot n n

test :: [Int] -> Bool
test assign = null $ filter (\t -> (not $ null [t1 | t1 <- threats t, t2 <- coords, t2 /= t, t1 == t2])) coords
  where
    n = length assign
    coords = (zip assign [1..n])
    threats (i, j) =
      [(a, b) | a <- [1..n], b <- [1..n], abs (i - a) == abs (j - b)] ++ -- same diagonal
      [((i + k) `mod` n, j) | k <- [1..(n-1)]] ++ -- same row
      [(i, (j + k) `mod` n) | k <- [1..(n-1)]] -- same col

naivequeens :: Int -> [[Int]]
naivequeens n = filter test $ generate n
