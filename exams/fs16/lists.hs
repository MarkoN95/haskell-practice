module Lists where

pairSum :: [Int] -> Int -> [(Int, Int)]
pairSum l n = [(a, b) | a <- l, b <- l, a + b == n]

interlace :: [a] -> [a] -> [a]
interlace (x:xs) (y:ys) = x:y:(interlace xs ys)
interlace [] r = r
interlace l [] = l

open :: [Integer]
open = filter (\n -> ((length [d | d <- [1..n], n `mod` d == 0]) `mod` 2) == 1) [1..]
