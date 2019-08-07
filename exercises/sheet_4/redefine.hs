module Redefine where

f :: [[a]] -> [a]
f = map reverse

g :: Eq a => [a] -> [a] -> [a]
g = curry (map fst . filter (uncurry (==)) . uncurry zip)


h :: [Int] -> Int
h = length . filter even
