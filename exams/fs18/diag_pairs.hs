module DiagParis where

natPairs :: [(Integer, Integer)]
natPairs = [(a,b) | x <- [1..], a <- [1..x], b <- [1..x], (a + b) == x]

rationals :: [(Integer, Integer)]
rationals = filter (\(a, b) -> gcd a b == 0) natPairs
