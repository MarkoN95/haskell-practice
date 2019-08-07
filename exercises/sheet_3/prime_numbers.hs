module PrimeNumbers where

prime :: Int -> Bool
prime n = n > 1 && length [x | x <- [1..floor $ sqrt $ fromIntegral n], n `mod` x == 0] == 1

primes :: Int -> [Int]
primes m = [p | p <- [2..m], prime p]

firstPrimes :: Int -> [Int]
firstPrimes m = take m [p | p <- [2..], prime p]
