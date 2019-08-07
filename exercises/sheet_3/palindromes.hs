module Palindromes where

palindromes :: [String] -> [String]
palindromes s = [v ++ w | v <- s, w <-s, (v ++ w) == reverse (v ++ w)]
