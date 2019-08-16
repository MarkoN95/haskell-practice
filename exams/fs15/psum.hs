module Psum where

psum :: [Integer] -> [Integer]
psum l = aux l [0]
  where
    aux [] sums = sums
    aux (x:xs) sums = sums ++ aux xs [last sums + x]
