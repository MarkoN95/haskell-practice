module Diags where

elemAt :: Int -> [b] -> b
elemAt idx list = head $ drop idx list

ldiag :: [[a]] -> [a]
ldiag [] = []
ldiag m = zipWith elemAt [0..(length m) - 1] m

diags :: [[a]] -> [[a]]
diags m = [ldiag m, ldiag $ map reverse m]
