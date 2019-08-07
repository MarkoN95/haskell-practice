module IsMagic where

import Data.List

eqRows :: [[Int]] -> Bool
eqRows [] = True
eqRows [r] = True
eqRows (row1:row2:rest) = (sum row1) == (sum row2) && (eqRows (row2:rest))

isMagic :: [[Int]] -> Bool
isMagic [] = True
isMagic m = (eqRows m) && eqRows (transpose m)
