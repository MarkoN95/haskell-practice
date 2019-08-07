module Coins where

cntChange :: Int -> Int
cntChange = change [5, 10, 20, 50, 100, 200, 500]

change :: [Int] -> Int -> Int
change coins amount
  | amount == 0 = 1
  | amount < 0 || null coins = 0
  | otherwise = (change (tail coins) amount) + change coins (amount - head coins)
