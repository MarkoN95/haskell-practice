module OneTimePad where

otp :: [Bool] -> [Bool] -> [Bool]
otp = zipWith (\a b -> (a && (not b)) || ((not a) && b))
