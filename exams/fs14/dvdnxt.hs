module DvdNxt where

dvdNxt :: [Int] -> [Int]
dvdNxt [] = []
dvdNxt [x] = [x]
dvdNxt (x:y:zs)
  | y `mod` x == 0 = x : dvdNxt (y:zs)
  | otherwise = dvdNxt (y:zs)

stableDN :: [Int] -> [Int]
stableDN x
  | x == dvdNxt x = x
  | otherwise = stableDN (dvdNxt x)

prepend :: String -> [[String]] -> [String]
prepend str = concatMap (map (str++))
