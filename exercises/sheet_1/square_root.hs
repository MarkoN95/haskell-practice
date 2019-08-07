module SquareRoot where

improve :: Double -> Double -> Double
improve x yn = (yn + (x / yn)) * 0.5

goodEnough :: Double -> Double -> Bool
goodEnough y y' = abs((y - y') / y') < 1.0e-20

newton :: Double -> Double -> Double
newton x yn
  | goodEnough next yn = next
  | otherwise = newton x next
  where
    next = improve x yn

root :: Double -> Double
root x = newton x 1
