module Complex where

re :: (Double, Double) -> Double
re (real, _) = real

im :: (Double, Double) -> Double
im (_, imaginary) = imaginary

conj :: (Double, Double) -> (Double, Double)
conj (real, imaginary) = (real, -imaginary)

add :: (Double, Double) -> (Double, Double) -> (Double, Double)
add (r1, i1) (r2, i2) = (r1 + r2, i1 + i2)

mult :: (Double, Double) -> (Double, Double) -> (Double, Double)
mult (r1, i1) (r2, i2) = (r1 * r2 - i1 * i2, i1 * r2 + r1 * i2)

absv :: (Double, Double) ->  Double
absv (real, imaginary) = sqrt (real^2 + imaginary^2)
