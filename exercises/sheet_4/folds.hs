module ConcatMap where

concatMap' f = foldr aux e
  where
    aux curr acc = (f curr) ++ acc
    e = []


myFoldl f v l = undefined
