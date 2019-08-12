module Ternary where

data Ternary a = Leaf | Node a (Ternary a) (Ternary a) (Ternary a)

foldTer :: (a -> t -> t -> t -> t) -> t -> Ternary a -> t
foldTer f z Leaf = z
foldTer f z (Node n tl tm tr) = f n (foldTer f z tl) (foldTer f z tm) (foldTer f z tr)

mapTer :: (a -> b) -> Ternary a -> Ternary b
mapTer f = foldTer (\n tl tm tr -> (Node (f n) tl tm tr)) Leaf
