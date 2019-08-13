module RoseTree where

data Rose a = Leaf a | Node [Rose a]

foldRose :: (a -> b) -> ([b] -> b) -> Rose a -> b
foldRose fl fr (Leaf a) = fl a
foldRose fl fr (Node roses) = fr (map (foldRose fl fr) roses)

mapRose :: (a -> b) -> Rose a -> Rose b
mapRose f = foldRose (\a -> Leaf (f a)) (\as -> Node as)
