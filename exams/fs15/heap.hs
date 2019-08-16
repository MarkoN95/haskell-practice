module Heap where

data Heap = Leaf | Node Int Heap Heap

getMin :: Heap -> Maybe Int
getMin Leaf = Nothing
getMin (Node x l r) = Just x

merge :: Heap -> Heap -> Heap
merge Leaf r = r
merge l Leaf = l
merge (Node x1 l1 r1) (Node x2 l2 r2)
 | x1 < x2 = (Node x1 (merge l1 (Node x2 l2 r2)) r1)
 | otherwise = (Node x2 (merge l2 (Node x1 l1 r1)) r2)

delMin :: Heap -> Heap
delMin Leaf = Leaf
delMin (Node x l r) = merge l r
