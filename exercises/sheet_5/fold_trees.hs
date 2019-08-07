module Foldable where

data Tree3 a = Node (Tree3 a) (Tree3 a) (Tree3 a) | Leaf a

foldTree3 node leaf = go
  where
    go (Node l m r) = node (go l) (go m) (go r)
    go (Leaf a)     = leaf a

instance Foldable Tree3 where
  foldr f initial (Leaf a) = f a initial
  foldr f initial (Node l m r) = foldr f (foldr f (foldr f initial r) m) l
