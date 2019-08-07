module Tree where

data Tree t = Leaf | Node t (Tree t) (Tree t)

bfs :: Tree t -> [t]
bfs tree = walk_bfs [tree]
  where
    walk_bfs :: [Tree t] -> [t]
    walk_bfs [] = []
    walk_bfs [Leaf] = []
    walk_bfs xs = (map (\(Node a _ _) -> a) xs) ++ walk_bfs (concat (map children xs))
      where
        children :: Tree t -> [Tree t]
        children (Node _ Leaf Leaf) = []
        children (Node _ l Leaf) = [l]
        children (Node _ Leaf r) = [r]
        children (Node _ l r) = [l, r]

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

sortedTree :: Ord t => Tree t -> Bool
sortedTree = issorted . inorder
  where
    issorted [] = True
    issorted [x] = True
    issorted (x:y:xs) = x < y && issorted (y:xs)

inorder :: Ord t => Tree t -> [t]
inorder Leaf = []
inorder (Node x Leaf Leaf) = [x]
inorder (Node x l r) = (inorder l) ++ [x] ++ (inorder r)
