module ListTree where

data AList a b = AEnd a | ACons a b (AList a b)

ahead :: AList a b -> a
ahead (AEnd a) = a
ahead (ACons a b _) = a

alast :: AList a b -> a
alast (AEnd a) = a
alast (ACons a b rest) = ahead rest

aflat :: (a -> [b]) -> AList a b -> [b]
aflat f (AEnd a) = f a
aflat f (ACons a b rest) = (f a) ++ [b] ++ (aflat f rest)

data BTree key = Leaf [key] | Node (AList (BTree key) key)

inorder :: BTree key -> [key]
inorder (Leaf keys) = keys
inorder (Node keys) = aflat (\tree -> inorder tree) keys

lsorted :: Ord a => [a] -> Bool
lsorted (x:y:zs) = x <= y && lsorted (y:zs)
lsorted _ = True

bsorted :: Ord key => BTree key -> Bool
bsorted = lsorted . inorder
