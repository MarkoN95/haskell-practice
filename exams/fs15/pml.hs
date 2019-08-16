module Pml where

data F a = AP a | Not (F a) | And (F a) (F a) | E (F a)

foldF :: (a -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b) -> F a -> b
foldF a not' and' e = go
  where
    go (AP ap) = a ap
    go (Not f) = not' (go f)
    go (And l r) = and' (go l) (go r)
    go (E f) = e (go f)

instance (Show a) => Show (F a) where
  show (AP ap) = show ap
  show (Not f) = "(Not " ++ (show f) ++ ")"
  show (And l r) = "(" ++ (show l) ++ " && " ++ (show r) ++ ")"
  show (E f) = "(E " ++ (show f) ++ ")"
