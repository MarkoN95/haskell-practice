module PropLogic where

data Prop a = Var a | Not (Prop a) | And (Prop a) (Prop a) | Or (Prop a) (Prop a)

foldProp v n a o (Var p) = v p
foldProp v n a o (Not p) = n (foldProp v n a o p)
foldProp v n a o (And p1 p2) = (foldProp v n a o p1) `a` (foldProp v n a o p2)
foldProp v n a o (Or p1 p2) = (foldProp v n a o p1) `o` (foldProp v n a o p2)

evalProp :: (a -> Bool) -> Prop a -> Bool
evalProp v = foldProp v (not) (&&) (||)

propVars :: Eq a => Prop a -> [a]
propVars = foldProp (\a -> [a]) id aux aux
  where
    aux = (\l r -> l ++ (filter (\e -> not $ e `elem` l) r))

satProp :: Eq a => Prop a -> Bool
satProp p = True `elem` (map (\input -> evalProp (eval input) p) inputs)
  where
    eval input a = maybe False (\(n, v) -> v) (find (\(n, v) -> n == a) input)
    inputs = map (\s -> zip (propVars p) s) (powerSet (length (propVars p)))

instance (Show a) => Show (Prop a) where
  show (Var p) = show p
  show (Not p) = "(Not " ++ show p ++ ")"
  show (And p1 p2) = "(" ++ (show p1) ++ " && " ++ (show p2) ++ ")"
  show (Or p1 p2) = "(" ++ (show p1) ++ " || " ++ (show p2) ++ ")"
