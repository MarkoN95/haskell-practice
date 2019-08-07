module Tasks where

match :: String -> Bool
match = aux 0 0

aux :: Int -> Int -> String -> Bool
aux rnd curly [] = (rnd == 0) && (curly == 0)
aux rnd curly (c:cs)
  | rnd == -1 || curly == -1 = False
  | c == '(' = aux (rnd + 1) curly cs
  | c == ')' = aux (rnd - 1) curly cs
  | c == '{' = aux rnd (curly + 1) cs
  | c == '}' = aux rnd (curly - 1) cs
  | otherwise = aux rnd curly cs


risers :: Ord a => [a] -> [[a]]
risers = foldl aux []
  where
    aux acc curr
      | acc == [] = [[curr]]
      | last (last acc) <= curr = (init acc) ++ [(last acc) ++ [curr]]
      | otherwise = acc ++ [[curr]]
