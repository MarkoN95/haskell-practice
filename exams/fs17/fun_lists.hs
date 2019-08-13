module FunLists where

group :: Eq a => [a] -> [[a]]
group [] = []
group l = aux [[head l]] (tail l)
  where
    aux soFar [] = soFar
    aux soFar (x:xs)
      | x == last (last soFar) = aux (init soFar ++ [last soFar ++ [x]]) xs
      | otherwise = aux (soFar ++ [[x]]) xs


encode :: Eq a => [a] -> [(a, Int)]
encode list = map (\e -> (head e, length e)) (group list)

decodeSingle :: (a, Int) -> [a]
decodeSingle (ch, n)
  | n == 1 = [ch]
  | otherwise = [ch] ++ decodeSingle (ch, n - 1)

decode :: [(a, Int)] -> [a]
decode = concatMap decodeSingle

iterate' :: (a -> a) -> a -> [a]
iterate' f x = [x] ++ iterate' f (f x)

looknsay :: [[Int]]
looknsay = iterate aux [1]
  where
    aux l = concatMap (\(t, n) -> [n, t]) (encode l)
