data Interval a = V a a

make :: Ord a => a -> a -> Interval a
make x y = V (min x y) (max x y)

instance (Ord a, Show a) => Show (Interval a) where
  show (V l u) = "[" ++ (show l) ++ "," ++ (show u) ++ "]"

intersect :: Ord a => [Interval a] -> Maybe (Interval a)
intersect [] = Nothing
intersect [x] = Just x
intersect list = foldl aux (Just (head list)) list
  where
    aux Nothing _ = Nothing
    aux (Just (V a b)) (V x y)
      | lo <= hi = Just (V lo hi)
      | otherwise = Nothing
      where
        lo = max a x
        hi = min b y
