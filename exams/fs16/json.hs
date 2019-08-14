module Json where

data Json a = Val a | Obj [(String, Json a)]

foldJson :: (a -> b) -> ([(String, b)] -> b) -> Json a -> b
foldJson valf objf (Val a) = valf a
foldJson valf objf (Obj list) = objf (map (\(key, json) -> (key, foldJson valf objf json)) list)

values :: Json a -> [(String, a)]
values = foldJson (\a -> [("", a)]) (\list -> concatMap (\(key, json) -> map (\(jk, jv) -> (makeName key jk, jv)) json) list)
  where
    makeName :: String -> String -> String
    makeName k "" = k
    makeName k v = k ++ "." ++ v

instance (Eq a) => Eq (Json a) where
  (==) (Val l) (Val r) = l == r
  (==) (Obj _) (Val _) = False
  (==) (Val _) (Obj _) = False
  (==) (Obj l) (Obj r) = all (\a -> elem a r) l && all (\b -> elem b l) r
