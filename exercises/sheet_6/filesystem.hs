module FileSystemEntries where

data FSEntry = Folder String [FSEntry] | File String String

fFSE :: (String -> [a] -> a) -> (String -> String -> a) -> FSEntry -> a
fFSE folder file (File n v) = file n v
fFSE folder file (Folder n es) = folder n (map (fFSE folder file) es)

number :: FSEntry -> Int
number = fFSE (\_ es -> 1 + sum es) (\_ _ -> 1)

count :: Char -> FSEntry -> Int
count ch = fFSE (\_ es -> sum es) (\_ v -> length $ filter (== ch) v)

paths :: FSEntry -> [String]
paths = fFSE (\fldrName partialPaths ->  concatMap (map ((fldrName++"/")++)) partialPaths) (\fName _ -> [fName])
