module Words where

split :: Char -> String -> [String]
split sep str = strSplit "" (str ++ [sep]) [] [sep]

strSplit :: String -> String -> [String] -> String -> [String]
strSplit soFar "" parts sep = parts
strSplit soFar (n:rest) parts sep
  | [n] == sep = strSplit "" rest (parts ++ [soFar]) sep
  | otherwise = strSplit (soFar ++ [n]) rest parts sep

isASpace :: Char -> Bool
isASpace ' ' = True
isASpace _ = False

toWords :: String -> [String]
toWords = (filter (/="")) . (split ' ')

countWords :: String -> Int
countWords = length . toWords
