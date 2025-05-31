-- find index of char in list of chars

indexOf :: Char -> [Char] -> Int
indexOf ch [] = undefined
indexOf ch (x:xs) | x == ch = 0
                  | otherwise = 1 + indexOf ch xs
