type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

-- find index of char in list of chars

indexOf :: Char -> Alphabet -> Int
indexOf ch [] = undefined
indexOf ch (x:xs) | x == ch = 0
                  | otherwise = 1 + indexOf ch xs

-- retrieve an element at a certain index

{-elemAt :: Int -> [Char] -> Char
elemAt x [] = undefined
elemAt x (y:ys) | x == indexOf y ys = y
elemAt x (y:ys) | otherwise = elemAt x+1 ys
-}

-- list of alphabet !! index
-- index = index of char in upperAlphabet + offset
-- index `mod` 26 to circle values between 0 and 26

upperRot :: Int -> Char -> Char
upperRot n ch = upperAlphabet !! ((indexOf ch upperAlphabet + n) `mod` 26)

lowerRot :: Int -> Char -> Char
lowerRot n ch = lowerAlphabet !! ((indexOf ch lowerAlphabet + n) `mod` 26)
