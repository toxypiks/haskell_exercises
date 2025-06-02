type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a'..'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A'..'Z']

digits :: Alphabet
digits = ['0'..'9']

isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet

isDigit :: Char -> Bool
isDigit char = char `elem` digits

isMisc :: Char -> Bool
isMisc char = char `notElem` lowerAlphabet ++ upperAlphabet ++ digits

-- find index of char in list of chars
indexOf :: Char -> Alphabet -> Int
indexOf ch [] = undefined
indexOf ch (x:xs) | x == ch = 0
                  | otherwise = 1 + indexOf ch xs

-- retrieve an element at a certain index
elemAt :: [a] -> Int -> a
elemAt [] _ = undefined
elemAt (x:xs) n
       | n < 0 = undefined
       | n == 0 = x
       | otherwise = elemAt xs (n - 1)

-- list of alphabet !! index
-- index = index of char in upperAlphabet + offset
-- index `mod` 26 to circle values between 0 and 26

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch =
    alphabet !! (((indexOf ch alphabet) + n) `mod` length alphabet)

upperRot :: Int -> Char -> Char
upperRot n ch = alphabetRot upperAlphabet n ch

lowerRot :: Int -> Char -> Char
lowerRot n ch = alphabetRot lowerAlphabet n ch

-- rotation function for arbitrary characters

rotChar :: Int -> Char -> Char
rotChar n ch
        | isLower ch = lowerRot n ch
        | isUpper ch = upperRot n ch
        | otherwise = ch

-- apply caesars cyper to a list of chars
caesar :: Int -> String -> String
caesar n [] = []
caesar n (x:xs) = rotChar n x : caesar n xs

-- using map
caesar_map :: Int -> String -> String
caesar_map n message = map (\ch -> rotChar n ch) message

rot13 :: String -> String
rot13 message = caesar 13 message
