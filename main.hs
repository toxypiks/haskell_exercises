mysum :: Int -> Int -> Int
mysum x y = x + y

fac :: Int -> Int
fac n | n == 0 = 1
      |otherwise = n * fac (n-1)

fibb :: Int -> Int
fibb n | n == 0 = 1
       | n == 1 = 1
       | otherwise = (fibb (n - 1)) + (fibb (n - 2))

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100 then x else x*2

boombang :: [Int] -> [String]
boombang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
