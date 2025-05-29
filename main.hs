mysum :: Int -> Int -> Int
mysum x y = x + y

fac :: Int -> Int
fac n | n == 0 = 1
      |otherwise = n * fac (n-1)

fibb :: Int -> Int
fibb n | n == 0 = 1
       | n == 1 = 1
       | otherwise = (fibb (n - 1)) + (fibb (n - 2))
