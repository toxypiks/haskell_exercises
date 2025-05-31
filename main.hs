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

my_max :: Int -> Int -> Int
my_max a b | a > b = a
           |otherwise = b

my_if b a1 a2 = case b of
                  True -> a1
                  False -> a2

-- syntactic sugar for case syntax
my_if' True a1 _ = a1
my_if' False _ a2 = a2

squared_diff x y = (x-y) * (x-y)

squared_diff1 x y =
    let d = x - y
    in d * d

squared_diff2 x y = d * d
    where d = x - y

-- where and let are basically the same but you'll find where more often in code from other ppl
