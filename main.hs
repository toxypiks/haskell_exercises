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

fst (x, y) = x
snd (x, y) = y

first (x, y, z) = x
second (x, y, z) = y
third (x, y, z) = z

-- you can also use the dont care operator e.g. third (_, _, z) = z

head' (x:xs) = x
tail' (x:xs) = xs

null' [] = True
null' (_:_) = False

len [] = 0
len (x:xs) = 1 + len(xs)

last' [] = error "init: empty list"
last' [x] = x
last' (_: xs) = last' xs

init' [] = error "init: empty list"
init' [x] = [] -- alternative: [_] = []
init' (x:xs) = x:init' xs

[] +++ ys = ys
(x:xs) +++ ys = x:(xs +++ ys)

my_reverse [] = []
my_reverse (x:xs) =  (my_reverse xs) ++ [x] -- without parantheses [x] no ++ list operator

my_reverse' xs = f xs []
     where f [] acc = acc
           f (y:ys) acc = f ys (y:acc) -- Umwandlung in endständige Rekursion

my_elem _ [] = False
my_elem elem (x:xs)
  | elem == x = True
  | otherwise = my_elem elem xs


teiler n = [x | x <- [1..n], n `mod` x == 0]

mymap fct [] = []
mymap fct (x:xs) = fct x:mymap fct xs

mymaplc f xs = [ f x | x <- xs]

cons_if f x xs = if f x then x:xs else xs

my_filter arg [] = []
my_filter arg (x:xs) = cons_if arg x (my_filter arg xs)

my_filterlc f xs = [x | x <- xs, f x]

my_sum [] = 0
my_sum (x:xs) = x + my_sum xs

my_sumf xs = foldl helper 0 xs -- start value 0
             where helper x y = x + y

my_zip _ [] _ = []
my_zip _ _ [] = []
my_zip fct (x:xs) (y:ys) = fct x y : my_zip fct xs ys

stringList' [] = []
stringList' (x:xs) = show x:stringList' xs

data Konto = Konto {
      nummer :: Int,
      inhaber :: String,
      stand :: Float} deriving (Eq, Show)

{--main = do
  putStrLn "Type temperature in Celsius: "
  tmp_celsius <- getLine
  let tmp_fahrenheit = read tmp_celsius * 9.0 / 5.0 + 32.0
  let response = "Temperature in Fahrenheit: " ++ show tmp_fahrenheit
  putStrLn response
--}
