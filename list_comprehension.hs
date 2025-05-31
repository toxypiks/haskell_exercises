{- List comprehension in haskell follows the mathematical definition of such.
   Firstly, the output function is defined. After a pipe, the input set is speci   fied, followed by a predicate.-}

-- simple example that works with x values from 1 to 10 but only the ones greater than  12

x = [x*2 | x <- [1..10], x*2 >= 12]

-- function returns BOOM! if x < 10 and odd, else BANG! and all x have to be odd for both conditions
boombang :: [Int] -> [String]
boombang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- its possible to include several predicates

x1 = [x | x <- [10..20], x /= 13, x /=15, x /= 19]

-- and several lists

x2 = [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

-- length function

my_lenght xs = sum [1 | _ <- xs]

-- removing all lowercase letters from a string

removeNonUppercase str = [c | c <- str, c `elem` ['A'..'Z']]

-- nested list comprehensions for nested lists

x3 xxs = [[x | x <- xs, even x] | xs <- xxs]
