{-# LANGUAGE BangPatterns #-}

fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)
 --złozonosc wykladnicza

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]
fib2 n = fibs !! n
-- lioniowo obliczeniowo i pamieciowo, jesli obliczamy wartosc juz znana
-- to obliczeniowa stala

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' a [] = False
elem' a (x:xs) = if x==a then True
else elem' a xs

doubleAll :: Num t => [t] -> [t] -- doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll (x:xs) = [head(x:xs)*2] ++ doubleAll xs

squareAll :: Num t => [t] -> [t] -- squareAll [2,3] = [4,9]
squareAll [] = []
squareAll (x:xs) = [head(x:xs)^2] ++ squareAll (xs)

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven (x:xs) = if x `mod` 2 == 0 then [x]
else selectEven xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 = loop 1
 where loop acc []     = acc
       loop acc (x:xs) = loop (x * acc) xs

length'2 :: [a] -> Int
length'2 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (1 + acc) xs

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
   where loop !acc []     = acc
         loop !acc (x:xs) = loop (x + acc) xs
-- dla [1..9mil]
-- sum' -- okolo 5 sec 2,3 mild bytes
-- sum'3 -- okolo 5 sec 2,3 mild bytes
-- sum'4 -- okolo 3.25 1,9 mild bytes
-- w sum'4 acc nie jest lazy wiec funkcja nie musi wracac by obliczyc wczesniej leniwe wartosci

