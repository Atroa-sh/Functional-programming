--length[(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a ^ 2 + b ^ 2 == c ^ 2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []
-- wyglada na poprawna
-- nie jest optymalna bo np mozemy sprawdzac i do wartosci roof(sqrt(n))
-- jesli przed tym progiem nie znalazlo dzielnika to juz go nie znajdzie
howManyPrimesin1000 = length [i | i <- [1..1000], isPrime i == True]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

allEqual :: Eq a => [a] -> Bool
allEqual xs = length(head(xs):[xs !! i | i <-[1..length(xs)-1], xs !! i == xs !! (i-1) ]) == length xs

