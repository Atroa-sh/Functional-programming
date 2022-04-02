sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' [] = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum :: Num a => [a] -> a
sum x = sumWith (\y -> y) x

sumSqr x = sumWith (\y -> y^2) x

sumCube x = sumWith (\y -> y^3) x

sumAbs x = sumWith (\x -> if x>0 then x else -x) x

-- sumWith (\y -> y^5) [1..15]

prod' :: Num a => [a] -> a
prod' [] =1
prod' (x:xs) = x*prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] =1
prodWith f (x:xs) = f x * prodWith f xs

prodSqr x = prodWith (\y -> y^2) x

sumSqrt x = sumWith (\y -> sqrt(y)) x


    




