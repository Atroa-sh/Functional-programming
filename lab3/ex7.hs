onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 == 1 = x : onlyOdd xs
    | otherwise      = onlyOdd xs

onlyUpper [] =[]
onlyUpper (x:xs)
    | (fromEnum(x) <= 90 && fromEnum(x) >= 65) = x : onlyUpper xs
    | otherwise = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) 
    | p(x) == True = x : filter' p xs
    | otherwise = filter' p xs


onlyEven' (x:xs) = filter' (\x -> if x`mod`2==0 then True else False ) (x:xs)
onlyOdd' (x:xs)  = filter' (\x -> if x`mod`2==1 then True else False) (x:xs)
onlyUpper' (x:xs) = filter' (\x -> if fromEnum(x) <= 90 && fromEnum(x) >= 65 then True else False) (x:xs)

--length . onlyEven $ [1..10^6]
--length . filter even  $[1..10^6] 

--length ([x | x <- [1..10^6], x `mod` 2==0])