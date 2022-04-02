import Data.Char
doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs



sqrElems :: Num a => [a] -> [a]
sqrElems [] =[]
sqrElems (x:xs) = x^2 : sqrElems xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElems' (x:xs) = map' (\x -> 2*x) (x:xs)
sqrElems' (x:xs) = map' (\x -> x^2) (x:xs)
lowerCase (x:xs) = map' (\x -> if fromEnum(x) <= 90 && fromEnum(x) >= 65 then toEnum(fromEnum(x)+32) else x) (x:xs)

doubleElems'' (x:xs) = [n*2 |n<-(x:xs)]
sqrElems'' (x:xs) = [n^2 | n<-(x:xs)]

lowerCase'' (x:xs) = [if fromEnum(n) <= 90 && fromEnum(n) >= 65 then toEnum(fromEnum(n)+32) else n | n <- (x:xs)]

--length . filter even $ [x*2 | x <- [1..10^7]]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt a (x:xs) = map ($ a) (x:xs)




