sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs -- (+) (g x) (sumWith g xs)

prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs -- (*) (g x) (prodWith g xs)

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sumWith'' g  = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 1

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = loop (zip (init xs) (tail xs))
loop [] = True
loop ((x,y):xp) = if x<=y then loop xp else False

everySecond :: [t] -> [t]
everySecond xs = [xs !! x | x<-[0..length(xs)-1] , x`mod`2==0]

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] [] [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = loop' (zip (init xs) (tail xs))
loop' [] = True
loop' ((x,y):xp) = if x>=y then loop' xp else False