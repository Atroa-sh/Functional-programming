qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter (<=x) xs--[ y | y <- xs, y <= x ] --filter (>=x) xs
   rightPart xs = filter (>x) xs--[ y | y <- xs, y > x  ]

mSort [a] = [a]
mSort tab = concat [(mSort leftPart), (mSort rightPart)]
    where
   leftPart = [ y | y <- [1..(div)((length tab)) 2] ]
   rightPart = [ y | y <- [(div)((length tab)) 2 +1.. length tab -1] ]

isSorted :: [Int] -> Bool -- isSorted [1,2,2,3] = True
isSorted (x:[]) =True
isSorted (x:xs) = if x <= head xs then isSorted xs
else False

reverse' :: [a] -> [a] -- reverse [1,2,3] = [3,2,1]
reverse' [] = []
reverse' tab = [last tab] ++ reverse' (take (length tab -1) tab) 

zip' :: [a] -> [b] -> [(a,b)] -- zip' [1,2] [3,4] = [(1,3), (2,4)]
zip' [] [] = []
zip' t1 t2 = [(head t1,head t2)] ++ zip' (drop 1 t1) (drop 1 t2)

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] [] [] = []
zip3' t1 t2 t3 = [(head t1,head t2,head t3)] ++ zip3' (drop 1 t1) (drop 1 t2) (drop 1 t3)



