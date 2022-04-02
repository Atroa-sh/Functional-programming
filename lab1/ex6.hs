absInt :: Int -> Int
absInt n | n >= 0    = n
         | otherwise = -n

sgn :: Int -> Int
min3Int :: (Int, Int, Int) -> Int -- min (1,2,3)=1, min (1,1,3)=1
sgn n
    | n >0 = 1
    | n < 0 = (-1)
    | otherwise = 0
min3Int (x,y,z)
    | x>=y && y>=z = z
    | y>=x && x>=z = z
    | x >= z && z>=y = y
    | z >= x && x>=y = y
    | z>=y && y>=x = x
    | y>=z && z>=x = x

isDigit :: Char -> Bool
isDigit x -- = fromEnum (x) <= 57 && fromEnum (x) >= 48
    | fromEnum x <= 57 && fromEnum x >= 48 = True
    | otherwise = False

