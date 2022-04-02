sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1

absInt :: Int -> Int -- absInt 2 = absInt (-2) = 2
absInt n = if n <0
    then (-n)
    else n

min2Int :: (Int, Int) -> Int -- min (1,2) = 1, min (-1, -1) = -1
min2Int (x,y) = if x<=y
    then x
    else y

min3Int :: (Int, Int, Int) -> Int
min3Int (x,y,z) = min2Int ((min2Int (x,y)),z)

toUpper :: Char -> Char
toLower :: Char -> Char
toUpper x = toEnum (fromEnum (x) - 32)
toLower x = toEnum (fromEnum (x) + 32)

isDigit :: Char -> Bool
charToNum :: Char -> Int
isDigit x = fromEnum (x) <= 57 && fromEnum (x) >= 48
charToNum x = fromEnum x --nie wiem czy o to chodzi

romanDigit :: Char -> String
romanDigit x = romanDigitList !! (fromEnum (x) - 49)
romanDigitList :: [String]
romanDigitList = "I":"II":"III":"IV":"V":"VI":"VII":"VIII":"IX":[]






--romanDigitList !! (fromEnum (x) - 48)