isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s
getElemAtIdx x s = last (reverse (drop (x-1) s) )
toUpper :: Char -> Char
toUpper x = toEnum (fromEnum (x) - 32)
toArr x = x: []
capitalize :: [Char] -> [Char]
capitalize w = 
    if fromEnum (head (w)) >=97 && fromEnum (head (w)) <=122
    then toArr (toUpper (head (w))) ++ drop 1 w
    else "To nie jest mala litera"




