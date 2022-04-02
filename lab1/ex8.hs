

absInt n =
 case (n >= 0) of
   True -> n
   _    -> -n

isItTheAnswer :: String -> Bool
not' :: Bool -> Bool
or' :: (Bool, Bool) -> Bool
and' :: (Bool, Bool) -> Bool
nand' :: (Bool, Bool) -> Bool
xor' :: (Bool, Bool) -> Bool

isItTheAnswer "Love" = True
isItTheAnswer _      = False

isItTheAnswer n =
    case (n == "Love") of
        True -> True
        _ -> False


or' n = 
    case n==(False,False) of
        True -> False
        _ -> True

and' n =
    case n==(True,True) of
        True -> True
        _ -> False

nand' n =
    case n==(True,True) of
        True -> False
        _ -> True

xor' n =
    case n of
        (True,False) -> True
        (False,True) -> True
        _ -> False

not' n =
    case n of
        True -> False
        _ -> True




