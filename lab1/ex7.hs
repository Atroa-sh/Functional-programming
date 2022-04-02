not' :: Bool -> Bool
not' True = False
not' False = True
isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _      = False
or' :: (Bool, Bool) -> Bool
and' :: (Bool, Bool) -> Bool
nand' :: (Bool, Bool) -> Bool
xor' :: (Bool, Bool) -> Bool

or' (False,False) = False
or' _ = True

and' (True,True) = True
and' _ = False

nand' (True,True) = False
nand' _ = True

xor' (False,False) = False
xor' (True,True) = False
xor' _ = True

-- Takie podejsice jest szybsze niz rozpisywanie kazdego przypadku osobno
-- np. jesli or' (True,True) = True jest wzorcem pasujacym do naszego
-- to program i tak musi sprawdzic wczesniejsze wzorce
-- jesli wszystkie kombinacje zwracajace True zapakujemy do jednego _
-- to dla niektorych wypadkow program bedize musial sprawdzic mniejsza
-- liczbe przypadkow



