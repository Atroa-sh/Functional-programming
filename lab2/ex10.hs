fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fstdivsecond :: Integral a => [a] -> Bool
fstdivsecond (x : y : _) | y `mod` x ==0 = True
fstdivsecond _ =False 

fstdivthird :: Integral a => [a] -> Bool
fstdivthird (x : _ : y : _) | y `mod` x ==0 = True
fstdivthird _ = False 

cie x = r * sqrt(2) * sqrt(1 - cos(x/r))
    where r = 3.75

