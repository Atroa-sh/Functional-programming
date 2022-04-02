import Data.List

funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

--funcListExt = funcList ++ [\x -> sqrt(1 + x)]

sortDesc :: Ord a => [a] -> [a]
--sortDesc xs = (reverse . sort) xs
sortDesc xs = reverse (sort xs)

--(f . w3 1 2 . h) 3

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g [] = True
are2FunsEqAt f g (x:xs) = if (f x == g x) then are2FunsEqAt f g xs
else False

infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = (\x -> f (g (x)))

--(((,) $ 1) $ 2)
-- najbardziej czytelna osobisice jest dla mnie forma (f . g . h) 3


