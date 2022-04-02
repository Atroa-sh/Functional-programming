sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> (\x -> x^4)
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n x = case n of
    0 -> 1
    _ -> (x^n)/fromIntegral(factorial(n)) + expApproxUpTo (n-1) x

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = (\x -> (f (x+h) - f x)/h)



