f1 :: Double -> Double
f1 = (\x -> x-2.0)

f2 :: Double -> Double -> Double
f2 = (\x y -> sqrt(x^2 + y^2))

f3 :: Integer -> Integer -> Integer -> Double
f3 = (\x y z -> sqrt(fromIntegral(x)^2 + fromIntegral(y)^2 + fromIntegral(z)^2))

f4' :: Integer -> Integer
f4' = (\y -> 2*y)

f5' :: Integer -> Integer
f5' = (\x -> x*2)

f6' :: Integer -> Integer
f6' = (\x -> 2^x)

f7' :: Integer -> Integer
f7' = (\x -> x^2)

f8' :: Double -> Double
f8' = (\x -> x/3)

f9' :: Integer -> Integer
f9' = (\x -> 4-x)

abs' :: Integer -> Integer
abs' = (\x -> if x>0 then x else -x)

f7 :: Integer -> Bool
f7 = (\x -> if x `mod`2==0 then True else False)

f8 = (\x -> let y=sqrt(x) in 2 * y^3 * (y - 1))

--f9 = (\x -> if x==1 then 3 else 0)