concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs


concat'' (x:xs) = foldr (++) [] (x:xs)

-- concat $ map (\x -> [x*2]) $ [1..5] -- [2,4,6,8,10]
-- concatMap (\x -> [x*2]) [1..5] -- [2,4,6,8,10]
-- concatMap (\x -> x ++ "!") ["Ready", "Steady", "Go"]
