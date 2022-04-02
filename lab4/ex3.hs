data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt



data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt



data Expr a = Lit a  -- literal/value a, e.g. Lit 2 = 2
            |  Expr a :+: Expr a
            | Expr a :*: Expr a
            | Expr a :-: Expr a

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2
eval (e1 :-: e2) = eval e1 - eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (e1 :*: e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"
show' (e1 :-: e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"


depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT lt)  (depthOfBT rt)

preorder :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
preorder EmptyBT = []
preorder (NodeBT n lt rt) = [n] ++ (preorder lt) ++ (preorder rt)


inorder :: BinTree a -> [a] 
inorder EmptyBT = []
inorder (NodeBT n lt rt) =  (inorder lt) ++ [n] ++ (inorder rt)


postorder :: BinTree a -> [a] 
postorder EmptyBT = []
postorder (NodeBT n lt rt) =  (postorder lt) ++  (postorder rt) ++ [n]


mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = (NodeBT (f n) (mapBT f lt) (mapBT f rt) )

insert :: Ord a => a -> BinTree a -> BinTree a 
insert a EmptyBT = (NodeBT a EmptyBT EmptyBT)
insert a (NodeBT n lt rt)
    | n>a =  NodeBT n (insert a lt) rt
    | n==a = NodeBT n lt rt
    | n<a = NodeBT n lt (insert a rt)


list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)


instance Eq a => Eq (BinTree a) where
    (==) (NodeBT a1 lt1 rt1) (NodeBT a2 lt2 rt2) = (inorder (NodeBT a1 lt1 rt1) == inorder (NodeBT a2 lt2 rt2))
