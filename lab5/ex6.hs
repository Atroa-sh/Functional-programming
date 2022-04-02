{-# LANGUAGE DeriveFunctor #-}

newtype Box a = MkBox a deriving (Show, Functor)


data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show, Functor )
-- działa


data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show,Functor )--działa


--instance Functor BinTree where
--    fmap _ EmptyBT = EmptyBT
--    fmap f (NodeBT a l r) = NodeBT (f a) (fmap f l) (fmap f r)
    --działa
