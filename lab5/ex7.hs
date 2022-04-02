
newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)


newtype MyTriple a = MyTriple (a,a,a) deriving Show
--instance Applicative MyTriple where
--  pure = MyTriple
--  (MyTriple (f1,f2,f3))<*> (a,b,c) = fmap f3 (a,b,c)
--dunno i gave up

instance Functor MyTriple where
    fmap f (MyTriple (a,b,c)) = MyTriple (f a,f b,f c)
