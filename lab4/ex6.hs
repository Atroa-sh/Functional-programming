class Mappable t where
  fMap :: (a -> b) -> t a -> t b

data Vec3D a = Vec3D {x::a, y::a, z::a} deriving Show

instance Mappable Vec3D where
  fMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)


newtype Pair a = Pair (a,a) deriving Show

instance Mappable Pair where
  fMap f (Pair (x,y)) = Pair (f x, f y)
  

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)
                 deriving Show

instance Mappable Maybe where
  fMap f (Just a) = Just (f a)
  fMap f Nothing = Nothing

instance Mappable Either where
  fMap f (Left a)= Left (f a)
  fMap f (Right a) = Right (f a)

instance Mappable ((->) a) where
  fMap f g = f . g

class VectorLike t where
 (|==|) :: Eq a => t a -> t a -> Bool
 (|+|), (|-|) :: (Num a) => t a -> t a -> t a
 (|*|) :: (Num a) => t a -> t a -> a
 (||?), (|-?) :: (Num a, Eq a) => t a -> t a -> Bool -- równoległość i prostopadłość
 vectLength :: Floating a => t a -> a
 unitVectOf :: Floating a => t a -> t a

instance VectorLike Vec3D where
  (|==|) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = a1==a2 && b1==b2 && c1==c2
  (|+|) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = Vec3D (a1+a2) (b1+b2) (c1+c2)
  (|-|) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = Vec3D (a1-a2) (b1-b2) (c1-c2)
  (|*|) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = Vec3D (b1*c2 - c1*b2) (c1*a2 - a1*c2) (a1*b2 - b1*a2)
  (||?) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = a1/a2==b1/b2 && b1/b2==c1/c2
  (|-?) (Vec3D a1 b1 c1) (Vec3D a2 b2 c2) = b1*c2 - c1*b2==0 && c1*a2 - a1*c2==0 && a1*b2 - b1*a2==0
  vectLength (Vec3D a1 b1 c1) = sqrt(a1^2 + b1^2 + c1^2)
  unitVectOf (Vec3D a1 b1 c1) = Vec3D (a1/sqrt(a1^2 + b1^2 + c1^2)) (b1/sqrt(a1^2 + b1^2 + c1^2)) (c1/sqrt(a1^2 + b1^2 + c1^2))
  



  