newtype MyInt = MkMyInt Int
instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2
-- inne operacje z klasy Ord rowniez dzialaja. minimal w :i Ord wskazuje na <=
-- wnioskowac by mozna ze tyle funkcji trzeba zdefiniowac by "system" dzialal
-- chodz nie jestem pewien poprawnosci tego wniosku

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)
instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i


data Fraction a = Fraction {num::a, denom::a}
instance Eq a => Eq (Fraction a) where
    (==) Fraction {num=num1,denom=denom1} Fraction {num=num2, denom=denom2} = num1==num2 && denom1==denom2

instance Show a => Show (Fraction a) where
    show (Fraction {num=num1,denom=denom1}) = "Fraction " ++ show num1 ++ ", " ++ show denom1


