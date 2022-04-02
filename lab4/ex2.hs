-- product type example (one constructor)
type X = Int
type Y = Int
data CartInt2DVec = MkCartInt2DVec X Y -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}


-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x


-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

data Cart3DVec a = Cart3DVec a a a

xcord (Cart3DVec a _ _) = a
ycord (Cart3DVec _ a _) = a
zcord (Cart3DVec _ _ a) = a



data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r)= pi*r^2
area (Rectangle a b)=a*b 

data TrafficLights = Red' | Yellow | Green


actionFor :: TrafficLights -> String
actionFor Red' = "Stop!"
actionFor Yellow = "Steady"
actionFor Green = "Go!"
