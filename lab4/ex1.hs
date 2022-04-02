polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

type CartesianCoord3D' a = (a,a,a)
type RollerCoord' a = (a,a,a)

rollerToCartesian' :: Floating a => RollerCoord' a -> CartesianCoord3D' a
rollerToCartesian' (r,phi,z) = (r * cos phi, r * sin phi, z)


type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String
personInfoToString' :: PersonInfoToStringType'
personInfoToString' (nm,snm,addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr


