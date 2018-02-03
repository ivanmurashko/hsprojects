module Elliptic 
( Point(..)
, Curve(..)
, points
, (.+.)
, (.*.)
) where

-- a^(-1)
inv :: Integer -> Integer -> Integer
inv a p = head $ filter (\x -> mod (x*a) p == 1) [1 .. p - 1]


data Curve = Curve Integer Integer Integer

instance Eq Curve where
    (==) (Curve a1 b1 p1) (Curve a2 b2 p2) = a1 == a2 && b1 == b2 && p1 == p2

instance Show Curve where
    show (Curve a b p) = "y^2 = (x^3 + " ++ show a ++ "x + " 
                         ++ show b ++ ") mod " ++ show p

data Point = PointZero | Point { xpos :: Integer, ypos :: Integer,  curve :: Curve } 

instance Eq Point where
    (==) PointZero PointZero = True
    (==) PointZero _ = False
    (==) _ PointZero = False
    (==) (Point x1 y1 c1) (Point x2 y2 c2) = x1 == x2 && y1 == y2 && c1 == c2

instance Show Point where
    show PointZero = "0"
    show p = "(" ++ show (xpos p) ++ "," ++ show (ypos p) ++ ")"

instance Monoid Point where 
    mempty = PointZero
    mappend = (.+.)

-- Addition of 2 points
(.+.) :: Point -> Point -> Point
a .+. PointZero = a
PointZero .+. a = a
p1@(Point x1 y1 c1@(Curve a b p)) .+. p2@(Point x2 y2 c2)  =
    if c1 /= c2
       then error "Invalid curve"
       else if mod (x1 - x2) p == 0 && mod (y1 + y2) p == 0
            then PointZero
            else let m = if mod (x1 - x2) p == 0
                         then
                             mod ((3 * x1^2 + a)*(inv (2*y1) p)) p
                         else
                             mod ((y1 - y2)*(inv (x1 - x2) p )) p
                     x = mod (m*m - x1 - x2) p
                     y = mod (-y1 + m*(x1 - x)) p
                 in Point x y c1

-- Scalar multiplication
(.*.) :: Int -> Point -> Point
n .*. p = foldl (.+.) PointZero $ take n $ repeat p
           
points :: Curve -> [Point]
points c@(Curve a b p) = PointZero : (map (\(x,y) -> Point x y c) $ filter ( \(x,y) -> (mod (y^2 - x^3 - a*x - b) p == 0) ) [(x,y) | x <- [0 .. p - 1 ], y <- [0 .. p - 1]])
