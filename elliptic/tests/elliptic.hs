import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Elliptic

testPoints :: Assertion
testPoints = filter (\p -> p /= PointZero && xpos p == 1) ps @?= [Point 1 2 c, Point 1 17 c]
             where
               c = Curve (-7) 10 19
               ps = points c

-- https://en.wikipedia.org/wiki/Counting_points_on_elliptic_curves
testCurveOrder :: Assertion
testCurveOrder = cord c @?= 9 where
    c = Curve 1 1 5

testPointOrder :: Assertion
testPointOrder = pord p @?= 12 where
    p = Point 13 8 (Curve (-7) 10 19)

               
-- https://habrahabr.ru/post/335906/
testSum :: Assertion
testSum = p1 .+. p2 @?= Point 80 87 c where
    c = Curve 2 3 97
    p1 = Point 3 6 c
    p2 = Point 80 10 c

-- (1,2) + (1,-2 mod 19) = (1,2) + (1, 17) = 0
testSumZero :: Assertion
testSumZero = p1 .+. p2 @?= PointZero where
    c = Curve (-7) 10 19
    p1 = Point 1 2 c
    p2 = Point 1 17 c    

testSumZero1 :: Assertion
testSumZero1 = p1 .+. PointZero @?= p1 where
    c = Curve 2 3 97
    p1 = Point 3 6 c

testSumZero2 :: Assertion
testSumZero2 = PointZero .+. p1 @?= p1 where
    c = Curve 2 3 97
    p1 = Point 3 6 c

-- https://habrahabr.ru/post/335906/
testScalarMul :: Assertion
testScalarMul = map (\n -> n .*. Point 3 6 c) [0..5] @?= res where
    c = Curve 2 3 97
    res = [PointZero, 
           Point 3 6 c, Point 80 10 c, 
           Point 80 87 c, Point 3 91 c,
           PointZero]


main :: IO ()
main = defaultMainWithOpts
       [testCase "Points" testPoints, 
        testCase "Curve Order" testCurveOrder,
        testCase "Point Order" testPointOrder,  
        testCase "Sum" testSum,
        testCase "Sum zero" testSumZero,
        testCase "Sum zero 1" testSumZero1,
        testCase "Sum zero 2" testSumZero2,
        testCase "Scalar multiplication" testScalarMul
       ]
       mempty   
