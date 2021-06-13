import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import TheSupermarketQueue
import FabergeEasterEggsCrushTest

testTheSupermarketQueue :: Assertion
testTheSupermarketQueue = map (\(xs,n) -> queueTime xs n) 
                          [([], 1), ([1,2,3,4], 1), 
                           ([2,2,3,3,4,4], 2), ([1,2,3,4,5], 100)] @?= 
                          [0, 10, 9, 5]

testFabergeEasterEggsCrushTest :: Assertion
testFabergeEasterEggsCrushTest = map (\(n,m) -> heigth n m) [(0,14), (2,0), (2,14), (7,20), (7,500)] @?=
                                 [0, 0, 105, 137979, 1507386560013475]
                          
main :: IO ()
main = defaultMainWithOpts
       [testCase "The Supermarket Queue" testTheSupermarketQueue,
        testCase "Fabergè Easter Eggs crush test" testFabergeEasterEggsCrushTest
       ]
       mempty   
