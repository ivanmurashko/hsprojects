import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import TheSupermarketQueue

testTheSupermarketQueue :: Assertion
testTheSupermarketQueue = map (\(xs,n) -> queueTime xs n) 
                          [([], 1), ([1,2,3,4], 1), 
                           ([2,2,3,3,4,4], 2), ([1,2,3,4,5], 100)] @?= 
                          [0, 10, 9, 5]

main :: IO ()
main = defaultMainWithOpts
       [testCase "The Supermarket Queue" testTheSupermarketQueue
       ]
       mempty   
