import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.List
import MathGames

testAllBeginNumbers :: Assertion
testAllBeginNumbers = sort (allbeginnum 12345) @?= [1,12,123,1234]

testFibonachi :: Assertion
testFibonachi = map fib [0,1,2,3,4,5] @?= [0,1,1,2,3,5]


main :: IO ()
main = defaultMainWithOpts
       [testCase "All begin numbers" testAllBeginNumbers,
        testCase "Fibonachi" testFibonachi
       ]
       mempty   
