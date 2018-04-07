import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import PrimeNumbers

testGenerator :: Assertion
testGenerator = take 5 numbers @?= [2,3,5,7,11]


main :: IO ()
main = defaultMainWithOpts
       [testCase "Generator" testGenerator
       ]
       mempty   
