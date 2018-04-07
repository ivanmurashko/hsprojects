import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import PrimeNumbers

testGenerator :: Assertion
testGenerator = take 4 numbers @?= [1,2,3,5]


main :: IO ()
main = defaultMainWithOpts
       [testCase "Generator" testGenerator
       ]
       mempty   
