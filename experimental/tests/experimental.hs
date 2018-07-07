import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Experimental

testCheck :: Assertion
testCheck = check @?= True


main :: IO ()
main = defaultMainWithOpts
       [testCase "Check (trivial)" testCheck
       ]
       mempty   
