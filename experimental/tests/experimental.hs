import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Experimental

testCheck :: Assertion
testCheck = check @?= True

testMgu :: Assertion
testMgu = all (\(x,y,z) -> 250*x + y - 2005*z == 0) mgu @?= True


main :: IO ()
main = defaultMainWithOpts
       [testCase "Check (trivial)" testCheck,
        testCase "MGU" testMgu
       ]
       mempty   
