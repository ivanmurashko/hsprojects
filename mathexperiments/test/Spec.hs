import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List
import MathGames

-- Fibonachi numbers tests
prop_fib :: Word -> Bool
prop_fib n
    | n == 0 = fib n == 0
    | n == 1 = fib n == 1
    | otherwise = (fib n + fib (n+1)) == fib (n + 2) 

testFib :: TestTree
testFib = testGroup "Fibonachi numbers tests" [
           testCase "Fibonachi base" $ map fib [0,1,2,3,4,5] @?= [0,1,1,2,3,5], 
           testProperty "Base property" prop_fib
          ]


-- Lucas numbers
prop_luk :: Word -> Bool
prop_luk n
    | n == 0 = luk n == 2
    | n == 1 = luk n == 1
    | otherwise = (luk n + luk (n+1)) == luk (n + 2) 

testLuk :: TestTree
testLuk = testGroup "Luckas numbers tests" [
           testProperty "Base property" prop_luk
          ]

-- Misc tests
testMisc :: TestTree
testMisc = testGroup "Misc tests" [
            testCase "Factorial tests" $
                     map fac [0,1,2,3,4,5] @?= [1,1,2,6,24,120], 
            testCase "All begin numbers" $ 
                     sort (allbeginnum 12345) @?= [1,12,123,1234]
           ]


allTests :: TestTree
allTests = testGroup "Math experiments tests" [
            testFib, testLuk, testMisc
           ]

main :: IO ()
main = defaultMain allTests
