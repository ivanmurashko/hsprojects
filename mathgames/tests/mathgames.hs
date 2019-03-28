import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.List
import MathGames

testAllBeginNumbers :: Assertion
testAllBeginNumbers = sort (allbeginnum 12345) @?= [1,12,123,1234]


main :: IO ()
main = defaultMainWithOpts
       [testCase "All begin numbers" testAllBeginNumbers
       ]
       mempty   
