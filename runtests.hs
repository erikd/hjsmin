
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.HJsMin

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Text.HJsMin"
    [ testCase "first" caseFirst
    ]

caseFirst :: Assertion
caseFirst = 1 @=? blah

