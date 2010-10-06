
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Jasmine
import Text.Jasmine.Parse

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Text.Jasmine.Parse"
    [ testCase "first" caseFirst
    , testCase "helloWorld" caseHelloWorld  
    ]

caseFirst :: Assertion
caseFirst = 1 @=? blah

caseHelloWorld =  
  "Parsed:JSNode JS_value (JSValue \"function\") [JSNode JS_value (JSValue \"\\\"Hello\\\"\") [] [] [],JSNode JS_value (JSValue \"\\\"a\\\"\") [] [] [],JSNode JS_value (JSValue \"functionbody\") [] [] []] [] []"
  @=? doParse functionDeclaration "function Hello(a) {}"

-- EOF
