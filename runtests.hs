
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Jasmine
import Text.Jasmine.Parse hiding (main)

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Text.Jasmine.Parse"
    [ testCase "first" caseFirst
    , testCase "helloWorld"       caseHelloWorld  
    , testCase "helloWorld2"      caseHelloWorld2  
    , testCase "simpleAssignment" caseSimpleAssignment
    , testCase "0_f.js"           case0_f
    ]

caseFirst :: Assertion
caseFirst = 1 @=? blah

caseHelloWorld =  
  "Parsed:JSNode JS_value (JSValue \"function\") [JSNode JS_value (JSIdentifier \"Hello\") [] [] [],JSNode JS_value (JSIdentifier \"a\") [] [] [],JSNode JS_value (JSValue \"functionbody\") [] [] []] [] []"
  @=? doParse functionDeclaration "function Hello(a) {}"

caseHelloWorld2 =  
  "Parsed:JSNode JS_value (JSValue \"function\") [JSNode JS_value (JSIdentifier \"Hello\") [] [] [],JSNode JS_value (JSIdentifier \"a\") [] [] [],JSNode JS_value (JSValue \"functionbody\") [JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"assignmentExpression\") [JSNode JS_value (JSIdentifier \"b\") [] [] [],JSNode JS_value (JSValue \"=\") [] [] [],JSNode JS_value (JSDecimal 1) [] [] []] [] []] [] []] [] []] [] []"
  @=? doParse functionDeclaration "function Hello(a) {b=1}"
  
caseSimpleAssignment = 
  "Parsed:[JSNode JS_BLOCK NoValue [JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"assignmentExpression\") [JSNode JS_value (JSIdentifier \"a\") [] [] [],JSNode JS_value (JSValue \"=\") [] [] [],JSNode JS_value (JSDecimal 1) [] [] []] [] []] [] []] [] []]"
  @=? doParse statementList "a=1;"  

case0_f =
  ""
  @=? doParse program "function Hello(a) {ExprArray(1,1);}"
-- EOF
