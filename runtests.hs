
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
  "Parsed:JSNode JS_value (JSValue \"function\") [JSNode JS_value (JSIdentifier \"Hello\") [] [] [],JSNode JS_value (JSIdentifier \"a\") [] [] [],JSNode JS_value (JSValue \"functionbody\") [JSNode JS_BLOCK NoValue [JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"assignmentExpression\") [JSNode JS_value (JSValue \"assignmentStart\") [JSNode JS_value (JSValue \"memberExpression'\") [JSNode JS_value (JSIdentifier \"b\") [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] [],JSNode JS_value (JSValue \"=\") [] [] []] [] [],JSNode JS_value (JSValue \"memberExpression'\") [JSNode JS_value (JSDecimal 1) [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] []] [] []] [] []] [] []] [] []"
  @=? doParse functionDeclaration "function Hello(a) {b=1}"
  
caseSimpleAssignment = 
  "Parsed:[JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"assignmentExpression\") [JSNode JS_value (JSValue \"assignmentStart\") [JSNode JS_value (JSValue \"memberExpression'\") [JSNode JS_value (JSIdentifier \"a\") [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] [],JSNode JS_value (JSValue \"=\") [] [] []] [] [],JSNode JS_value (JSValue \"memberExpression'\") [JSNode JS_value (JSDecimal 1) [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] []] [] [],JSNode JS_value (JSValue \"empty\") [] [] []]"
  @=? doParse statementList "a=1;"  

case0_f =
  "Parsed:JSNode JS_BLOCK NoValue [JSNode JS_value (JSValue \"function\") [JSNode JS_value (JSIdentifier \"Hello\") [] [] [],JSNode JS_value (JSIdentifier \"a\") [] [] [],JSNode JS_value (JSValue \"functionbody\") [JSNode JS_BLOCK NoValue [JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"callExpression\") [JSNode JS_value (JSValue \"memberExpression'\") [JSNode JS_value (JSIdentifier \"ExprArray\") [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] [],JSNode JS_value (JSValue \"arguments\") [JSNode JS_value (JSValue \"argumentList\") [JSNode JS_value (JSValue \"memberExpression'\") [JSNode JS_value (JSDecimal 1) [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] [],JSNode JS_value (JSValue \"memberExpression'\") [JSNode JS_value (JSDecimal 1) [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] []] [] []] [] []] [] [],JSNode JS_value (JSValue \"empty\") [] [] []] [] []] [] []] [] []] [] []"
  
  @=? doParse program "function Hello(a) {ExprArray(1,1);}"
  --Narrowed it to 'doParse callExpression "ExprArray(1,1)"' failing on the comma
-- EOF
