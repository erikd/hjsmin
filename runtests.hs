
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
    , testCase "case01_semi1.js"  case01_semi1 
    ]

caseFirst :: Assertion
caseFirst = 1 @=? blah

caseHelloWorld =  
  "Parsed:JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\",JSFunctionBody []]"
  @=? doParse functionDeclaration "function Hello(a) {}"

caseHelloWorld2 =  
  "Parsed:JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\",JSFunctionBody [JSNode JS_BLOCK NoValue [JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"assignmentExpression\") [JSIdentifier \"b\",JSNode JS_value (JSValue \"memberExpression\") [] [] [],JSNode JS_value (JSValue \"=\") [] [] [],JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSNode JS_value (JSDecimal 1) [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] []] [] []] [] []]]"
  @=? doParse functionDeclaration "function Hello(a) {b=1}"
  
caseSimpleAssignment = 
  "Parsed:[JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"assignmentExpression\") [JSIdentifier \"a\",JSNode JS_value (JSValue \"memberExpression\") [] [] [],JSNode JS_value (JSValue \"=\") [] [] [],JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSNode JS_value (JSDecimal 1) [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] []] [] [],JSEmpty]"
  @=? doParse statementList "a=1;"  

case0_f =
  "Parsed:JSNode JS_BLOCK NoValue [JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\",JSFunctionBody [JSNode JS_BLOCK NoValue [JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSIdentifier \"ExprArray\",JSNode JS_value (JSValue \"memberExpression\") [] [] [],JSNode JS_value (JSValue \"arguments\") [JSNode JS_value (JSValue \"argumentList\") [JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSNode JS_value (JSDecimal 1) [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] [],JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSNode JS_value (JSDecimal 1) [] [] [],JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] []] [] []] [] []] [] [],JSEmpty] [] []]]] [] []"
  @=? doParse program "function Hello(a) {ExprArray(1,1);}"
  
case01_semi1 =
  "Parsed:JSNode JS_BLOCK NoValue [JSNode JS_BLOCK NoValue [JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSIdentifier \"zero\",JSNode JS_value (JSValue \"memberExpression.\") [JSIdentifier \"one\"] [] []] [] []] [] [],JSEmpty,JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSIdentifier \"zero\",JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] []] [] [],JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSIdentifier \"one\",JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] [],JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSIdentifier \"two\",JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] [],JSEmpty,JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSIdentifier \"three\",JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] [],JSNode JS_BLOCK NoValue [JSNode JS_BLOCK NoValue [] [] []] [] [],JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSIdentifier \"four\",JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] [],JSEmpty,JSNode JS_value (JSValue \"expression\") [JSNode JS_value (JSValue \"multiplicativeExpression%\") [JSIdentifier \"five\",JSNode JS_value (JSValue \"memberExpression\") [] [] []] [] []] [] []] [] []"

  @=? doParse program (
    "{zero.one;zero}\n"++
    "one\n"++
    "two;three\n"++
    "{{}} four;\n"++
    "// five\n"++
    "five")
  
-- EOF
