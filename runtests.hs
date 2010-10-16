
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Jasmine
import Text.Jasmine.Parse hiding (main)
import Text.Jasmine.Pretty

main :: IO ()
main = defaultMain [testSuite,testSuiteMin]

testSuite :: Test
testSuite = testGroup "Text.Jasmine.Parse"
    [ testCase "first" caseFirst
    , testCase "helloWorld"       caseHelloWorld  
    , testCase "helloWorld2"      caseHelloWorld2  
    , testCase "simpleAssignment" caseSimpleAssignment
    , testCase "0_f.js"           case0_f
    , testCase "01_semi1.js"      case01_semi1 
    , testCase "min_100_animals"  case_min_100_animals
    ]

testSuiteMin :: Test
testSuiteMin = testGroup "Text.Jasmine.Pretty"
    [ testCase "helloWorld"       caseMinHelloWorld  
    , testCase "helloWorld2"      caseMinHelloWorld2  
    -- , testCase "simpleAssignment" caseMinSimpleAssignment
    -- , testCase "0_f.js"           caseMin0_f
    -- , testCase "01_semi1.js"      caseMin01_semi1 
    -- , testCase "min_100_animals"  caseMin_min_100_animals
    ]

caseFirst :: Assertion
caseFirst = 1 @=? blah

srcHelloWorld = "function Hello(a) {}"
caseHelloWorld =  
  JSFunction (JSIdentifier "Hello") [JSIdentifier "a"] (JSFunctionBody [])
  @=? doParse functionDeclaration srcHelloWorld
caseMinHelloWorld = 
  srcHelloWorld @=? (show $ minify srcHelloWorld)

srcHelloWorld2 = "function Hello(a) {b=1}" 
caseHelloWorld2 =  
  JSFunction (JSIdentifier "Hello") [JSIdentifier "a"] (JSFunctionBody [JSSourceElements [JSExpression [JSElement "assignmentExpression" [JSIdentifier "b",JSOperator "=",JSDecimal 1]]]])
  @=? doParse functionDeclaration srcHelloWorld2
caseMinHelloWorld2 =  
  srcHelloWorld2 @=? (show $ minify srcHelloWorld2)

srcSimpleAssignment = "a=1;"   
caseSimpleAssignment = 
  [JSExpression [JSElement "assignmentExpression" [JSIdentifier "a",JSOperator "=",JSDecimal 1]],JSEmpty]
  @=? doParse statementList srcSimpleAssignment

src0_f = "function Hello(a) {ExprArray(1,1);}"
case0_f =
  JSSourceElements [JSFunction (JSIdentifier "Hello") [JSIdentifier "a"] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier "ExprArray",JSArguments [JSDecimal 1,JSDecimal 1]],JSEmpty]])]
  @=? doParse program src0_f
  
src01_semi1 = (
    "{zero.one;zero}\n"++
    "one\n"++
    "two;three\n"++
    "{{}} four;\n"++
    "// five\n"++
    "five")  
case01_semi1 =
  JSSourceElements [JSBlock [JSExpression [JSIdentifier "zero",JSNode JS_value (JSValue "memberExpression.") [JSIdentifier "one"] [] []],JSEmpty,JSExpression [JSIdentifier "zero"]],JSExpression [JSIdentifier "one"],JSExpression [JSIdentifier "two"],JSEmpty,JSExpression [JSIdentifier "three"],JSBlock [JSBlock []],JSExpression [JSIdentifier "four"],JSEmpty,JSExpression [JSIdentifier "five"]]
  @=? doParse program src01_semi1
  
src_min_100_animals = "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\"" 
case_min_100_animals =
  JSSourceElements [JSFunction (JSIdentifier "Animal") [JSIdentifier "name"] (JSFunctionBody [JSSourceElements [JSIf (JSExpression [JSUnary "!",JSIdentifier "name"]) (JSNode JS_value (JSValue "throw") [JSExpression [JSNode JS_value (JSValue "memberExpression.new") [] [] [],JSIdentifier "Error",JSArguments [JSStringLiteral '\'' "Must specify an animal name"]]] [] []),JSEmpty,JSExpression [JSElement "assignmentExpression" [JSNode JS_value (JSValue "this") [] [] [],JSNode JS_value (JSValue "memberExpression.") [JSIdentifier "name"] [] [],JSOperator "=",JSIdentifier "name"]]]]),JSEmpty,JSExpression [JSElement "assignmentExpression" [JSIdentifier "Animal",JSNode JS_value (JSValue "memberExpression.") [JSIdentifier "prototype",JSNode JS_value (JSValue "memberExpression.") [JSIdentifier "toString"] [] []] [] [],JSOperator "=",JSFunctionBody [JSSourceElements [JSNode JS_value (JSValue "return") [JSExpression [JSNode JS_value (JSValue "this") [] [] [],JSNode JS_value (JSValue "memberExpression.") [JSIdentifier "name"] [] []]] [] []]]]],JSEmpty,JSExpression [JSElement "assignmentExpression" [JSIdentifier "o",JSOperator "=",JSNode JS_value (JSValue "memberExpression.new") [] [] [],JSIdentifier "Animal",JSArguments [JSStringLiteral '"' "bob"]]],JSEmpty,JSExpression [JSNode JS_value (JSValue "equalityExpression==") [JSIdentifier "o",JSNode JS_value (JSValue "memberExpression.") [JSIdentifier "toString"] [] [],JSArguments [],JSStringLiteral '"' "bob"] [] []]]
  @=? doParse program src_min_100_animals
  
-- EOF
