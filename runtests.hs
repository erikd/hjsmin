
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
    , testCase "01_semi1.js"      case01_semi1 
    , testCase "min_100_animals"  case_min_100_animals
    ]

caseFirst :: Assertion
caseFirst = 1 @=? blah

caseHelloWorld =  
  "Parsed:JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\",JSFunctionBody []]"
  @=? doParse functionDeclaration "function Hello(a) {}"

caseHelloWorld2 =  
  "Parsed:JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\",JSFunctionBody [JSNode JS_BLOCK NoValue [JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"b\",JSOperator \"=\",JSDecimal 1]]] [] []]]"
  @=? doParse functionDeclaration "function Hello(a) {b=1}"
  
caseSimpleAssignment = 
  "Parsed:[JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"a\",JSOperator \"=\",JSDecimal 1]],JSEmpty]"
  @=? doParse statementList "a=1;"  

case0_f =
  "Parsed:JSNode JS_BLOCK NoValue [JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\",JSFunctionBody [JSNode JS_BLOCK NoValue [JSExpression [JSIdentifier \"ExprArray\",JSNode JS_value (JSValue \"arguments\") [JSNode JS_value (JSValue \"argumentList\") [JSDecimal 1,JSDecimal 1] [] []] [] []],JSEmpty] [] []]]] [] []"
  @=? doParse program "function Hello(a) {ExprArray(1,1);}"
  
case01_semi1 =
  "Parsed:JSNode JS_BLOCK NoValue [JSNode JS_BLOCK NoValue [JSExpression [JSIdentifier \"zero\",JSNode JS_value (JSValue \"memberExpression.\") [JSIdentifier \"one\"] [] []],JSEmpty,JSExpression [JSIdentifier \"zero\"]] [] [],JSExpression [JSIdentifier \"one\"],JSExpression [JSIdentifier \"two\"],JSEmpty,JSExpression [JSIdentifier \"three\"],JSNode JS_BLOCK NoValue [JSNode JS_BLOCK NoValue [] [] []] [] [],JSExpression [JSIdentifier \"four\"],JSEmpty,JSExpression [JSIdentifier \"five\"]] [] []"

  @=? doParse program (
    "{zero.one;zero}\n"++
    "one\n"++
    "two;three\n"++
    "{{}} four;\n"++
    "// five\n"++
    "five")
  
case_min_100_animals =
  "Parsed:JSNode JS_BLOCK NoValue [JSFunction (JSIdentifier \"Animal\") [JSIdentifier \"name\",JSFunctionBody [JSNode JS_BLOCK NoValue [JSIf (JSExpression [JSUnary \"!\",JSIdentifier \"name\"]) (JSNode JS_value (JSValue \"throw\") [JSExpression [JSNode JS_value (JSValue \"memberExpression.new\") [] [] [],JSIdentifier \"Error\",JSNode JS_value (JSValue \"arguments\") [JSNode JS_value (JSValue \"argumentList\") [JSStringLiteral '\\'' \"Must specify an animal name\"] [] []] [] []]] [] []),JSEmpty,JSExpression [JSElement \"assignmentExpression\" [JSNode JS_value (JSValue \"this\") [] [] [],JSNode JS_value (JSValue \"memberExpression.\") [JSIdentifier \"name\"] [] [],JSOperator \"=\",JSIdentifier \"name\"]]] [] []]],JSEmpty,JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"Animal\",JSNode JS_value (JSValue \"memberExpression.\") [JSIdentifier \"prototype\",JSNode JS_value (JSValue \"memberExpression.\") [JSIdentifier \"toString\"] [] []] [] [],JSOperator \"=\",JSFunctionBody [JSNode JS_BLOCK NoValue [JSNode JS_value (JSValue \"return\") [JSExpression [JSNode JS_value (JSValue \"this\") [] [] [],JSNode JS_value (JSValue \"memberExpression.\") [JSIdentifier \"name\"] [] []]] [] []] [] []]]],JSEmpty,JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"o\",JSOperator \"=\",JSNode JS_value (JSValue \"memberExpression.new\") [] [] [],JSIdentifier \"Animal\",JSNode JS_value (JSValue \"arguments\") [JSNode JS_value (JSValue \"argumentList\") [JSStringLiteral '\"' \"bob\"] [] []] [] []]],JSEmpty,JSExpression [JSNode JS_value (JSValue \"equalityExpression==\") [JSIdentifier \"o\",JSNode JS_value (JSValue \"memberExpression.\") [JSIdentifier \"toString\"] [] [],JSNode JS_value (JSValue \"arguments\") [] [] [],JSStringLiteral '\"' \"bob\"] [] []]] [] []"
  @=? doParse program 
  "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\""
  
-- EOF
