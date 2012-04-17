module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.Char
import Text.Jasmine
import Language.JavaScript.Parser
import Text.Jasmine.Pretty
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

main :: IO ()
main = defaultMain [testSuite,testSuiteMin,testSuiteFiles,testSuiteFilesUnminified]

testSuite :: Test
testSuite = testGroup "Text.Jasmine.Parse"
    [
      testCase "helloWorld"       caseHelloWorld
    , testCase "helloWorld2"      caseHelloWorld2
    , testCase "simpleAssignment" caseSimpleAssignment
    , testCase "emptyFor"         caseEmptyFor
    , testCase "fullFor"          caseFullFor
    , testCase "forVarFull"       caseForVarFull
    , testCase "ifelse1"          caseIfElse1
    , testCase "ifelse2"          caseIfElse2
    , testCase "0_f.js"           case0_f
    , testCase "01_semi1.js"      case01_semi1
    , testCase "min_100_animals"  case_min_100_animals
    , testCase "mergeStrings"     caseMergeStrings
    , testCase "TrailingCommas"   caseTrailingCommas
    , testCase "GetSet"           caseGetSet
    , testCase "Unicode"          caseUnicode
    , testCase "Issue3"           caseIssue3
    , testCase "Issue4"           caseIssue4
    , testCase "Switch1"          caseSwitch1
    , testCase "If1"              caseIf1
    , testCase "If2"              caseIf2
    , testCase "If3"              caseIf3
    , testCase "BootstrapDropdown" caseBootstrapDropdown
    ]

testSuiteMin :: Test
testSuiteMin = testGroup "Text.Jasmine.Pretty Min"
    [ testCase "helloWorld"       caseMinHelloWorld
    , testCase "helloWorld2"      caseMinHelloWorld2
    , testCase "simpleAssignment" caseMinSimpleAssignment
    , testCase "ifelse1"          caseMinIfElse1
    , testCase "ifelse2"          caseMinIfElse2
    , testCase "0_f.js"           caseMin0_f
    , testCase "01_semi1.js"      caseMin01_semi1
    , testCase "min_100_animals"  caseMin_min_100_animals
    , testCase "minNestedSquare"  caseMinNestedSquare
    , testCase "minMergeStrings"  caseMinMergeStrings
    , testCase "EitherLeft"       caseEitherLeft
    , testCase "EitherRight"      caseEitherRight
    , testCase "TrailingCommas"   caseMinTrailingCommas
    , testCase "GetSet"           caseMinGetSet
    , testCase "Unicode"          caseMinUnicode
    , testCase "MinIssue3"        caseMinIssue3
    , testCase "MinIssue4"        caseMinIssue4
    , testCase "MinSwitch1"       caseMinSwitch1
    , testCase "MinIf1"           caseMinIf1
    , testCase "MinIf2"           caseMinIf2
    , testCase "MinIf3"           caseMinIf3
    , testCase "MinBootstrapDropdown" caseMinBootstrapDropdown
    ]

testSuiteFiles :: Test
testSuiteFiles = testGroup "Text.Jasmine.Pretty files"
  [ testCase "00_f.js"          (testFile "./test/pminified/00_f.js")
  , testCase "01_semi1.js"      (testFile "./test/pminified/01_semi1.js")
  , testCase "02_sm.js"         (testFile "./test/pminified/02_sm.js")
  , testCase "03_sm.js"         (testFile "./test/pminified/03_sm.js")
  , testCase "04_if.js"         (testFile "./test/pminified/04_if.js")
  , testCase "05_comments_simple.js" (testFile "./test/pminified/05_comments_simple.js")
  , testCase "05_regex.js"      (testFile "./test/pminified/05_regex.js")
  , testCase "06_callexpr.js"   (testFile "./test/pminified/06_callexpr.js")
  , testCase "06_newexpr.js"    (testFile "./test/pminified/06_newexpr.js")
  , testCase "06_var.js"        (testFile "./test/pminified/06_var.js")
  , testCase "07_expr.js"       (testFile "./test/pminified/07_expr.js")
  , testCase "10_switch.js"     (testFile "./test/pminified/10_switch.js")
  , testCase "14_labelled_stmts.js" (testFile "./test/pminified/14_labelled_stmts.js")
  , testCase "15_literals.js"   (testFile "./test/pminified/15_literals.js")
  , testCase "16_literals.js"   (testFile "./test/pminified/16_literals.js")
  , testCase "20_statements.js" (testFile "./test/pminified/20_statements.js")
  , testCase "25_trycatch.js"   (testFile "./test/pminified/25_trycatch.js")
  , testCase "40_functions.js"  (testFile "./test/pminified/40_functions.js")
  , testCase "67_bob.js"        (testFile "./test/pminified/67_bob.js")
  , testCase "110_perfect.js"   (testFile "./test/pminified/110_perfect.js")
  , testCase "120_js.js"        (testFile "./test/pminified/120_js.js")
  , testCase "121_jsdefs.js"    (testFile "./test/pminified/121_jsdefs.js")
  , testCase "122_jsexec.js"    (testFile "./test/pminified/122_jsexec.js")
  , testCase "122_jsexec2.js"   (testFile "./test/pminified/122_jsexec2.js")
  , testCase "122_jsexec3.js"   (testFile "./test/pminified/122_jsexec3.js")
  -- , testCase "123_jsparse.js"   (testFile "./test/pminified/123_jsparse.js")
       -- TODO: something strange here, assigning code block to variable?
       -- See http://msdn.microsoft.com/en-us/library/77kz8hy0.aspx, get/set keywords for object accessors

  --, testCase "130_htojs2.js"     (testFile "./test/parsingonly/130_htojs2.js")
  --, testCase ""     (testFile "./test/pminified/")
  ]

testSuiteFilesUnminified :: Test
testSuiteFilesUnminified = testGroup "Text.Jasmine.Pretty filesUnminified"
  [ testCase "00_f.js"          (testFileUnminified "00_f.js")
  , testCase "01_semi1.js"      (testFileUnminified "01_semi1.js")
  , testCase "02_sm.js"         (testFileUnminified "02_sm.js")
  , testCase "03_sm.js"         (testFileUnminified "03_sm.js")
  , testCase "04_if.js"         (testFileUnminified "04_if.js")
  , testCase "05_comments_simple.js" (testFileUnminified "05_comments_simple.js")
  , testCase "05_regex.js"      (testFileUnminified "05_regex.js")
  , testCase "06_callexpr.js"   (testFileUnminified "06_callexpr.js")
  , testCase "06_newexpr.js"    (testFileUnminified "06_newexpr.js")
  , testCase "06_var.js"        (testFileUnminified "06_var.js")
  , testCase "07_expr.js"       (testFileUnminified "07_expr.js")
  , testCase "10_switch.js"     (testFileUnminified "10_switch.js")
  , testCase "14_labelled_stmts.js" (testFileUnminified "14_labelled_stmts.js")
  , testCase "15_literals.js"   (testFileUnminified "15_literals.js")
  , testCase "16_literals.js"   (testFileUnminified "16_literals.js")
  , testCase "20_statements.js" (testFileUnminified "20_statements.js")
  , testCase "25_trycatch.js"   (testFileUnminified "25_trycatch.js")
  , testCase "40_functions.js"  (testFileUnminified "40_functions.js")
  , testCase "67_bob.js"        (testFileUnminified "67_bob.js")
  , testCase "110_perfect.js"   (testFileUnminified "110_perfect.js")
  , testCase "120_js.js"        (testFileUnminified "120_js.js")
  , testCase "121_jsdefs.js"    (testFileUnminified "121_jsdefs.js")
  , testCase "122_jsexec.js"    (testFileUnminified "122_jsexec.js")

  --, testCase "122_jsexec2.js"   (testFileUnminified "122_jsexec2.js")
  ]

srcHelloWorld = "function Hello(a) {}"
caseHelloWorld =
  "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\"] (JSBlock ([])),JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcHelloWorld)
caseMinHelloWorld =
  -- "function Hello(a){}" @=? (minify (U.fromString srcHelloWorld))
  testMinify "function Hello(a){}" srcHelloWorld

srcHelloWorld2 = "function Hello(a) {b=1}"
caseHelloWorld2 =
  "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\"] (JSBlock ([JSExpression [JSIdentifier \"b\",JSOperator JSLiteral \"=\",JSDecimal \"1\"]])),JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcHelloWorld2)
caseMinHelloWorld2 =
  -- "function Hello(a){b=1}" @=? (minify (U.fromString srcHelloWorld2))
  testMinify "function Hello(a){b=1}" srcHelloWorld2

srcSimpleAssignment = "a=1;"
caseSimpleAssignment =
  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"a\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcSimpleAssignment)
caseMinSimpleAssignment =
  testMinify "a=1" srcSimpleAssignment

srcEmptyFor = "for (i = 0;;){}"
caseEmptyFor =
  "Right (JSSourceElementsTop [JSFor [JSExpression [JSIdentifier \"i\",JSOperator JSLiteral \"=\",JSDecimal \"0\"]] [] [] (JSBlock ([])),JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcEmptyFor)
srcFullFor = "for (i = 0;i<10;i++){}"
caseFullFor =
  "Right (JSSourceElementsTop [JSFor [JSExpression [JSIdentifier \"i\",JSOperator JSLiteral \"=\",JSDecimal \"0\"]] [JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"i\"] [JSDecimal \"10\"]]] [JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"i\"]]] (JSBlock ([])),JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcFullFor)

srcForVarFull = "for(var i=0,j=tokens.length;i<j;i++){}"
caseForVarFull =
  "Right (JSSourceElementsTop [JSForVar [JSVarDecl (JSIdentifier \"i\") [JSLiteral \"=\",JSDecimal \"0\"],JSLiteral \",\",JSVarDecl (JSIdentifier \"j\") [JSLiteral \"=\",JSMemberDot [JSIdentifier \"tokens\"] (JSIdentifier \"length\")]] [JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"i\"] [JSIdentifier \"j\"]]] [JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"i\"]]] (JSBlock ([])),JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcForVarFull)

srcIfElse1 = "if(a){b=1}else c=2";
caseIfElse1 =
   "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"a\"]) ([JSBlock ([JSExpression [JSIdentifier \"b\",JSOperator JSLiteral \"=\",JSDecimal \"1\"]])]) ([JSLiteral \"else\",JSExpression [JSIdentifier \"c\",JSOperator JSLiteral \"=\",JSDecimal \"2\"]]),JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcIfElse1)
caseMinIfElse1 =
  testMinify "if(a){b=1}else c=2" srcIfElse1

srcIfElse2 = "if(a){b=1}else {c=2;d=4}";
caseIfElse2 =
  "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"a\"]) ([JSBlock ([JSExpression [JSIdentifier \"b\",JSOperator JSLiteral \"=\",JSDecimal \"1\"]])]) ([JSLiteral \"else\",JSBlock ([JSExpression [JSIdentifier \"c\",JSOperator JSLiteral \"=\",JSDecimal \"2\"],JSLiteral \";\",JSExpression [JSIdentifier \"d\",JSOperator JSLiteral \"=\",JSDecimal \"4\"]])]),JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcIfElse2)
caseMinIfElse2 =
  testMinify "if(a){b=1}else{c=2;d=4}" srcIfElse2

src0_f = "function Hello(a) {ExprArray(1,1);}"
case0_f =
   "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\"] (JSBlock ([JSExpression [JSIdentifier \"ExprArray\",JSArguments [JSDecimal \"1\",JSLiteral \",\",JSDecimal \"1\"]],JSLiteral \";\"])),JSLiteral \"\"])"
  -- @=? (show $ parseString program src0_f)
  @=? (showStrippedMaybe $ parseProgram src0_f)
caseMin0_f =
  testMinify "function Hello(a){ExprArray(1,1)}" src0_f

src01_semi1 = (
    "{zero.one1;zero}\n"++
    "one1\n"++
    "two;three\n"++
    "{{}} four;\n"++
    "// five\n"++
    "five")
case01_semi1 =
  "Right (JSSourceElementsTop [JSBlock ([JSExpression [JSMemberDot [JSIdentifier \"zero\"] (JSIdentifier \"one1\")],JSLiteral \";\",JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"one1\"],JSExpression [JSIdentifier \"two\"],JSLiteral \";\",JSExpression [JSIdentifier \"three\"],JSBlock ([JSBlock ([])]),JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"five\"],JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram src01_semi1)
caseMin01_semi1 =
  testMinify "{zero.one1;zero};one1;two;three;four;five" src01_semi1

src_min_100_animals = "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\""
case_min_100_animals =
  "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Animal\") [JSIdentifier \"name\"] (JSBlock ([JSIf (JSExpression [JSUnary \"!\",JSIdentifier \"name\"]) ([JSThrow (JSExpression [JSLiteral \"new\",JSIdentifier \"Error\",JSArguments [JSStringLiteral '\\'' \"Must specify an animal name\"]]),JSLiteral \";\"]) ([]),JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\"),JSOperator JSLiteral \"=\",JSIdentifier \"name\"]])),JSLiteral \";\",JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"Animal\"] (JSIdentifier \"prototype\")] (JSIdentifier \"toString\"),JSOperator JSLiteral \"=\",JSFunctionExpression [] [] (JSBlock ([JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")]] JSLiteral \"\"]))],JSLiteral \";\",JSExpression [JSIdentifier \"o\",JSOperator JSLiteral \"=\",JSLiteral \"new\",JSIdentifier \"Animal\",JSArguments [JSStringLiteral '\"' \"bob\"]],JSLiteral \";\",JSExpression [JSExpressionBinary \"==\" [JSMemberDot [JSIdentifier \"o\"] (JSIdentifier \"toString\"),JSArguments []] [JSStringLiteral '\"' \"bob\"]],JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram src_min_100_animals)
caseMin_min_100_animals =
  testMinify src_min_100_animals src_min_100_animals

srcMergeStrings = "throw new TypeError(\"Function.prototype.apply called on\"+\" uncallable object\");"
caseMergeStrings =
  "Right (JSSourceElementsTop [JSThrow (JSExpression [JSLiteral \"new\",JSIdentifier \"TypeError\",JSArguments [JSExpressionBinary \"+\" [JSStringLiteral '\"' \"Function.prototype.apply called on\"] [JSStringLiteral '\"' \" uncallable object\"]]]),JSLiteral \";\",JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcMergeStrings)
caseMinMergeStrings =
  testMinify "throw new TypeError(\"Function.prototype.apply called on uncallable object\")" srcMergeStrings

srcNestedSquare = "this.cursor+=match[0].length;"
caseNestedSquare =
  "Right (JSSourceElementsTop [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"cursor\"),JSOperator \"+=\",JSMemberDot [JSMemberSquare [JSIdentifier \"match\"] (JSExpression [JSDecimal \"0\"])] (JSIdentifier \"length\")],JSLiteral \";\"])"
  @=? (showStrippedMaybe $ parseProgram srcNestedSquare)
caseMinNestedSquare =
  testMinify "this.cursor+=match[0].length" srcNestedSquare

caseEitherLeft  =
  -- Left "UnexpectedToken (MulToken {token_span = (AlexPn 2 1 3,'=',\"*SYNTAX*ERROR*\")})"
  -- Left "\"MulToken {token_span = TokenPn 2 1 3, token_comment = [NoComment]}\""
  Left "\"MulToken {token_span = TokenPn 2 1 3, token_comment = [NoComment]}\""
  @=? minifym (LB.fromChunks [(E.encodeUtf8 $ T.pack "a=*SYNTAX*ERROR*")])

caseEitherRight  =
  Right (LB.fromChunks [(E.encodeUtf8 $ T.pack "a=\"no syntax error\"")]) @=? minifym (LB.fromChunks [(E.encodeUtf8 $ T.pack "a=\"no syntax error\";")])

srcTrailingCommas = "x={a:1,};y=[d,e,];"
caseTrailingCommas =
  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"a\") [JSDecimal \"1\"],JSLiteral \",\"]],JSLiteral \";\",JSExpression [JSIdentifier \"y\",JSOperator JSLiteral \"=\",JSArrayLiteral [JSIdentifier \"d\",JSElision JSLiteral \",\",JSIdentifier \"e\",JSLiteral \",\"]],JSLiteral \";\",JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcTrailingCommas)
caseMinTrailingCommas =
  testMinify "x={a:1,};y=[d,e,]" srcTrailingCommas

srcGetSet = "x={get foo() {return 1},set foo(a) {x=a}}"
caseGetSet =
  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSObjectLiteral [JSPropertyAccessor NT (JSLiteral \"get\") (TokenPn 3 1 4) [NoComment] (JSIdentifier \"foo\") [] (JSBlock ([JSReturn [JSExpression [JSDecimal \"1\"]] JSLiteral \"\"])),JSLiteral \",\",JSPropertyAccessor NT (JSLiteral \"set\") (TokenPn 24 1 25) [NoComment] (JSIdentifier \"foo\") [JSIdentifier \"a\"] (JSBlock ([JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSIdentifier \"a\"]]))]],JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcGetSet)
caseMinGetSet =
  testMinify "x={get foo(){return 1},set foo(a){x=a}}" srcGetSet

srcUnicode = "var x = \"שלום\";"
caseUnicode =
  "Right (JSSourceElementsTop [JSVariables JSLiteral \"var\" [JSVarDecl (JSIdentifier \"x\") [JSLiteral \"=\",JSStringLiteral '\"' \"\\1513\\1500\\1493\\1501\"]],JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcUnicode)
caseMinUnicode =
  testMinify "var x=\"שלום\"" srcUnicode

srcIssue3 = "var myLatlng = new google.maps.LatLng(56.8379100, 60.5806664);"
caseIssue3 =
  "Right (JSSourceElementsTop [JSVariables JSLiteral \"var\" [JSVarDecl (JSIdentifier \"myLatlng\") [JSLiteral \"=\",JSLiteral \"new\",JSMemberDot [JSMemberDot [JSIdentifier \"google\"] (JSIdentifier \"maps\")] (JSIdentifier \"LatLng\"),JSArguments [JSDecimal \"56.8379100\",JSLiteral \",\",JSDecimal \"60.5806664\"]]],JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcIssue3)
caseMinIssue3 =
  testMinify "var myLatlng=new google.maps.LatLng(56.8379100,60.5806664)" srcIssue3

srcIssue4 = "/* * geolocation. пытаемся определить свое местоположение * если не получается то используем defaultLocation * @Param {object} map экземпляр карты * @Param {object LatLng} defaultLocation Координаты центра по умолчанию * @Param {function} callbackAfterLocation Фу-ия которая вызывается после * геолокации. Т.к запрос геолокации асинхронен */x"
caseIssue4 =
  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\"],JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcIssue4)
caseMinIssue4 =
  testMinify "x" srcIssue4

srcSwitch1 = "switch(i){case 1:1;case 2:2}"
caseSwitch1 =
   "Right (JSSourceElementsTop [JSSwitch (JSExpression [JSIdentifier \"i\"]) JSBlock ([JSCase (JSExpression [JSDecimal \"1\"]) ([JSExpression [JSDecimal \"1\"],JSLiteral \";\"]),JSCase (JSExpression [JSDecimal \"2\"]) ([JSExpression [JSDecimal \"2\"]])]),JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcSwitch1)
caseMinSwitch1 =
  testMinify "switch(i){case 1:1;case 2:2}" srcSwitch1

srcIf1="if(i>0)consts+=\", \";var t=tokens[i];"
caseIf1 =
  "Right (JSSourceElementsTop [JSIf (JSExpression [JSExpressionBinary \">\" [JSIdentifier \"i\"] [JSDecimal \"0\"]]) ([JSExpression [JSIdentifier \"consts\",JSOperator JSLiteral \"+=\",JSStringLiteral '\"' \", \"],JSLiteral \";\"]) ([]),JSVariables JSLiteral \"var\" [JSVarDecl (JSIdentifier \"t\") [JSLiteral \"=\",JSMemberSquare [JSIdentifier \"tokens\"] (JSExpression [JSIdentifier \"i\"])]],JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcIf1)
caseMinIf1 =
  testMinify "if(i>0)consts+=\", \";var t=tokens[i]" srcIf1

srcIf2 = "if (getValue)\n   execute;\nelse {\n   execute;\n}"
caseIf2 =
  "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"getValue\"]) ([JSExpression [JSIdentifier \"execute\"],JSLiteral \";\"]) ([JSLiteral \"else\",JSBlock ([JSExpression [JSIdentifier \"execute\"],JSLiteral \";\"])]),JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcIf2)
caseMinIf2 =
  testMinify "if(getValue){execute}else execute" srcIf2

srcIf3 = "if(getValue){execute}else execute"
caseIf3 =
  "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"getValue\"]) ([JSExpression [JSIdentifier \"execute\"],JSLiteral \";\"]) ([JSLiteral \"else\",JSBlock ([JSExpression [JSIdentifier \"execute\"],JSLiteral \";\"])]),JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcIf2)
caseMinIf3 =
  testMinify "if(getValue){execute}else execute" srcIf3

srcBootstrapDropdown = "clearMenus()\n!isActive && $parent.toggleClass('open')"
caseBootstrapDropdown =
  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"clearMenus\",JSArguments []],JSExpression [JSExpressionBinary \"&&\" [JSUnary \"!\",JSIdentifier \"isActive\"] [JSMemberDot [JSIdentifier \"$parent\"] (JSIdentifier \"toggleClass\"),JSArguments [JSStringLiteral '\\'' \"open\"]]],JSLiteral \"\"])"
  @=? (showStrippedMaybe $ parseProgram srcBootstrapDropdown)
caseMinBootstrapDropdown =
  -- Note: jsmin preserves the \n, rather than the semi. A matter of taste, it is the same number of chars.
  testMinify "clearMenus();!isActive&&$parent.toggleClass('open')" srcBootstrapDropdown

-- ---------------------------------------------------------------------
-- utilities

--testMinify expected src = (LB.fromChunks [(U.fromString expected)])  @=? (minify (U.fromString src))
testMinify expected src = (LB.fromChunks [(E.encodeUtf8 $ T.pack expected)])  @=? (minify $ LB.fromChunks [(E.encodeUtf8 $ T.pack src)])

testFile :: FilePath -> IO ()
testFile filename =
  do
     x <- readFile (filename)
     let x' = trim x
     -- x' @=? (minify (U.fromString x')  )
     testMinify x' x'


testFileUnminified :: FilePath -> IO ()
testFileUnminified filename =
  do
     x <- readFile ("./test/pminified/" ++ filename)
     y <- readFile ("./test/parsingonly/" ++ filename)
     let x' = trim x
     -- x' @=? (minify (U.fromString y))
     testMinify x' y


trim      :: String -> String
trim      = f . f
   where f = reverse . dropWhile isSpace

-- For language-javascript
parseProgram src = parse src "src"

-- EOF
