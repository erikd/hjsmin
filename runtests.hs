
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.Char
import Text.Jasmine
--import Text.Jasmine.Parse -- hiding (main)
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
    ]

testSuiteMin :: Test
testSuiteMin = testGroup "Text.Jasmine.Pretty"
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
  "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\"] (JSFunctionBody [])])"
  @=? (show $ parseProgram srcHelloWorld)
caseMinHelloWorld = 
  -- "function Hello(a){}" @=? (minify (U.fromString srcHelloWorld))
  testMinify "function Hello(a){}" srcHelloWorld
  
srcHelloWorld2 = "function Hello(a) {b=1}" 
caseHelloWorld2 =  
  "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\"] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"b\",JSOperator \"=\",JSDecimal \"1\"]]])])"
  @=? (show $ parseProgram srcHelloWorld2)
caseMinHelloWorld2 =  
  -- "function Hello(a){b=1}" @=? (minify (U.fromString srcHelloWorld2))
  testMinify "function Hello(a){b=1}" srcHelloWorld2

srcSimpleAssignment = "a=1;"   
caseSimpleAssignment = 
  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"a\",JSOperator \"=\",JSDecimal \"1\"],JSLiteral \";\"])"
  @=? (show $ parseProgram srcSimpleAssignment)
caseMinSimpleAssignment =
  testMinify "a=1" srcSimpleAssignment

srcEmptyFor = "for (i = 0;;){}"
caseEmptyFor =
  "Right (JSSourceElementsTop [JSFor [JSExpression [JSIdentifier \"i\",JSOperator \"=\",JSDecimal \"0\"]] [] [] (JSLiteral \";\")])"
  @=? (show $ parseProgram srcEmptyFor)  
srcFullFor = "for (i = 0;i<10;i++){}"
caseFullFor =
  "Right (JSSourceElementsTop [JSFor [JSExpression [JSIdentifier \"i\",JSOperator \"=\",JSDecimal \"0\"]] [JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"i\"] [JSDecimal \"10\"]]] [JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"i\"]]] (JSLiteral \";\")])"
  @=? (show $ parseProgram srcFullFor)
  
srcForVarFull = "for(var i=0,j=tokens.length;i<j;i++){}"
caseForVarFull =
  "Right (JSSourceElementsTop [JSForVar [JSVarDecl (JSIdentifier \"i\") [JSDecimal \"0\"],JSVarDecl (JSIdentifier \"j\") [JSMemberDot [JSIdentifier \"tokens\"] (JSIdentifier \"length\")]] [JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"i\"] [JSIdentifier \"j\"]]] [JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"i\"]]] (JSLiteral \";\")])"
  @=? (show $ parseProgram srcForVarFull)

srcIfElse1 = "if(a){b=1}else c=2";
caseIfElse1 =
  "Right (JSSourceElementsTop [JSIfElse (JSExpression [JSIdentifier \"a\"]) (JSBlock (JSStatementList [JSExpression [JSIdentifier \"b\",JSOperator \"=\",JSDecimal \"1\"]])) (JSExpression [JSIdentifier \"c\",JSOperator \"=\",JSDecimal \"2\"])])"
  -- @=? (show $ parseString program srcIfElse1)
  @=? (show $ parseProgram srcIfElse1)
caseMinIfElse1 =
  testMinify "if(a){b=1}else c=2" srcIfElse1

srcIfElse2 = "if(a){b=1}else {c=2;d=4}";
caseIfElse2 =
  "Right (JSSourceElementsTop [JSIfElse (JSExpression [JSIdentifier \"a\"]) (JSBlock (JSStatementList [JSExpression [JSIdentifier \"b\",JSOperator \"=\",JSDecimal \"1\"]])) (JSBlock (JSStatementList [JSExpression [JSIdentifier \"c\",JSOperator \"=\",JSDecimal \"2\"],JSLiteral \";\",JSExpression [JSIdentifier \"d\",JSOperator \"=\",JSDecimal \"4\"]]))])"
  -- @=? (show $ parseString program srcIfElse2)
  @=? (show $ parseProgram srcIfElse2)
caseMinIfElse2 =
  testMinify "if(a){b=1}else{c=2;d=4}" srcIfElse2

src0_f = "function Hello(a) {ExprArray(1,1);}"
case0_f =
  -- "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\"] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"ExprArray\",JSArguments [[JSDecimal 1],[JSDecimal 1]]],JSLiteral \"\"]])]"
  "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Hello\") [JSIdentifier \"a\"] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier \"ExprArray\",JSArguments [[JSDecimal \"1\"],[JSDecimal \"1\"]]],JSLiteral \";\"]])])"
  -- @=? (show $ parseString program src0_f)
  @=? (show $ parseProgram src0_f)
caseMin0_f =
  testMinify "function Hello(a){ExprArray(1,1)}" src0_f
  
src01_semi1 = (
    "{zero.one;zero}\n"++
    "one\n"++
    "two;three\n"++
    "{{}} four;\n"++
    "// five\n"++
    "five")  
case01_semi1 =
  "Right (JSSourceElementsTop [JSBlock (JSStatementList [JSExpression [JSMemberDot [JSIdentifier \"zero\"] (JSIdentifier \"one\")],JSLiteral \";\",JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"one\"],JSExpression [JSIdentifier \"two\"],JSLiteral \";\",JSExpression [JSIdentifier \"three\"],JSLiteral \";\",JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"five\"]])"
  @=? (show $ parseProgram src01_semi1)
caseMin01_semi1 =
  testMinify "{zero.one;zero};one;two;three;four;five" src01_semi1
  
src_min_100_animals = "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\"" 
case_min_100_animals =
  "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Animal\") [JSIdentifier \"name\"] (JSFunctionBody [JSSourceElements [JSIf (JSExpression [JSUnary \"!\",JSIdentifier \"name\"]) (JSBlock (JSStatementList [JSThrow (JSExpression [JSLiteral \"new \",JSIdentifier \"Error\",JSArguments [[JSStringLiteral '\\'' \"Must specify an animal name\"]]])])),JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\"),JSOperator \"=\",JSIdentifier \"name\"]]]),JSLiteral \";\",JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"Animal\"] (JSIdentifier \"prototype\")] (JSIdentifier \"toString\"),JSOperator \"=\",JSFunctionExpression [] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")],JSLiteral \"\"]]])],JSLiteral \";\",JSExpression [JSIdentifier \"o\",JSOperator \"=\",JSLiteral \"new \",JSIdentifier \"Animal\",JSArguments [[JSStringLiteral '\"' \"bob\"]]],JSLiteral \";\",JSExpression [JSExpressionBinary \"==\" [JSMemberDot [JSIdentifier \"o\"] (JSIdentifier \"toString\"),JSArguments []] [JSStringLiteral '\"' \"bob\"]]])"
  -- "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Animal\") [JSIdentifier \"name\"] (JSFunctionBody [JSSourceElements [JSIf (JSExpression [JSUnary \"!\",JSIdentifier \"name\"]) (JSThrow (JSExpression [JSLiteral \"new \",JSIdentifier \"Error\",JSArguments [[JSStringLiteral '\\'' \"Must specify an animal name\"]]])),JSLiteral \";\",JSExpression [JSElement \"assignmentExpression\" [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\"),JSOperator \"=\",JSIdentifier \"name\"]]]]),JSLiteral \";\",JSExpression [JSElement \"assignmentExpression\" [JSMemberDot [JSMemberDot [JSIdentifier \"Animal\"] (JSIdentifier \"prototype\")] (JSIdentifier \"toString\"),JSOperator \"=\",JSFunctionExpression [] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")],JSLiteral \"\"]]])]],JSLiteral \";\",JSExpression [JSElement \"assignmentExpression\" [JSIdentifier \"o\",JSOperator \"=\",JSLiteral \"new \",JSIdentifier \"Animal\",JSArguments [[JSStringLiteral '\"' \"bob\"]]]],JSLiteral \";\",JSExpression [JSExpressionBinary \"==\" [JSMemberDot [JSIdentifier \"o\"] (JSIdentifier \"toString\"),JSArguments []] [JSStringLiteral '\"' \"bob\"]]])"
  @=? (show $ parseProgram src_min_100_animals)
caseMin_min_100_animals =
  testMinify src_min_100_animals src_min_100_animals
  
srcMergeStrings = "throw new TypeError(\"Function.prototype.apply called on\"+\" uncallable object\");"
caseMergeStrings =  
  "Right (JSSourceElementsTop [JSThrow (JSExpression [JSLiteral \"new \",JSIdentifier \"TypeError\",JSArguments [[JSExpressionBinary \"+\" [JSStringLiteral '\"' \"Function.prototype.apply called on\"] [JSStringLiteral '\"' \" uncallable object\"]]]]),JSLiteral \";\"])"
  @=? (show $ parseProgram srcMergeStrings)
caseMinMergeStrings =  
  testMinify "throw new TypeError(\"Function.prototype.apply called on uncallable object\")" srcMergeStrings
  
srcNestedSquare = "this.cursor+=match[0].length;"
caseNestedSquare =
  "Right (JSSourceElementsTop [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"cursor\"),JSOperator \"+=\",JSMemberDot [JSMemberSquare [JSIdentifier \"match\"] (JSExpression [JSDecimal \"0\"])] (JSIdentifier \"length\")],JSLiteral \";\"])"
  @=? (show $ parseProgram srcNestedSquare)
caseMinNestedSquare =  
  testMinify "this.cursor+=match[0].length" srcNestedSquare
  
caseEitherLeft  =  
  Left "UnexpectedToken (MulToken {token_span = (AlexPn 2 1 3,'=',\"*SYNTAX*ERROR*\")})"
  -- Left "UnexpectedToken (MulToken {token_span = SpanPoint {span_filename = \"src\", span_row = 1, span_column = 3}})"
  @=? minifym (LB.fromChunks [(E.encodeUtf8 $ T.pack "a=*SYNTAX*ERROR*")])
  
caseEitherRight  =  
  Right (LB.fromChunks [(E.encodeUtf8 $ T.pack "a=\"no syntax error\"")]) @=? minifym (LB.fromChunks [(E.encodeUtf8 $ T.pack "a=\"no syntax error\";")])
                  
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
