
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Jasmine
import Text.Jasmine.Parse hiding (main)
import Text.Jasmine.Pretty
import Data.Char

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
    , testCase "nestedSquare"     caseNestedSquare
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
  -- , testCase "123_jsparse.js"   (testFile "./test/pminified/123_jsparse.js")      
       -- TODO: something strange here, assigning code block to variable?
       -- See http://msdn.microsoft.com/en-us/library/77kz8hy0.aspx, get/set keywords for object accessors
    
  --, testCase "130_htojs2.js"     (testFile "./test/parsingonly/130_htojs2.js")      
  --, testCase ""     (testFile "./test/pminified/")      
  ]  

testSuiteFilesUnminified :: Test
testSuiteFilesUnminified = testGroup "Text.Jasmine.Pretty files"
  [ testCase "00_f.js"          (testFileUnminified "00_f.js")
  , testCase "01_semi1.js"      (testFileUnminified "01_semi1.js")  
  , testCase "02_sm.js"         (testFileUnminified "02_sm.js")  
  , testCase "03_sm.js"         (testFileUnminified "03_sm.js")  
  , testCase "04_if.js"         (testFileUnminified "04_if.js")  
  , testCase "05_comments_simple.js" (testFileUnminified "05_comments_simple.js")  
  , testCase "05_regex.js"      (testFileUnminified "05_regex.js")  
  {-  
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
  , testCase "122_jsexec2.js"   (testFileUnminified "122_jsexec2.js")      
  -}
  ]  

srcHelloWorld = "function Hello(a) {}"
caseHelloWorld =  
  JSFunction (JSIdentifier "Hello") [JSIdentifier "a"] (JSFunctionBody [])
  @=? doParse functionDeclaration srcHelloWorld
caseMinHelloWorld = 
  "function Hello(a){}" @=? (minify srcHelloWorld)

srcHelloWorld2 = "function Hello(a) {b=1}" 
caseHelloWorld2 =  
  JSFunction (JSIdentifier "Hello") [JSIdentifier "a"] (JSFunctionBody [JSSourceElements [JSExpression [JSElement "assignmentExpression" [JSIdentifier "b",JSOperator "=",JSDecimal 1]]]])
  @=? doParse functionDeclaration srcHelloWorld2
caseMinHelloWorld2 =  
  "function Hello(a){b=1}" @=? (minify srcHelloWorld2)

srcSimpleAssignment = "a=1;"   
caseSimpleAssignment = 
  JSStatementList [JSExpression [JSElement "assignmentExpression" [JSIdentifier "a",JSOperator "=",JSDecimal 1]],JSLiteral ";"]
  @=? doParse statementList srcSimpleAssignment
caseMinSimpleAssignment =
  "a=1" @=? (minify srcSimpleAssignment)

srcEmptyFor = "for (i = 0;;){}"
caseEmptyFor =
  JSFor [JSExpression [JSElement "assignmentExpression" [JSIdentifier "i",JSOperator "=",JSDecimal 0]]] [] [] (JSLiteral ";")
  @=? doParse iterationStatement srcEmptyFor
  
srcFullFor = "for (i = 0;i<10;i++){}"
caseFullFor =
  JSFor [JSExpression [JSElement "assignmentExpression" [JSIdentifier "i",JSOperator "=",JSDecimal 0]]] [JSExpression [JSIdentifier "i",JSExpressionBinary "<" [JSDecimal 10] []]] [JSExpression [JSExpressionPostfix "++" [JSIdentifier "i"]]] (JSLiteral ";")
  @=? doParse iterationStatement srcFullFor

srcForVarFull = "for(var i=0,j=tokens.length;i<j;i++){}"
caseForVarFull =
  JSForVar [JSVarDecl (JSIdentifier "i") [JSDecimal 0],JSVarDecl (JSIdentifier "j") [JSIdentifier "tokens",JSMemberDot [JSIdentifier "length"]]] [JSExpression [JSIdentifier "i",JSExpressionBinary "<" [JSIdentifier "j"] []]] [JSExpression [JSExpressionPostfix "++" [JSIdentifier "i"]]] (JSLiteral ";")
  @=? doParse iterationStatement srcForVarFull

srcIfElse1 = "if(a){b=1}else c=2";
caseIfElse1 =
  JSSourceElementsTop [JSIfElse (JSExpression [JSIdentifier "a"]) (JSBlock (JSStatementList [JSExpression [JSElement "assignmentExpression" [JSIdentifier "b",JSOperator "=",JSDecimal 1]]])) (JSExpression [JSElement "assignmentExpression" [JSIdentifier "c",JSOperator "=",JSDecimal 2]])]
  @=? doParse program srcIfElse1
caseMinIfElse1 =
  "if(a){b=1}else c=2" @=? (minify srcIfElse1)

srcIfElse2 = "if(a){b=1}else {c=2;d=4}";
caseIfElse2 =
  JSSourceElementsTop [JSIfElse (JSExpression [JSIdentifier "a"]) (JSBlock (JSStatementList [JSExpression [JSElement "assignmentExpression" [JSIdentifier "b",JSOperator "=",JSDecimal 1]]])) (JSBlock (JSStatementList [JSExpression [JSElement "assignmentExpression" [JSIdentifier "c",JSOperator "=",JSDecimal 2]],JSLiteral ";",JSExpression [JSElement "assignmentExpression" [JSIdentifier "d",JSOperator "=",JSDecimal 4]]]))]
  @=? doParse program srcIfElse2
caseMinIfElse2 =
  "if(a){b=1}else{c=2;d=4}" @=? (minify srcIfElse2)

src0_f = "function Hello(a) {ExprArray(1,1);}"
case0_f =
  JSSourceElementsTop [JSFunction (JSIdentifier "Hello") [JSIdentifier "a"] (JSFunctionBody [JSSourceElements [JSExpression [JSIdentifier "ExprArray",JSArguments [[JSDecimal 1],[JSDecimal 1]]],JSLiteral ""]])]
  @=? doParse program src0_f
caseMin0_f =
  "function Hello(a){ExprArray(1,1)}" @=? (minify src0_f)
  
src01_semi1 = (
    "{zero.one;zero}\n"++
    "one\n"++
    "two;three\n"++
    "{{}} four;\n"++
    "// five\n"++
    "five")  
case01_semi1 =
  JSSourceElementsTop [JSBlock (JSStatementList [JSExpression [JSIdentifier "zero",JSMemberDot [JSIdentifier "one"]],JSLiteral ";",JSExpression [JSIdentifier "zero"]]),JSExpression [JSIdentifier "one"],JSExpression [JSIdentifier "two"],JSLiteral ";",JSExpression [JSIdentifier "three"],JSLiteral ";",JSExpression [JSIdentifier "four"],JSLiteral ";",JSExpression [JSIdentifier "five"]]
  @=? doParse program src01_semi1
caseMin01_semi1 =
  "{zero.one;zero};one;two;three;four;five" @=? (minify src01_semi1)  
  
src_min_100_animals = "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\"" 
case_min_100_animals =
  JSSourceElementsTop [JSFunction (JSIdentifier "Animal") [JSIdentifier "name"] (JSFunctionBody [JSSourceElements [JSIf (JSExpression [JSUnary "!",JSIdentifier "name"]) (JSThrow (JSExpression [JSLiteral "new ",JSIdentifier "Error",JSArguments [[JSStringLiteral '\'' "Must specify an animal name"]]])),JSLiteral ";",JSExpression [JSElement "assignmentExpression" [JSLiteral "this",JSMemberDot [JSIdentifier "name"],JSOperator "=",JSIdentifier "name"]]]]),JSLiteral ";",JSExpression [JSElement "assignmentExpression" [JSIdentifier "Animal",JSMemberDot [JSIdentifier "prototype",JSMemberDot [JSIdentifier "toString"]],JSOperator "=",JSFunctionExpression [] (JSFunctionBody [JSSourceElements [JSReturn [JSExpression [JSLiteral "this",JSMemberDot [JSIdentifier "name"]],JSLiteral ""]]])]],JSLiteral ";",JSExpression [JSElement "assignmentExpression" [JSIdentifier "o",JSOperator "=",JSLiteral "new ",JSIdentifier "Animal",JSArguments [[JSStringLiteral '"' "bob"]]]],JSLiteral ";",JSExpression [JSIdentifier "o",JSMemberDot [JSIdentifier "toString"],JSArguments [[]],JSExpressionBinary "==" [JSStringLiteral '"' "bob"] []]]
  @=? doParse program src_min_100_animals
caseMin_min_100_animals =
  src_min_100_animals @=? (minify src_min_100_animals)  
  
srcNestedSquare = "this.cursor+=match[0].length;"
caseNestedSquare =
  JSSourceElementsTop [JSExpression [JSElement "assignmentExpression" [JSLiteral "this",JSMemberDot [JSIdentifier "cursor"],JSOperator "+=",JSIdentifier "match",JSMemberSquare (JSExpression [JSDecimal 0]) [JSMemberDot [JSIdentifier "length"]]]],JSLiteral ";"]
  @=? doParse program srcNestedSquare
caseMinNestedSquare =  
  "this.cursor+=match[0].length" @=? (minify srcNestedSquare)
  
-- ---------------------------------------------------------------------
-- utilities

testFile :: FilePath -> IO ()
testFile filename =
  do 
     x <- readFile (filename)
     let x' = trim x
     x' @=? (minify x')  


testFileUnminified :: FilePath -> IO ()
testFileUnminified filename =
  do 
     x <- readFile ("./test/pminified/" ++ filename)
     y <- readFile ("./test/parsingonly/" ++ filename)
     let x' = trim x
     x' @=? (minify y)  




trim      :: String -> String
trim      = f . f
   where f = reverse . dropWhile isSpace

-- EOF
