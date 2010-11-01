module Text.Jasmine.Pretty
    (       
    renderJS
    ) where

import Text.Jasmine.Parse
import Text.PrettyPrint
import Data.List

-- ---------------------------------------------------------------------


renderJS :: JSNode -> Doc
renderJS (JSEmpty l)             = (renderJS l)
renderJS (JSIdentifier s)        = text s
renderJS (JSDecimal i)           = text $ show i
renderJS (JSOperator s)          = text s
renderJS (JSExpression xs)       = rJS xs
renderJS (JSSourceElements xs)   = rJS (map fixBlock $ fixSourceElements xs)
renderJS (JSSourceElementsTop xs)= rJS (fixTop $ map fixBlock $ fixSourceElements xs)
renderJS (JSElement _s xs)       = rJS xs
renderJS (JSFunction s p xs)     = (text "function") <+> (renderJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
renderJS (JSFunctionBody xs)     = (text "{") <> (rJS xs) <> (text "}")
renderJS (JSFunctionExpression as s) = (text "function") <> (text "(") <> (commaList as) <> (text ")") <> (renderJS s)
renderJS (JSArguments xs)        = (text "(") <> (commaListList xs) <> (text ")")

renderJS (JSBlock x)             = (text "{") <> (renderJS x) <> (text "}")

renderJS (JSIf c (JSLiteral ";"))= (text "if") <> (text "(") <> (renderJS c) <> (text ")") 
renderJS (JSIf c t)              = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t)

renderJS (JSIfElse c t (JSLiteral ";")) = (text "if") <> (text "(") <> (renderJS c) <> (text ")") 
renderJS (JSIfElse c t e)        = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t) 
                                   <> (text "else") <> (spaceOrBlock $ fixBlock e)
renderJS (JSMemberDot xs)        = (text ".") <> (rJS xs)
renderJS (JSMemberSquare x xs)   = (text "[") <> (renderJS x) <> (text "]") <> (rJS xs)
renderJS (JSLiteral l)           = (text l)
renderJS (JSStringLiteral s l)   = empty <> (char s) <> (text l) <> (char s)
renderJS (JSUnary l  )           = text l
renderJS (JSArrayLiteral xs)     = (text "[") <> (rJS xs) <> (text "]")

renderJS (JSBreak [] [])            = (text "break")
renderJS (JSBreak [] xs)            = (text "break") -- <> (rJS xs) -- <> (text ";")
renderJS (JSBreak is xs)            = (text "break") <+> (rJS is) -- <> (rJS xs)

renderJS (JSCallExpression "()" xs) = (rJS xs)
renderJS (JSCallExpression   t  xs) = (char $ head t) <> (rJS xs) <> (if ((length t) > 1) then (char $ last t) else empty)

-- No space between 'case' and string literal. TODO: what about expression in parentheses?
renderJS (JSCase (JSExpression [JSStringLiteral sepa s]) xs) = (text "case") <> (renderJS (JSStringLiteral sepa s)) 
                                                               <> (char ':') <> (renderJS xs)          
renderJS (JSCase e xs)           = (text "case") <+> (renderJS e) <> (char ':') <> (renderJS xs) -- <> (text ";");         

renderJS (JSCatch i [] s)        = (text "catch") <> (char '(') <> (renderJS i) <>  (char ')') <> (renderJS s)
renderJS (JSCatch i c s)         = (text "catch") <> (char '(') <> (renderJS i) <>  
                                   (text " if ") <> (rJS c) <> (char ')') <> (renderJS s)

renderJS (JSContinue is)         = (text "continue") <> (rJS is) -- <> (char ';')
renderJS (JSDefault xs)          = (text "default") <> (char ':') <> (renderJS xs)
renderJS (JSDoWhile s e ms)      = (text "do") <> (renderJS s) <> (text "while") <> (char '(') <> (renderJS e) <> (char ')') -- <> (renderJS ms)
renderJS (JSElementList xs)      = rJS xs
renderJS (JSElision xs)          = (char ',') <> (rJS xs)
--renderJS (JSExpressionBinary o e1 e2) = (rJS e1) <> (text o) <> (rJS e2)
renderJS (JSExpressionBinary o e1 e2) = (text o) <> (rJS e1) <> (rJS e2)
renderJS (JSExpressionParen e)        = (char '(') <> (renderJS e) <> (char ')')
renderJS (JSExpressionPostfix o e)    = (rJS e) <> (text o)
renderJS (JSExpressionTernary c v1 v2) = (rJS c) <> (char '?') <> (rJS v1) <> (char ':') <> (rJS v2)
renderJS (JSFinally b)                 = (text "finally") <> (renderJS b)

renderJS (JSFor e1 e2 e3 s)            = (text "for") <> (char '(') <> (commaList e1) <> (char ';') 
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS $ fixBlock s)
renderJS (JSForIn e1 e2 s)             = (text "for") <> (char '(') <> (rJS e1) <+> (text "in")
                                         <+> (renderJS e2) <> (char ')') <> (renderJS $ fixBlock s)
renderJS (JSForVar e1 e2 e3 s)         = (text "for") <> (char '(') <> (text "var") <+> (commaList e1) <> (char ';') 
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS $ fixBlock s)
renderJS (JSForVarIn e1 e2 s)          = (text "for") <> (char '(') <> (text "var") <+> (renderJS e1) <+> (text "in") 
                                         <+> (renderJS e2) <> (char ')') <> (renderJS $ fixBlock s)
                                         
renderJS (JSHexInteger i)              = (text $ show i) -- TODO: need to tweak this                                         
renderJS (JSLabelled l v)              = (renderJS l) <> (text ":") <> (renderJS v)
renderJS (JSObjectLiteral xs)          = (text "{") <> (commaList xs) <> (text "}")
renderJS (JSPropertyNameandValue n vs) = (renderJS n) <> (text ":") <> (rJS vs)
renderJS (JSRegEx s)                   = (text s)

renderJS (JSReturn [])                 = (text "return")
renderJS (JSReturn [JSLiteral ";"])    = (text "return;")
renderJS (JSReturn xs)                 = (text "return") <> (if (spaceNeeded xs) then (text " ") else (empty)) <> (rJS $ fixSourceElements xs) 

renderJS (JSThrow e)                   = (text "throw") <+> (renderJS e)

renderJS (JSStatementList xs)          = rJS (fixSourceElements $ map fixBlock xs)

renderJS (JSSwitch e xs)               = (text "switch") <> (char '(') <> (renderJS e) <> (char ')') <> 
                                         (char '{') <> (rJS $ fixSemis xs)  <> (char '}')
renderJS (JSTry e xs)                  = (text "try") <> (renderJS e) <> (rJS xs)

renderJS (JSVarDecl i [])              = (renderJS i) 
renderJS (JSVarDecl i xs)              = (renderJS i) <> (text "=") <> (rJS xs)

renderJS (JSVariables kw xs)           = (text kw) <+> (commaList xs)

renderJS (JSWhile e (JSLiteral ";"))   = (text "while") <> (char '(') <> (renderJS e) <> (char ')') -- <> (renderJS s)
renderJS (JSWhile e s)                 = (text "while") <> (char '(') <> (renderJS e) <> (char ')') <> (renderJS s)

renderJS (JSWith e s)                  = (text "with") <> (char '(') <> (renderJS e) <> (char ')') <> (rJS s)
          
-- Helper functions
rJS :: [JSNode] -> Doc
rJS xs = hcat $ map renderJS xs

commaList :: [JSNode] -> Doc
commaList xs = (hcat $ (punctuate comma (toDoc xs)))

commaListList :: [[JSNode]] -> Doc
commaListList xs = (hcat $ punctuate comma $ map rJS xs)

toDoc :: [JSNode] -> [Doc]
toDoc xs = map renderJS xs

spaceOrBlock :: JSNode -> Doc
spaceOrBlock (JSBlock xs) = renderJS (JSBlock xs)
spaceOrBlock x            = (text " ") <> (renderJS x)

-- ---------------------------------------------------------------
-- Utility stuff


fixTop :: [JSNode] -> [JSNode]
fixTop [] = []
fixTop xs = if (last xs == (JSLiteral ";")) then (init xs) else (xs)

-- Fix semicolons in output of sourceelements and statementlist

fixSourceElements :: [JSNode] -> [JSNode]
fixSourceElements xs = fixSemis $ myFix xs
  
myFix :: [JSNode] -> [JSNode]
myFix []      = []
myFix [x]     = [x]
myFix (x:(JSFunction v1 v2 v3):xs)  = x : (JSLiteral "\n") : myFix ((JSFunction v1 v2 v3) : xs)
-- Messy way, but it works, until the 3rd element arrives ..
myFix ((JSExpression x):(JSExpression y):xs) = (JSExpression x):(JSLiteral ";"):myFix ((JSExpression y):xs)
myFix ((JSExpression x):(JSBlock y):xs)      = (JSExpression x):(JSLiteral ";"):myFix ((JSBlock y):xs)
myFix ((JSBlock x)     :(JSBlock y):xs)      = (JSBlock x)     :(JSLiteral ";"):myFix ((JSBlock y):xs)
myFix ((JSBlock x)     :(JSExpression y):xs) = (JSBlock x)     :(JSLiteral ";"):myFix ((JSExpression y):xs)

-- Merge adjacent variable declarations, but only of the same type
myFix ((JSVariables t1 x1s):(JSLiteral l):(JSVariables t2 x2s):xs) 
  | t1 == t2 = myFix ((JSVariables t1 (x1s++x2s)):xs)
  | otherwise = (JSVariables t1 x1s):myFix ((JSLiteral l):(JSVariables t2 x2s):xs)

-- Merge adjacent semi colons
myFix ((JSLiteral ";"):(JSLiteral ";"):xs)  = myFix ((JSLiteral ";"):xs)
myFix ((JSLiteral ";"):(JSLiteral "" ):xs)  = myFix ((JSLiteral ""):xs)

myFix (x:xs)  = x : myFix xs

-- Sort out Semicolons
fixSemis :: [JSNode] -> [JSNode]
fixSemis xs = fixSemis' $ filter (\x -> x /= JSLiteral ";" && x /= JSLiteral "") xs

fixSemis' [] = []
fixSemis' [x] = [x]
fixSemis' ((JSIf c (JSReturn [JSLiteral ";"])):xs)    = (JSIf c (JSReturn [JSLiteral ";"])):(fixSemis' xs)
fixSemis' ((JSIf c (JSContinue [JSLiteral ";"])):xs)    = (JSIf c (JSContinue [JSLiteral ";"])):(fixSemis' xs)
fixSemis' (x:(JSLiteral "\n"):xs) = x:(JSLiteral "\n"):(fixSemis' xs)
fixSemis' ((JSCase e1 ((JSStatementList []))):(JSCase e2 x):xs) = (JSCase e1 ((JSStatementList []))):fixSemis' ((JSCase e2 x):xs)
fixSemis' (x:xs) = x:(JSLiteral ";"):fixSemis' xs

-- Remove extraneous braces around blocks
fixBlock (JSBlock (JSStatementList [x])) = x
fixBlock (JSBlock (JSStatementList xs)) = fixBlock' (JSBlock (JSStatementList (fixSourceElements xs)))
fixBlock x = x

fixBlock' (JSBlock (JSStatementList [x])) = x
fixBlock' (JSBlock (JSStatementList [x,JSLiteral ""])) = x -- TODO: fix parser to not emit this case
fixBlock' x = x

-- A space is needed if this expression starts with an identifier etc, but not if with a '('
spaceNeeded :: [JSNode] -> Bool
spaceNeeded xs = 
  let
    str = show $ rJS xs
  in  
   head str /= '('


-- ---------------------------------------------------------------------
-- Test stuff


-- readJs "x=1;"
_case0 :: JSNode
_case0 = JSSourceElements 
          [
            JSExpression [JSElement "assignmentExpression" [JSIdentifier "x",JSOperator "=",JSDecimal 1]],
            JSEmpty (JSLiteral ";")
          ]

-- doParse statementList "a=1;"  

_case1 :: [JSNode]
_case1 = [JSExpression 
         [JSElement "assignmentExpression" 
          [JSIdentifier "a",JSOperator "=",JSDecimal 1]
         ]
        ,JSEmpty (JSLiteral ";")
        ]
        
_case2 :: JSNode        
_case2 = JSFunctionExpression [] (JSFunctionBody 
                                 [JSSourceElements 
                                  [JSReturn [JSExpression 
                                             [JSLiteral "this",JSMemberDot [JSIdentifier "name"]],
                                             JSLiteral ""]]]
                                )
                                -- ]],JSEmpty (JSLiteral ";")  
  
          
-- doParse expression "opTypeNames={'\\n':\"NEWLINE\",';':\"SEMICOLON\",',':\"COMMA\"}"          
_case4 :: JSNode
_case4 = JSExpression 
          [
            JSElement "assignmentExpression" 
               [
               JSIdentifier "opTypeNames",
               JSOperator "=",
               JSObjectLiteral 
                 [JSPropertyNameandValue (JSStringLiteral '\'' "\\n") [JSStringLiteral '"' "NEWLINE"],
                  JSPropertyNameandValue (JSStringLiteral '\'' ";") [JSStringLiteral '"' "SEMICOLON"],
                  JSPropertyNameandValue (JSStringLiteral '\'' ",") [JSStringLiteral '"' "COMMA"]
                 ]
               ]
          ]
          

-- doParse program "function load(s){if(typeof s!=\"string\")return s;a=1}"
_case5 :: JSNode
_case5 = JSSourceElements 
          [JSFunction (JSIdentifier "load") 
           [JSIdentifier "s"] 
             (JSFunctionBody 
              [
                JSSourceElements 
                  [
                    JSIf 
                      (JSExpression [JSUnary "typeof ",JSIdentifier "s",JSExpressionBinary "!=" [JSStringLiteral '"' "string"] []]) 
                      (JSReturn [JSExpression [JSIdentifier "s"],JSLiteral ";"])
                    ,JSLiteral ";"
                    ,JSExpression [JSElement "assignmentExpression" [JSIdentifier "a",JSOperator "=",JSDecimal 1]]
                    ]
              ]
             )
          ]
          
-- doParse program "{if(typeof s!=\"string\")return s;evaluate(1)}"
_case6 :: JSNode
_case6 = JSSourceElements [] 

--doParse program  "function load(s){if(typeof s!=\"string\")return s;evaluate(1)}"
_case7 :: JSNode
_case7 = JSSourceElements 
        [
          JSFunction (JSIdentifier "load") [JSIdentifier "s"] 
            (JSFunctionBody 
             [JSSourceElements 
              [JSIf 
                 (JSExpression [JSUnary "typeof ",JSIdentifier "s",JSExpressionBinary "!=" [JSStringLiteral '"' "string"] []]) 
                 (JSReturn [JSExpression [JSIdentifier "s"],JSLiteral ";"])
              ,
               JSExpression [JSIdentifier "evaluate",JSArguments [[JSDecimal 1]]]
              ]
             ]
            )
        ]

--doParse program "for(i=0,j=assignOps.length;i<j;i++){}"
_case8 :: JSNode
_case8 = JSSourceElements 
          [
            JSFor 
              [JSExpression 
                 [JSElement "assignmentExpression" 
                   [JSIdentifier "i",JSOperator "=",JSDecimal 0],
                    JSElement "assignmentExpression" 
                     [JSIdentifier "j",JSOperator "=",JSIdentifier "assignOps",JSMemberDot [JSIdentifier "length"]]
                 ]
              ] 
              
              [JSExpression 
                 [JSIdentifier "i",JSExpressionBinary "<" [JSIdentifier "j"] []]
              ] 
              
              [JSExpression [JSExpressionPostfix "++" [JSIdentifier "i"]]] 
              
              (JSLiteral ";")
          ]        
          
_case01_semi1 :: JSNode
_case01_semi1 = JSSourceElements 
                 [
                   JSBlock (JSStatementList 
                              [
                                JSExpression [JSIdentifier "zero",JSMemberDot [JSIdentifier "one"]],
                                JSLiteral ";",
                                JSExpression [JSIdentifier "zero"]
                              ]),
                   JSExpression [JSIdentifier "one"],
                   JSExpression [JSIdentifier "two"],
                   JSLiteral ";",
                   JSExpression [JSIdentifier "three"],
                   JSLiteral ";",
                   JSExpression [JSIdentifier "four"],
                   JSLiteral ";",
                   JSExpression [JSIdentifier "five"]
                 ]          
                 
-- doParse returnStatement "return this.name;"
_case9 :: JSNode
_case9 = JSReturn [JSExpression [JSLiteral "this",JSMemberDot [JSIdentifier "name"]],JSLiteral ";"]                 

_case9a :: [JSNode]
_case9a = [JSExpression [JSLiteral "this",JSMemberDot [JSIdentifier "name"]],JSLiteral ";"]

--parseFile "./test/parsingonly/02_sm.js"
{-

{zero}
one;two
{
 three
 four;five;
  {
  six;{seven;}
  }
}

-}
_case10 :: JSNode
_case10 = JSSourceElements 
            [
              JSBlock (JSStatementList [JSExpression [JSIdentifier "zero"]]),
              JSExpression [JSIdentifier "one"],
              JSLiteral ";",
              JSExpression [JSIdentifier "two"],
              JSBlock (JSStatementList 
                       [JSExpression [JSIdentifier "three"],
                        JSExpression [JSIdentifier "four"],
                        JSLiteral ";",
                        JSExpression [JSIdentifier "five"],
                        JSLiteral ";",
                        JSBlock (JSStatementList 
                                 [JSExpression [JSIdentifier "six"],
                                  JSLiteral ";",
                                  JSBlock (JSStatementList 
                                           [
                                             JSExpression [JSIdentifier "seven"],
                                             JSLiteral ""
                                           ]
                                          )
                                 ]
                                )
                       ]
                      )
            ]
            
--parseFile "./test/parsingonly/05_comments_simple.js"
_case11 = JSSourceElements 
            [
              JSExpression [JSIdentifier "a",JSExpressionBinary "+" [JSDecimal 1] []],
              JSLiteral ";",
              JSLiteral ";"
            ]            
            
--doParse program "var newlines=spaces.match(/\\n/g);var newlines=spaces.match(/\\n/g);"
_case12 = JSSourceElements 
          [
            JSVariables "var" [JSVarDecl (JSIdentifier "newlines") 
                               [JSIdentifier "spaces",JSMemberDot [JSIdentifier "match"],
                                JSArguments [[JSRegEx "/\\n/g"]]]],
            JSLiteral ";",
            JSVariables "var" [JSVarDecl (JSIdentifier "newlines") 
                               [JSIdentifier "spaces",JSMemberDot [JSIdentifier "match"],
                                JSArguments [[JSRegEx "/\\n/g"]]]],
            JSLiteral ";"
          ]            

--doParse program "for(i=0;;){var t=1};for(var i=0,j=1;;){x=1}"
_case13 = JSSourceElements 
          [
            JSFor [JSExpression [JSElement "assignmentExpression" [JSIdentifier "i",JSOperator "=",JSDecimal 0]]] 
            [] 
            [] 
            (JSBlock (
                JSStatementList [JSVariables "var" [JSVarDecl (JSIdentifier "t") [JSDecimal 1]]]
                )
            ),
            JSLiteral ";",
            JSForVar [JSVarDecl (JSIdentifier "i") [JSDecimal 0],JSVarDecl (JSIdentifier "j") [JSDecimal 1]] 
            [] 
            [] 
            (JSBlock (
                JSStatementList [JSExpression [JSElement "assignmentExpression" [JSIdentifier "x",JSOperator "=",JSDecimal 1]]]
                )
            )
          ]          
          
-- doParse program "if (/^[a-z]/.test(t)) {consts += t.toUpperCase();keywords[t] = i;} else {consts += (/^\\W/.test(t) ? opTypeNames[t] : t);}consts += \" = \" + x;"
_case14 = JSSourceElements 
          [
            JSIfElse 
              (JSExpression [JSRegEx "/^[a-z]/",JSMemberDot [JSIdentifier "test"],JSArguments [[JSIdentifier "t"]]]) 
              (JSBlock (JSStatementList 
                        [
                          JSExpression [
                             JSElement "assignmentExpression" 
                               [JSIdentifier "consts",JSOperator "+=",JSIdentifier "t",
                                JSMemberDot [JSIdentifier "toUpperCase"],JSArguments [[]]]
                             ],
                          JSLiteral ";",
                          JSExpression [
                            JSElement "assignmentExpression" [JSIdentifier "keywords",
                                                              JSMemberSquare (JSExpression [JSIdentifier "t"]) [],
                                                              JSOperator "=",JSIdentifier "i"]],
                          JSLiteral ""])) 
              (JSBlock (JSStatementList 
                        [
                          JSExpression 
                            [
                              JSElement "assignmentExpression" 
                                [
                                  JSIdentifier "consts",JSOperator "+=",
                                  JSExpressionParen 
                                    (
                                      JSExpression 
                                        [
                                          JSExpressionTernary 
                                            [
                                              JSRegEx "/^\\W/",
                                              JSMemberDot [JSIdentifier "test"],
                                              JSArguments [[JSIdentifier "t"]]
                                            ] 
                                            [
                                              JSIdentifier "opTypeNames",
                                              JSMemberSquare (JSExpression [JSIdentifier "t"]) 
                                              []
                                            ] 
                                            [JSIdentifier "t"]
                                        ]
                                    )
                                ]
                            ],
                          JSLiteral ""
                        ]
                       )
              ),
            JSExpression [JSElement "assignmentExpression" 
                          [JSIdentifier "consts",JSOperator "+=",JSStringLiteral '"' " = ",
                           JSExpressionBinary "+" [JSIdentifier "x"] []]],
            JSLiteral ";"]          
          
-- doParse program "a+1;{}"
_case15 = JSSourceElements 
            [
              JSExpression [JSIdentifier "a",JSExpressionBinary "+" [JSDecimal 1] []],
              JSLiteral ";",
              JSLiteral ";"
            ]
            
-- doParse program "for (i = 0;;){var t=1;;}\nx=1;"
-- (renderJS _case16) should become "for (i = 0;;)var t=1;x=1"
_case16 = JSSourceElementsTop 
            [
              JSFor [JSExpression [JSElement "assignmentExpression" [JSIdentifier "i",JSOperator "=",JSDecimal 0]]] [] [] 
                (JSBlock 
                 (JSStatementList 
                  [
                    JSVariables "var" [JSVarDecl (JSIdentifier "t") [JSDecimal 1]],
                    JSLiteral ";",
                    JSLiteral ""
                  ]
                 )
                ),
              JSExpression [JSElement "assignmentExpression" [JSIdentifier "x",JSOperator "=",JSDecimal 1]],
              JSLiteral ";"
            ]

-- doParse program "return new global.Boolean(v);"
_case17 = JSSourceElementsTop 
            [
              JSReturn 
                [
                  JSExpression [JSLiteral "new ",JSIdentifier "global",JSMemberDot [JSIdentifier "Boolean"],JSArguments [[JSIdentifier "v"]]],
                  JSLiteral ";"]
                ]
            
--doParse program "if(typeof s!=\"string\")return;while(--n>=0)s+=t;"
_case18 = JSSourceElementsTop 
            [
              JSIf 
                (JSExpression [JSUnary "typeof ",JSIdentifier "s",JSExpressionBinary "!=" [JSStringLiteral '"' "string"] []]) 
                (JSReturn [JSLiteral ";"]),
              JSWhile (JSExpression [JSUnary "--",JSIdentifier "n",JSExpressionBinary ">=" [JSDecimal 0] []]) 
                (JSExpression 
                   [
                     JSElement "assignmentExpression" 
                       [JSIdentifier "s",JSOperator "+=",JSIdentifier "t"]
                   ]
                ),
              JSLiteral ";"
            ]            

-- doParse program "function f(){return n;\nx=1}"
_case19 = JSSourceElementsTop 
            [
              JSFunction (JSIdentifier "f") [] 
                (JSFunctionBody 
                 [
                   JSSourceElements 
                     [
                       JSReturn [JSExpression [JSIdentifier "n"],JSLiteral ";"],
                       JSExpression 
                         [JSElement "assignmentExpression" 
                          [
                            JSIdentifier "x",JSOperator "=",JSDecimal 1
                          ]
                         ]
                     ]
                 ]
                )
            ]
            
--fixSourceElements ([JSReturn [JSExpression [JSIdentifier "n"],JSLiteral ";"],JSExpression [JSElement "assignmentExpression" [JSIdentifier "x",JSOperator "=",JSDecimal 1]]])
_case20 = [
           JSReturn [JSExpression [JSIdentifier "n"],JSLiteral ";"],
           JSExpression 
             [JSElement "assignmentExpression" [JSIdentifier "x",JSOperator "=",JSDecimal 1]]
          ]            
            
--doParse program "if(!u)continue;t=n"
_case21 = JSSourceElementsTop 
            [
              JSIf (JSExpression [JSUnary "!",JSIdentifier "u"]) 
                   (JSContinue [JSLiteral ";"]),
              JSExpression [JSElement "assignmentExpression" [JSIdentifier "t",JSOperator "=",JSIdentifier "n"]]
            ]          
-- EOF

