module Text.Jasmine.Pretty
    (       
    renderJS
    ) where

import Text.Jasmine.Parse
import Text.PrettyPrint

-- ---------------------------------------------------------------------



renderJS :: JSNode -> Doc
renderJS (JSEmpty l)             = (renderJS l)
renderJS (JSIdentifier s)        = text s
renderJS (JSDecimal i)           = text $ show i
renderJS (JSOperator s)          = text s
renderJS (JSExpression xs)       = rJS xs
renderJS (JSSourceElements xs)   = rJS (fixSourceElements xs)
renderJS (JSElement _s xs)       = rJS xs
renderJS (JSFunction s p xs)     = (text "function") <+> (renderJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
renderJS (JSFunctionBody xs)     = (text "{") <> (rJS xs) <> (text "}")
renderJS (JSFunctionExpression as s) = (text "function") <> (text "(") <> (rJS as) <> (text ")") <> (renderJS s)
renderJS (JSArguments xs)        = (text "(") <> (commaListList xs) <> (text ")")
renderJS (JSBlock xs)            = (text "{") <> (rJS xs) <> (text "}")
renderJS (JSIf c t)              = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t)
renderJS (JSIfElse c t e)        = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t) <> (text "else") <> (spaceOrBlock e)
renderJS (JSMemberDot xs)        = (text ".") <> (rJS xs)
renderJS (JSMemberSquare x xs)   = (text "[") <> (renderJS x) <> (text "]") <> (rJS xs)
renderJS (JSLiteral l)           = (text l)
renderJS (JSStringLiteral s l)   = empty <> (char s) <> (text l) <> (char s)
renderJS (JSUnary l  )           = text l
renderJS (JSArrayLiteral xs)     = (text "[") <> (rJS xs) <> (text "]")

renderJS (JSBreak [])            = (text "break")
renderJS (JSBreak xs)            = (text "break") <> (rJS xs) -- <> (text ";")

renderJS (JSCallExpression "()" xs) = (rJS xs)
renderJS (JSCallExpression   t  xs) = (char $ head t) <> (rJS xs) <> (if ((length t) > 1) then (char $ last t) else empty)

-- No space between 'case' and string literal. TODO: what about expression in parentheses?
renderJS (JSCase (JSExpression [JSStringLiteral sepa s]) xs) = (text "case") <> (renderJS (JSStringLiteral sepa s)) <> (char ':') <> (rJS xs)          
renderJS (JSCase e xs)           = (text "case") <+> (renderJS e) <> (char ':') <> (rJS xs)          

renderJS (JSCatch i [] s)        = (text "catch") <> (char '(') <> (renderJS i) <>  (char ')') <> (renderJS s)
renderJS (JSCatch i c s)         = (text "catch") <> (char '(') <> (renderJS i) <>  
                                   (text " if ") <> (rJS c) <> (char ')') <> (renderJS s)

renderJS (JSContinue is)         = (text "continue") <> (rJS is) -- <> (char ';')
renderJS (JSDefault xs)          = (text "default") <> (char ':') <> (rJS xs)
renderJS (JSDoWhile s e ms)         = (text "do") <> (renderJS s) <> (text "while") <> (char '(') <> (renderJS e) <> (char ')') <> (renderJS ms)
renderJS (JSElementList xs)      = rJS xs
renderJS (JSElision xs)          = (char ',') <> (rJS xs)
--renderJS (JSExpressionBinary o e1 e2) = (rJS e1) <> (text o) <> (rJS e2)
renderJS (JSExpressionBinary o e1 e2) = (text o) <> (rJS e1) <> (rJS e2)
renderJS (JSExpressionParen e)        = (char '(') <> (renderJS e) <> (char ')')
renderJS (JSExpressionPostfix o e)    = (rJS e) <> (text o)
renderJS (JSExpressionTernary c v1 v2) = (rJS c) <> (char '?') <> (rJS v1) <> (char ':') <> (rJS v2)
renderJS (JSFinally b)                 = (text "finally") <> (renderJS b)
renderJS (JSFor e1 e2 e3 s)            = (text "for") <> (char '(') <> (commaList e1) <> (char ';') 
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS s)
renderJS (JSForIn e1 e2 s)             = (text "for") <> (char '(') <> (rJS e1) <> (text "in")                                         
                                         <> (renderJS e2) <> (char ')') <> (renderJS s)
renderJS (JSForVar e1 e2 e3 s)         = (text "for") <> (char '(') <> (text "var") <+> (commaList e1) <> (char ';') 
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS s)
renderJS (JSForVarIn e1 e2 s)          = (text "for") <> (char '(') <> (text "var") <+> (renderJS e1) <> (text "in") 
                                         <> (renderJS e2) <> (char ')') <> (renderJS s)
renderJS (JSHexInteger i)              = (text $ show i) -- TODO: need to tweak this                                         
renderJS (JSLabelled l v)              = (renderJS l) <> (text ":") <> (renderJS v)
renderJS (JSObjectLiteral xs)          = (text "{") <> (commaList xs) <> (text "}")
renderJS (JSPropertyNameandValue n vs) = (renderJS n) <> (text ":") <> (rJS vs)
renderJS (JSRegEx s)                   = (text s)
renderJS (JSReturn xs)                 = (text "return") <+> (rJS xs) -- <> (text ";") no longer required, handled by autosemi parsing
renderJS (JSThrow e)                   = (text "throw") <+> (renderJS e)
renderJS (JSSwitch e xs)               = (text "switch") <> (char '(') <> (renderJS e) <> (char ')') <> 
                                         (char '{') <> (rJS xs)  <> (char '}')
renderJS (JSTry e xs)                  = (text "try") <> (renderJS e) <> (rJS xs)

renderJS (JSVarDecl i [])              = (renderJS i) 
renderJS (JSVarDecl i xs)              = (renderJS i) <> (text "=") <> (rJS xs)

renderJS (JSVariables kw xs)           = (text kw) <+> (commaList xs)
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

fixSourceElements :: [JSNode] -> [JSNode]
fixSourceElements xs = myFix xs
  
myFix :: [JSNode] -> [JSNode]
myFix []      = []
myFix [x]     = [x]
myFix (x:(JSFunction v1 v2 v3):xs)  = x : (JSLiteral "\n") : myFix ((JSFunction v1 v2 v3) : xs)
myFix (x:xs)  = x : myFix xs

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
  
_case3 :: JSNode  
_case3 = JSSourceElements 
          [JSSwitch 
           (JSExpression [JSUnary "typeof ",JSIdentifier "v"]) 
           [JSCase 
            (JSExpression [JSStringLiteral '"' "boolean"]) 
            --(JSStringLiteral '"' "boolean") 
            [JSBreak [JSLiteral ""]]
           ]
          ]
          
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
_case6 = JSSourceElements 
          [JSBlock 
           [JSIf 
              (JSExpression 
                [JSUnary "typeof ",JSIdentifier "s",JSExpressionBinary "!=" [JSStringLiteral '"' "string"] []]) 
              (JSReturn 
                [JSExpression [JSIdentifier "s"],JSLiteral ";"]),
            JSExpression [JSIdentifier "evaluate",JSArguments [[JSDecimal 1]]
                         ]
           ]
          ]          

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
-- EOF

