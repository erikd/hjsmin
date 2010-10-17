module Text.Jasmine.Pretty
    (       
    renderJS
    ) where

import Text.Jasmine.Parse
import Text.PrettyPrint

-- ---------------------------------------------------------------------


r :: [JSNode] -> Doc
r (xs) = hcat $ map renderJS xs

renderJS :: JSNode -> Doc
renderJS (JSEmpty)               = text ";"
renderJS (JSIdentifier s)        = text s
renderJS (JSDecimal i)           = text $ show i
renderJS (JSOperator s)          = text s
renderJS (JSExpression xs)       = rJS xs
renderJS (JSSourceElements xs)   = rJS xs
renderJS (JSElement s xs)        = rJS xs
renderJS (JSFunction s p xs)     = (text "function") <+> (renderJS s) <> (text "(") <> (rJS p) <> (text ")") <> (renderJS xs)
renderJS (JSFunctionBody xs)     = (text "{") <> (rJS xs) <> (text "}")
renderJS (JSArguments xs)        = (text "(") <> (hcat $ (punctuate comma (toDoc xs))) <> (text ")")
renderJS (JSBlock xs)            = (text "{") <> (rJS xs) <> (text "}")
renderJS (JSIf c t)              = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t)
renderJS (JSIfElse c t e)        = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t) <> (text "else") <> (renderJS e)
renderJS (JSMemberDot xs)        = (text ".") <> (rJS xs)
renderJS (JSMemberSquare xs)     = (text "[") <> (rJS xs) <> (text "]")
renderJS (JSLiteral l)           = text l
renderJS (JSStringLiteral s l)   = (char s) <> (text l) <> (char s)
renderJS (JSUnary l  )           = text l
renderJS (JSArrayLiteral xs)     = (text "[") <> (rJS xs) <> (text "]")
renderJS (JSBreak xs)            = (text "break") <+> (rJS xs) <> (text ";")
renderJS (JSCallExpression t xs) = (char $ head t) <> (rJS xs) <> (if ((length t) > 1) then (char $ last t) else empty)
renderJS (JSCase e xs)           = (text "case") <+> (renderJS e) <> (char ':') <> (rJS xs)          
renderJS (JSCatch i s)           = (text "catch") <> (char '(') <> (renderJS i) <> (char ')') <> (renderJS s)
renderJS (JSContinue is)         = (text "continue") <+> (rJS is) <> (char ';')
renderJS (JSDefault xs)          = (text "default") <> (char ':') <> (rJS xs)
renderJS (JSDoWhile s e)         = (text "do") <+> (renderJS s) <> (text "while") <> (char '(') <> (renderJS e) <> (char ')') <> (char ';')
renderJS (JSElementList xs)      = rJS xs
renderJS (JSElision xs)          = (char ',') <> (rJS xs)
renderJS (JSExpressionBinary o e1 e2) = (rJS e1) <> (text o) <> (rJS e2)
renderJS (JSExpressionParen e)        = (char '(') <> (renderJS e) <> (char ')')
renderJS (JSExpressionPostfix o e)    = (rJS e) <> (text o)
renderJS (JSExpressionTernary c v1 v2) = (rJS c) <> (char '?') <> (rJS v1) <> (char ':') <> (rJS v2)
renderJS (JSFinally b)                 = (text "finally") <+> (renderJS b)
renderJS (JSFor e1 e2 e3 s)            = (text "for") <> (char '(') <> (renderJS e1) <> (char ';') 
                                         <> (renderJS e2) <> (char ';') <> (renderJS e3) <> (char ')') <> (renderJS s)
renderJS (JSForIn e1 e2 s)             = (text "for") <> (char '(') <> (rJS e1) <> (text "in")                                         
                                         <> (renderJS e2) <> (char ')') <> (renderJS s)
renderJS (JSForVar e1 e2 e3 s)         = (text "for") <> (char '(') <> (text "var") <+> (rJS e1) <> (char ';') 
                                         <> (renderJS e2) <> (char ';') <> (renderJS e3) <> (char ')') <> (renderJS s)
renderJS (JSForVarIn e1 e2 s)          = (text "for") <> (char '(') <> (text "var") <+> (renderJS e1) <> (text "in") 
                                         <> (renderJS e2) <> (char ')') <> (renderJS s)
renderJS (JSHexInteger i)              = (text $ show i) -- TODO: need to tweak this                                         
renderJS (JSLabelled l v)              = (renderJS l) <> (text ":") <> (renderJS v)
renderJS (JSObjectLiteral xs)          = (text "{") <> (rJS xs) <> (text "}")
renderJS (JSPropertyNameandValue n vs) = (renderJS n) <> (text ":") <> (rJS vs)
renderJS (JSRegEx s)                   = (text s)
renderJS (JSReturn xs)                 = (text "return") <+> (rJS xs) <> (text ";")
renderJS (JSThrow e)                   = (text "throw") <+> (renderJS e)
renderJS (JSSwitch e xs)               = (text "switch") <> (char '(') <> (renderJS e) <> (char ')') <> (rJS xs) <> (text ";")
renderJS (JSTry e xs)                  = (text "try") <+> (rJS xs)
renderJS (JSVarDecl i xs)              = (renderJS i) <> (rJS xs)
renderJS (JSVariables xs)              = (text "var") <+> (rJS xs)
renderJS (JSWhile e s)                 = (text "while") <> (char '(') <> (renderJS e) <> (char ')') <> (renderJS s)
renderJS (JSWith e s)                  = (text "with") <> (char '(') <> (renderJS e) <> (char ')') <> (renderJS s)
          
-- Helper functions
rJS :: [JSNode] -> Doc
rJS xs = hcat $ map renderJS xs

toDoc :: [JSNode] -> [Doc]
toDoc xs = map renderJS xs


-- ---------------------------------------------------------------------
-- Test stuff


-- readJs "x=1;"
case0 :: JSNode
case0 = JSSourceElements 
          [
            JSExpression [JSElement "assignmentExpression" [JSIdentifier "x",JSOperator "=",JSDecimal 1]],
            JSEmpty
          ]

-- doParse statementList "a=1;"  

case1 :: [JSNode]
case1 = [JSExpression 
         [JSElement "assignmentExpression" 
          [JSIdentifier "a",JSOperator "=",JSDecimal 1]
         ]
        ,JSEmpty
        ]
  
  
  



-- EOF

