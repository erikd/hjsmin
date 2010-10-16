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
renderJS (JSEmpty)             = text ";"
renderJS (JSIdentifier s)      = text s
renderJS (JSDecimal i)         = text $ show i
renderJS (JSOperator s)        = text s
renderJS (JSExpression xs)     = rJS xs
renderJS (JSSourceElements xs) = rJS xs
renderJS (JSElement s xs)      = rJS xs
renderJS (JSFunction s p xs)   = (text "function") <+> (renderJS s) <> (text "(") <> (rJS p) <> (text ")") <> (renderJS xs)
renderJS (JSFunctionBody xs)   = (text "{") <> (rJS xs) <> (text "}")
renderJS (JSArguments xs)      = (text "(") <> (hcat $ (punctuate comma (toDoc xs))) <> (text ")")
renderJS (JSBlock xs)          = (text "{") <> (rJS xs) <> (text "}")
renderJS (JSIf c t)            = (text "if") <> (renderJS c) <> (renderJS t)
renderJS (JSIfElse c t e)      = (text "if") <> (renderJS c) <> (renderJS t) <> (text "else") <> (renderJS e)
renderJS (JSMemberDot xs)      = (text ".") <> (rJS xs)
renderJS (JSMemberSquare xs)   = (text "[") <> (rJS xs) <> (text "]")
renderJS (JSLiteral l)         = text l
renderJS (JSStringLiteral s l) = (char s) <> (text l) <> (char s)
renderJS (JSUnary l  )         = text l


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

