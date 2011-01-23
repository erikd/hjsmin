module Text.Jasmine.Pretty
    (       
    renderJS
    ) where

import Data.Char
import Data.List
import Data.Monoid
-- import Text.Jasmine.Parse
import Language.JavaScript.Parser
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BS
import qualified Data.ByteString.Lazy as LB

-- ---------------------------------------------------------------------
-- Pretty printer stuff via blaze-builder

(<>) :: BB.Builder -> BB.Builder -> BB.Builder
(<>) a b = mappend a b

(<+>) :: BB.Builder -> BB.Builder -> BB.Builder
(<+>) a b = mconcat [a, (text " "), b]

hcat :: (Monoid a) => [a] -> a
hcat xs = mconcat xs

empty :: BB.Builder
empty = mempty

text :: String -> BB.Builder
text s = BS.fromString s

char :: Char -> BB.Builder
char c = BS.fromChar c

comma :: BB.Builder
comma = BS.fromChar ','

punctuate :: a -> [a] -> [a]
punctuate p xs = intersperse p xs

-- ---------------------------------------------------------------------

renderJS :: JSNode -> BB.Builder
renderJS (NS node _) = rn node

rn :: Node -> BB.Builder
rn (JSEmpty l)             = (renderJS l)
rn (JSIdentifier s)        = text s
rn (JSDecimal i)           = text i
rn (JSOperator s)          = text s
rn (JSExpression xs)       = rJS xs

--rn (JSSourceElements xs)   = rJS (map fixBlock $ fixSourceElements xs)
rn (JSSourceElements xs)   = rJS (fixSourceElements $ map fixBlock xs)

--rn (JSSourceElementsTop xs)= rJS (fixTop $ map fixBlock $ fixSourceElements xs)
rn (JSSourceElementsTop xs)= rJS (fixTop $ fixSourceElements $ map fixBlock xs)


rn (JSFunction s p xs)     = (text "function") <+> (renderJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSFunctionBody xs)     = (text "{") <> (rJS xs) <> (text "}")
rn (JSFunctionExpression [] p xs) = (text "function")             <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSFunctionExpression  s p xs) = (text "function") <+> (rJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSArguments xs)        = (text "(") <> (commaListList $ map fixLiterals xs) <> (text ")")

rn (JSBlock x)             = (text "{") <> (renderJS x) <> (text "}")

rn (JSIf c (NS (JSLiteral ";") _))= (text "if") <> (text "(") <> (renderJS c) <> (text ")") 
rn (JSIf c t)                     = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS $ fixBlock t)

rn (JSIfElse c t (NS (JSLiteral ";") _)) = (text "if") <> (text "(") <> (renderJS c) <> (text ")")  <> (renderJS t) 
                                   <> (text "else") 
rn (JSIfElse c t e)        = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t) 
                                   <> (text "else") <> (spaceOrBlock $ fixBlock e)
rn (JSMemberDot xs y)        = (rJS xs) <> (text ".") <> (renderJS y)
rn (JSMemberSquare xs x)   = (rJS xs) <> (text "[") <> (renderJS x) <> (text "]") 
rn (JSLiteral l)           = (text l)
rn (JSStringLiteral s l)   = empty <> (char s) <> (text l) <> (char s)
rn (JSUnary l  )           = text l
rn (JSArrayLiteral xs)     = (text "[") <> (rJS xs) <> (text "]")

rn (JSBreak [] [])            = (text "break")
rn (JSBreak [] _xs)           = (text "break") -- <> (rJS xs) -- <> (text ";")
rn (JSBreak is _xs)           = (text "break") <+> (rJS is) -- <> (rJS xs)

rn (JSCallExpression "()" xs) = (rJS xs)
rn (JSCallExpression   t  xs) = (char $ head t) <> (rJS xs) <> (if ((length t) > 1) then (char $ last t) else empty)

-- No space between 'case' and string literal. TODO: what about expression in parentheses?
--rn (JSCase (JSExpression [JSStringLiteral sepa s]) xs) = (text "case") <> (renderJS (JSStringLiteral sepa s)) 
rn (JSCase (NS (JSExpression [(NS (JSStringLiteral sepa s) s1)]) _) xs) = (text "case") <> (renderJS (NS (JSStringLiteral sepa s) s1)) 
                                                               <> (char ':') <> (renderJS xs)          
rn (JSCase e xs)           = (text "case") <+> (renderJS e) <> (char ':') <> (renderJS xs) -- <> (text ";");         

rn (JSCatch i [] s)        = (text "catch") <> (char '(') <> (renderJS i) <>  (char ')') <> (renderJS s)
rn (JSCatch i c s)         = (text "catch") <> (char '(') <> (renderJS i) <>  
                                   (text " if ") <> (rJS c) <> (char ')') <> (renderJS s)

rn (JSContinue is)         = (text "continue") <> (rJS is) -- <> (char ';')
rn (JSDefault xs)          = (text "default") <> (char ':') <> (renderJS xs)
rn (JSDoWhile s e _ms)     = (text "do") <> (renderJS s) <> (text "while") <> (char '(') <> (renderJS e) <> (char ')') -- <> (renderJS ms)
--rn (JSElementList xs)      = rJS xs
rn (JSElision xs)          = (char ',') <> (rJS xs)
rn (JSExpressionBinary o e1 e2) = (rJS e1) <> (text o) <> (rJS e2)
--rn (JSExpressionBinary o e1 e2) = (text o) <> (rJS e1) <> (rJS e2)
rn (JSExpressionParen e)        = (char '(') <> (renderJS e) <> (char ')')
rn (JSExpressionPostfix o e)    = (rJS e) <> (text o)
rn (JSExpressionTernary c v1 v2) = (rJS c) <> (char '?') <> (rJS v1) <> (char ':') <> (rJS v2)
rn (JSFinally b)                 = (text "finally") <> (renderJS b)

rn (JSFor e1 e2 e3 s)            = (text "for") <> (char '(') <> (commaList e1) <> (char ';') 
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS $ fixBlock s)
rn (JSForIn e1 e2 s)             = (text "for") <> (char '(') <> (rJS e1) <+> (text "in")
                                         <+> (renderJS e2) <> (char ')') <> (renderJS $ fixBlock s)
rn (JSForVar e1 e2 e3 s)         = (text "for") <> (char '(') <> (text "var") <+> (commaList e1) <> (char ';') 
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS $ fixBlock s)
rn (JSForVarIn e1 e2 s)          = (text "for") <> (char '(') <> (text "var") <+> (renderJS e1) <+> (text "in") 
                                         <+> (renderJS e2) <> (char ')') <> (renderJS $ fixBlock s)
                                         
rn (JSHexInteger i)              = (text $ show i) -- TODO: need to tweak this                                         
rn (JSLabelled l v)              = (renderJS l) <> (text ":") <> (rJS $ fixSourceElements [fixBlock v])
rn (JSObjectLiteral xs)          = (text "{") <> (commaList xs) <> (text "}")
rn (JSPropertyNameandValue n vs) = (renderJS n) <> (text ":") <> (rJS vs)
rn (JSRegEx s)                   = (text s)

rn (JSReturn [])                 = (text "return")
rn (JSReturn [(NS (JSLiteral ";") _)])    = (text "return;")
rn (JSReturn xs)                 = (text "return") <> (if (spaceNeeded xs) then (text " ") else (empty)) <> (rJS $ fixSourceElements xs) 

rn (JSThrow e)                   = (text "throw") <+> (renderJS e)

rn (JSStatementBlock x)          = (text "{") <> (renderJS x) <> (text "}")

rn (JSStatementList xs)          = rJS (fixSourceElements $ map fixBlock xs)

rn (JSSwitch e xs)               = (text "switch") <> (char '(') <> (renderJS e) <> (char ')') <> 
                                         (char '{') <> (rJS $ fixSemis xs)  <> (char '}')
rn (JSTry e xs)                  = (text "try") <> (renderJS e) <> (rJS xs)

rn (JSVarDecl i [])              = (renderJS i) 
rn (JSVarDecl i xs)              = (renderJS i) <> (text "=") <> (rJS xs)

rn (JSVariables kw xs)           = (text kw) <+> (commaList xs)

rn (JSWhile e (NS (JSLiteral ";") _))   = (text "while") <> (char '(') <> (renderJS e) <> (char ')') -- <> (renderJS s)
rn (JSWhile e s)                 = (text "while") <> (char '(') <> (renderJS e) <> (char ')') <> (renderJS s)

rn (JSWith e s)                  = (text "with") <> (char '(') <> (renderJS e) <> (char ')') <> (rJS s)
          
-- Helper functions
rJS :: [JSNode] -> BB.Builder
rJS xs = hcat $ map renderJS xs

commaList :: [JSNode] -> BB.Builder
commaList [] = empty
commaList xs = (hcat $ (punctuate comma (toDoc xs') ++ trail))
  where
    -- (xs', trail) = if (last xs == JSLiteral ",") then (init xs, [comma]) else (xs,[])
    (xs', trail) = if (x' == JSLiteral ",") then (init xs, [comma]) else (xs,[])
    (NS x' _) = last xs
    
commaListList :: [[JSNode]] -> BB.Builder
commaListList xs = (hcat $ punctuate comma $ map rJS xs)

toDoc :: [JSNode] -> [BB.Builder]
toDoc xs = map renderJS xs

spaceOrBlock :: JSNode -> BB.Builder
spaceOrBlock (NS (JSBlock xs) _) = rn (JSBlock xs)
spaceOrBlock (NS (JSStatementBlock xs) _) = rn (JSStatementBlock xs)
spaceOrBlock x            = (text " ") <> (renderJS x)


{-

TODO: Collapse this into JSLiteral ";"

JSStatementBlock (JSStatementList [JSStatementBlock (JSStatementList [])])
-}
-- ---------------------------------------------------------------
-- Utility stuff


fixTop :: [JSNode] -> [JSNode]
fixTop [] = []
fixTop xs = if (n == (JSLiteral ";")) then (init xs) else (xs)
  where
    (NS n _) = last xs

-- Fix semicolons in output of sourceelements and statementlist

fixSourceElements :: [JSNode] -> [JSNode]
fixSourceElements xs = fixSemis $ myFix xs
  
myFix :: [JSNode] -> [JSNode]
myFix []      = []

-- Sort out empty IF statements
myFix ((NS (JSIf c (NS (JSStatementBlock (NS (JSStatementList []) s1)) s2)) s3):xs) = (NS (JSIf c (NS (JSLiteral "") s1)) s2) : myFix (xs)

myFix [x]     = [x]

myFix (x:(NS (JSFunction v1 v2 v3) s1):xs)  = x : (NS (JSLiteral "\n") s1) : myFix ((NS (JSFunction v1 v2 v3) s1) : xs)
-- Messy way, but it works, until the 3rd element arrives ..
-- TODO: JSStatementBlock.  Damn.
myFix ((NS (JSExpression x) s1):(NS (JSExpression y) s2):xs) = (NS (JSExpression x) s1):(NS (JSLiteral ";") s1):myFix ((NS (JSExpression y) s2):xs)
myFix ((NS (JSExpression x) s1):(NS (JSBlock y) s2):xs)      = (NS (JSExpression x) s1):(NS (JSLiteral ";") s1):myFix ((NS (JSBlock y) s2):xs)
myFix ((NS (JSBlock x) s1)     :(NS (JSBlock y) s2):xs)      = (NS (JSBlock x) s1)     :(NS (JSLiteral ";") s1):myFix ((NS (JSBlock y) s2):xs)
myFix ((NS (JSBlock x) s1)     :(NS (JSExpression y) s2):xs) = (NS (JSBlock x) s1)     :(NS (JSLiteral ";") s1):myFix ((NS (JSExpression y) s2):xs)

myFix ((NS (JSExpression x) s1):(NS (JSStatementBlock y) s2):xs)      = 
  (NS (JSExpression x) s1):(NS (JSLiteral ";") s1):myFix ((NS (JSStatementBlock y) s2):xs)
myFix ((NS (JSStatementBlock x) s1)     :(NS (JSStatementBlock y) s2):xs)      = 
  (NS (JSStatementBlock x) s1)     :(NS (JSLiteral ";") s1):myFix ((NS (JSStatementBlock y) s2):xs)
myFix ((NS (JSStatementBlock x) s1)     :(NS (JSExpression y) s2):xs) = 
  (NS (JSStatementBlock x) s1)     :(NS (JSLiteral ";") s1):myFix ((NS (JSExpression y) s2):xs)

-- Merge adjacent variable declarations, but only of the same type
myFix ((NS (JSVariables t1 x1s) s1):(NS (JSLiteral l) s2):(NS (JSVariables t2 x2s) s3):xs) 
  | t1 == t2 = myFix ((NS (JSVariables t1 (x1s++x2s)) s1):xs)
  | otherwise = (NS (JSVariables t1 x1s) s1):myFix ((NS (JSLiteral l) s2):(NS (JSVariables t2 x2s) s3):xs)

myFix ((NS (JSVariables t1 x1s) s1):(NS (JSVariables t2 x2s) s2):xs) 
  | t1 == t2 = myFix ((NS (JSVariables t1 (x1s++x2s)) s1):xs)
  | otherwise = (NS (JSVariables t1 x1s) s1):myFix ((NS (JSVariables t2 x2s) s2):xs)

-- Merge adjacent semi colons
myFix ((NS (JSLiteral ";") s1):(NS (JSLiteral ";") s2):xs)  = myFix ((NS (JSLiteral ";") s1):xs)
myFix ((NS (JSLiteral ";") s1):(NS (JSLiteral "" ) s2):xs)  = myFix ((NS (JSLiteral "") s1):xs)

                       
myFix (x:xs)  = x : myFix xs

-- Merge strings split over lines and using "+"
fixLiterals :: [JSNode] -> [JSNode]
fixLiterals [] = []
-- Old version
fixLiterals ((NS (JSStringLiteral d1 s1) ss1):(NS (JSExpressionBinary "+" [(NS (JSStringLiteral d2 s2) ss2)] r) ss3):xs)
       | d1 == d2 = fixLiterals ((NS (JSStringLiteral d1 (s1++s2)) ss1):(r++xs))
       | otherwise = (NS (JSStringLiteral d1 s1) ss1):fixLiterals ((NS (JSExpressionBinary "+" [(NS (JSStringLiteral d2 s2) ss2)] r) ss3):xs) 

fixLiterals ((NS (JSExpressionBinary "+" [(NS (JSStringLiteral d1 s1) ss2)] [(NS (JSStringLiteral d2 s2) ss3)]) ss4):xs)
       | d1 == d2 = fixLiterals ((NS (JSStringLiteral d1 (s1++s2)) ss2):xs)
       | otherwise = (NS (JSExpressionBinary "+" [(NS (JSStringLiteral d1 s1) ss2)] [(NS (JSStringLiteral d2 s2) ss3)]) ss4):fixLiterals xs           

fixLiterals (x:xs) = x:fixLiterals xs

-- Sort out Semicolons
fixSemis :: [JSNode] -> [JSNode]
--fixSemis xs = fixSemis' $ filter (\x -> x /= JSLiteral ";" && x /= JSLiteral "") xs
fixSemis xs = fixSemis' $ filter (\(NS x _) -> x /= JSLiteral ";" && x /= JSLiteral "") xs

fixSemis' :: [JSNode] -> [JSNode]
fixSemis' [] = []
fixSemis' [(NS (JSContinue [(NS (JSLiteral ";") _)]) s2)] = [(NS (JSContinue []) s2)]
fixSemis' [x] = [x]
fixSemis' ((NS (JSIf c (NS (JSReturn [(NS (JSLiteral ";") s1)]) s2) ) s3):xs)  = 
  (NS (JSIf c (NS (JSReturn [(NS (JSLiteral ";") s1)]) s2)) s3):(fixSemis' xs)
fixSemis' ((NS (JSIf c (NS (JSContinue [(NS (JSLiteral ";") s1)]) s2) ) s3):xs)    = 
  (NS (JSIf c (NS (JSContinue [(NS (JSLiteral ";") s1)]) s2)) s3):(fixSemis' xs)
fixSemis' (x:(NS (JSLiteral "\n") s1):xs) = x:(NS (JSLiteral "\n") s1):(fixSemis' xs) -- TODO: is this needed?
fixSemis' ((NS (JSCase e1 ((NS (JSStatementList []) s1))) s2):(NS (JSCase e2 x) s3):xs) = 
  (NS (JSCase e1 ((NS (JSStatementList []) s1))) s2):fixSemis' ((NS (JSCase e2 x) s3):xs)
fixSemis' (x:xs) = x:(NS (JSLiteral ";") SpanEmpty):fixSemis' xs

-- Remove extraneous braces around blocks
fixBlock :: JSNode -> JSNode

fixBlock (NS (JSBlock          (NS (JSStatementList []) s1) ) _) = (NS (JSLiteral ";") s1)
fixBlock (NS (JSStatementBlock (NS (JSStatementList []) s1) ) _) = (NS (JSLiteral ";") s1)

fixBlock (NS (JSBlock          (NS (JSStatementList [x]) _) ) _) = fixBlock x
fixBlock (NS (JSStatementBlock (NS (JSStatementList [x]) _) ) _) = fixBlock x

fixBlock (NS (JSBlock (NS (JSStatementList xs) s1) ) s2) = 
  fixBlock' (NS (JSBlock (NS (JSStatementList (fixSourceElements xs)) s1)) s2)

fixBlock (NS (JSStatementBlock (NS (JSStatementList xs) s1) ) s2) = 
  fixBlock' (NS (JSStatementBlock (NS (JSStatementList (fixSourceElements xs)) s1)) s2)

fixBlock x = x

fixBlock' :: JSNode -> JSNode
fixBlock' (NS (JSBlock          (NS (JSStatementList [x]) _)) _) = x
fixBlock' (NS (JSStatementBlock (NS (JSStatementList [x]) _)) _) = x
--fixBlock' (JSBlock (JSStatementList [x,JSLiteral ""])) = x -- TODO: fix parser to not emit this case
fixBlock' x = x

-- A space is needed if this expression starts with an identifier etc, but not if with a '('
spaceNeeded :: [JSNode] -> Bool
spaceNeeded xs = 
  let
   -- str = show $ rJS xs
    str = LB.unpack $ BB.toLazyByteString $ rJS xs
  in  
   head str /= (fromIntegral $ ord '(')

-- ---------------------------------------------------------------------
-- Test stuff

r js = map (\x -> chr (fromIntegral x)) $ LB.unpack $ BB.toLazyByteString $ renderJS js

--readJs "{{{}}}"
_case0 :: JSNode
_case0 = NS (JSSourceElementsTop [NS (JSStatementBlock (NS (JSStatementList [NS (JSStatementBlock (NS (JSStatementList [NS (JSStatementBlock (NS (JSStatementList []) (SpanPoint {span_filename = "", span_row = 1, span_column = 3}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 3})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 3}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 2})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 2}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 1})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 1})

_case01 = [NS (JSStatementBlock (NS (JSStatementList [NS (JSStatementBlock (NS (JSStatementList [NS (JSStatementBlock (NS (JSStatementList []) (SpanPoint {span_filename = "", span_row = 1, span_column = 3}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 3})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 3}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 2})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 2}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 1})] 

-- readJs "if(x){}{a=2}"
_case1 = NS (JSSourceElementsTop [NS (JSIf (NS (JSExpression [NS (JSIdentifier "x") (SpanPoint {span_filename = "", span_row = 1, span_column = 4})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 4})) (NS (JSStatementBlock (NS (JSStatementList []) (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 1}),NS (JSStatementBlock (NS (JSStatementList [NS (JSExpression [NS (JSIdentifier "a") (SpanPoint {span_filename = "", span_row = 1, span_column = 9}),NS (JSOperator "=") (SpanPoint {span_filename = "", span_row = 1, span_column = 10}),NS (JSDecimal "2") (SpanPoint {span_filename = "", span_row = 1, span_column = 11})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 9})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 9}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 8})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 1})

_case11 = [NS (JSIf (NS (JSExpression [NS (JSIdentifier "x") (SpanPoint {span_filename = "", span_row = 1, span_column = 4})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 4})) (NS (JSStatementBlock (NS (JSStatementList []) (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 1}),NS (JSStatementBlock (NS (JSStatementList [NS (JSExpression [NS (JSIdentifier "a") (SpanPoint {span_filename = "", span_row = 1, span_column = 9}),NS (JSOperator "=") (SpanPoint {span_filename = "", span_row = 1, span_column = 10}),NS (JSDecimal "2") (SpanPoint {span_filename = "", span_row = 1, span_column = 11})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 9})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 9}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 8})]

_case12 = (NS (JSIf (NS (JSExpression [NS (JSIdentifier "x") (SpanPoint {span_filename = "", span_row = 1, span_column = 4})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 4})) (NS (JSLiteral "") (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))

-- readJs "bob:if(x){}\n{a}"
_case2 = NS (JSSourceElementsTop [NS (JSLabelled (NS (JSIdentifier "bob") (SpanPoint {span_filename = "", span_row = 1, span_column = 1})) (NS (JSIf (NS (JSExpression [NS (JSIdentifier "x") (SpanPoint {span_filename = "", span_row = 1, span_column = 8})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 8})) (NS (JSStatementBlock (NS (JSStatementList []) (SpanPoint {span_filename = "", span_row = 1, span_column = 10}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 10}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 5}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 1}),NS (JSStatementBlock (NS (JSStatementList [NS (JSExpression [NS (JSIdentifier "a") (SpanPoint {span_filename = "", span_row = 2, span_column = 2})]) (SpanPoint {span_filename = "", span_row = 2, span_column = 2})]) (SpanPoint {span_filename = "", span_row = 2, span_column = 2}))) (SpanPoint {span_filename = "", span_row = 2, span_column = 1})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 1})

{-
-- readJs "x=1;"
_case0 :: JSNode
_case0 = JSSourceElements 
          [
            JSExpression [JSIdentifier "x",JSOperator "=",JSDecimal "1"],
            JSEmpty (JSLiteral ";")
          ]

-- doParse statementList "a=1;"  
_case1 :: [JSNode]
_case1 = [JSExpression 
         [JSIdentifier "a",JSOperator "=",JSDecimal "1"]
        ,JSEmpty (JSLiteral ";")
        ]
        
          
-- doParse expression "opTypeNames={'\\n':\"NEWLINE\",';':\"SEMICOLON\",',':\"COMMA\"}"          
_case4 :: JSNode
_case4 = JSExpression 
          [
               JSIdentifier "opTypeNames",
               JSOperator "=",
               JSObjectLiteral 
                 [JSPropertyNameandValue (JSStringLiteral '\'' "\\n") [JSStringLiteral '"' "NEWLINE"],
                  JSPropertyNameandValue (JSStringLiteral '\'' ";")   [JSStringLiteral '"' "SEMICOLON"],
                  JSPropertyNameandValue (JSStringLiteral '\'' ",")   [JSStringLiteral '"' "COMMA"]
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
                      (JSExpression [JSUnary "typeof ",JSIdentifier "s",
                                     JSExpressionBinary "!=" [JSStringLiteral '"' "string"] []]) 
                      (JSReturn [JSExpression [JSIdentifier "s"],JSLiteral ";"])
                    ,JSLiteral ";"
                    ,JSExpression [JSIdentifier "a",JSOperator "=",JSDecimal "1"]
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
               JSExpression [JSIdentifier "evaluate",JSArguments [[JSDecimal "1"]]]
              ]
             ]
            )
        ]

--doParse program "for(i=0,j=assignOps.length;i<j;i++){}"
_case8 :: JSNode
_case8 = undefined
          
                 
-- doParse returnStatement "return this.name;"
_case9 :: JSNode
_case9 = undefined

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
_case11 :: JSNode
_case11 = JSSourceElements 
            [
              JSExpression [JSIdentifier "a",JSExpressionBinary "+" [JSDecimal "1"] []],
              JSLiteral ";",
              JSLiteral ";"
            ]            
            
--doParse program "var newlines=spaces.match(/\\n/g);var newlines=spaces.match(/\\n/g);"
_case12 :: JSNode
_case12 = undefined

--doParse program "for(i=0;;){var t=1};for(var i=0,j=1;;){x=1}"
_case13 :: JSNode
_case13 = JSSourceElements 
          [
            JSFor [JSExpression [JSIdentifier "i",JSOperator "=",JSDecimal "0"]] 
            [] 
            [] 
            (JSBlock (
                JSStatementList [JSVariables "var" [JSVarDecl (JSIdentifier "t") [JSDecimal "1"]]]
                )
            ),
            JSLiteral ";",
            JSForVar [JSVarDecl (JSIdentifier "i") [JSDecimal "0"],JSVarDecl (JSIdentifier "j") [JSDecimal "1"]] 
            [] 
            [] 
            (JSBlock (
                JSStatementList [JSExpression [JSIdentifier "x",JSOperator "=",JSDecimal "1"]]
                )
            )
          ]          
          
-- doParse program "if (/^[a-z]/.test(t)) {consts += t.toUpperCase();keywords[t] = i;} else {consts += (/^\\W/.test(t) ? opTypeNames[t] : t);}consts += \" = \" + x;"
_case14 :: JSNode
_case14 = undefined
          
-- doParse program "a+1;{}"
_case15 :: JSNode
_case15 = JSSourceElements 
            [
              JSExpression [JSIdentifier "a",JSExpressionBinary "+" [JSDecimal "1"] []],
              JSLiteral ";",
              JSLiteral ";"
            ]
            
-- doParse program "for (i = 0;;){var t=1;;}\nx=1;"
-- (renderJS _case16) should become "for (i = 0;;)var t=1;x=1"
_case16 :: JSNode
_case16 = JSSourceElementsTop 
            [
              JSFor [JSExpression [JSIdentifier "i",JSOperator "=",JSDecimal "0"]] [] [] 
                (JSBlock 
                 (JSStatementList 
                  [
                    JSVariables "var" [JSVarDecl (JSIdentifier "t") [JSDecimal "1"]],
                    JSLiteral ";",
                    JSLiteral ""
                  ]
                 )
                ),
              JSExpression [JSIdentifier "x",JSOperator "=",JSDecimal "1"],
              JSLiteral ";"
            ]

-- doParse program "return new global.Boolean(v);"
_case17 :: JSNode
_case17 = undefined
            
--doParse program "if(typeof s!=\"string\")return;while(--n>=0)s+=t;"
_case18 :: JSNode
_case18 = JSSourceElementsTop 
            [
              JSIf 
                (JSExpression [JSUnary "typeof ",JSIdentifier "s",JSExpressionBinary "!=" [JSStringLiteral '"' "string"] []]) 
                (JSReturn [JSLiteral ";"]),
              JSWhile (JSExpression [JSUnary "--",JSIdentifier "n",JSExpressionBinary ">=" [JSDecimal "0"] []]) 
                (JSExpression 
                   [JSIdentifier "s",JSOperator "+=",JSIdentifier "t"]
                ),
              JSLiteral ";"
            ]            

-- doParse program "function f(){return n;\nx=1}"
_case19 :: JSNode
_case19 = JSSourceElementsTop 
            [
              JSFunction (JSIdentifier "f") [] 
                (JSFunctionBody 
                 [
                   JSSourceElements 
                     [
                       JSReturn [JSExpression [JSIdentifier "n"],JSLiteral ";"],
                       JSExpression 
                         [
                            JSIdentifier "x",JSOperator "=",JSDecimal "1"
                         ]
                     ]
                 ]
                )
            ]
            
--fixSourceElements ([JSReturn [JSExpression [JSIdentifier "n"],JSLiteral ";"],JSExpression [JSElement "assignmentExpression" [JSIdentifier "x",JSOperator "=",JSDecimal 1]]])
_case20 :: [JSNode]
_case20 = [
           JSReturn [JSExpression [JSIdentifier "n"],JSLiteral ";"],
           JSExpression 
             [JSIdentifier "x",JSOperator "=",JSDecimal "1"]
          ]            
            
--doParse program "if(!u)continue;t=n"
_case21 :: JSNode
_case21 = JSSourceElementsTop 
            [
              JSIf (JSExpression [JSUnary "!",JSIdentifier "u"]) 
                   (JSContinue [JSLiteral ";"]),
              JSExpression [JSIdentifier "t",JSOperator "=",JSIdentifier "n"]
            ]          
            
--doParse program "if (!v.base){throw new ReferenceError(v.propertyName + \" is not defined\");};x=1"
_case22 :: JSNode
_case22 = undefined

--parseString program "{throw new TypeError(\"Function.prototype.apply called on\"+\n\" uncallable object\");}"
_case23 :: JSNode
_case23 = JSSourceElementsTop 
            [
              JSBlock 
               (
                 JSStatementList 
                   [
                     JSThrow 
                       (JSExpression 
                          [
                            JSLiteral "new ",
                            JSIdentifier "TypeError",
                            JSArguments 
                              [
                                [
                                  JSStringLiteral '"' "Function.prototype.apply called on",
                                  JSExpressionBinary "+" 
                                    [
                                      JSStringLiteral '"' " uncallable object"
                                    ] 
                                    []
                                ]
                              ]
                          ]
                       ),
                     JSLiteral ""
                   ]
               )
            ]

            
--doParse program "x=\"hello \" + \"world\";"
_case24 :: JSNode
_case24 = JSSourceElementsTop 
            [
              JSExpression 
                    [JSIdentifier "x",
                     JSOperator "=",
                     JSStringLiteral '"' "hello ",
                     JSExpressionBinary "+" 
                       [JSStringLiteral '"' "world"] []
                    ]
                ,
              JSLiteral ";"
            ]            
            
--doParse program (U.fromString "try{}catch(e){continue;}")
_case25 :: JSNode
_case25 = JSSourceElementsTop 
            [
              JSTry 
                (JSBlock (JSStatementList [])) 
                [
                  JSCatch 
                    (JSIdentifier "e") 
                    [] 
                    (JSBlock 
                      (JSStatementList 
                        [
                          JSContinue [JSLiteral ";"]
                        ]
                      )
                    )
                ]
            ]

-- readJs "var newlines=spaces.match(/\\n/g);var newlines=spaces.match(/\\n/g);"
_case26 :: JSNode
_case26 = JSSourceElementsTop 
          [
            JSVariables "var" 
              [
                JSVarDecl (JSIdentifier "newlines") 
                  [JSMemberDot [JSIdentifier "spaces"] (JSIdentifier "match"),JSArguments [[JSRegEx "/\\n/g"]]]
              ],
            JSVariables "var" 
              [
                JSVarDecl (JSIdentifier "newlines") [JSMemberDot [JSIdentifier "spaces"] (JSIdentifier "match"),
                                                     JSArguments [[JSRegEx "/\\n/g"]]]
              ]
          ]
          
-- readJs "\"mary\"+\"had\""
_case27 :: JSNode
_case27 = JSSourceElementsTop 
          [
            JSExpression [JSExpressionBinary "+" [JSStringLiteral '"' "mary"] [JSStringLiteral '"' "had"]]
          ]
          
-- readJs "throw new TypeError(\"Function.prototype.apply called on\"+\" uncallable object\")"
_case28 :: JSNode
_case28 = JSSourceElementsTop 
          [
            JSThrow 
              (
                JSExpression 
                  [
                    JSLiteral "new ",
                    JSIdentifier "TypeError",
                    JSArguments 
                      [
                        [
                          JSExpressionBinary "+" 
                            [JSStringLiteral '"' "Function.prototype.apply called on"] 
                            [JSStringLiteral '"' " uncallable object"]
                        ]
                      ]
                  ]
              )
          ]          
-}
-- EOF

