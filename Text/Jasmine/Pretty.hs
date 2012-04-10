module Text.Jasmine.Pretty
    (
    renderJS
    ) where

import Data.Char
import Data.List
import Data.Monoid (Monoid, mappend, mempty, mconcat)
-- import Text.Jasmine.Parse
import Language.JavaScript.Parser (JSNode(..),Node(..),tokenPosnEmpty)
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
renderJS (NN node    ) = rn node
renderJS (NT node _ _) = rn node

rn :: Node -> BB.Builder
--rn (JSEmpty l)             = (renderJS l)

-- Terminals
rn (JSIdentifier s)        = text s
rn (JSDecimal i)           = text i
rn (JSLiteral l)           = (text l)
rn (JSHexInteger i)        = (text $ show i) -- TODO: need to tweak this
rn (JSStringLiteral s l)   = empty <> (char s) <> (text l) <> (char s)
rn (JSRegEx s)             = (text s)

-- Non Terminals
rn (JSOperator x)          = renderJS x
rn (JSExpression xs)       = rJS xs


--rn (JSSourceElements xs)   = rJS (map fixBlock $ fixSourceElements xs)
rn (JSSourceElements xs)   = rJS (fixSourceElements $ map fixBlock xs)

--rn (JSSourceElementsTop xs)= rJS (fixTop $ map fixBlock $ fixSourceElements xs)
rn (JSSourceElementsTop xs)= rJS (fixTop $ fixSourceElements $ map fixBlock xs)


--rn (JSFunction s p xs)     = (text "function") <+> (renderJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSFunction _f s _lb p _rb _lb2 xs _rb2) = (text "function") <+> (renderJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)

rn (JSFunctionBody xs)     = (text "{") <> (rJS xs) <> (text "}")

--rn (JSFunctionExpression [] p xs) = (text "function")             <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
--rn (JSFunctionExpression  s p xs) = (text "function") <+> (rJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSFunctionExpression _f [] _lb p _rb _lb2 xs _rb2) = (text "function")             <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)
rn (JSFunctionExpression _f s _lb p _rb _lb2 xs _rb2)  = (text "function") <+> (rJS s) <> (text "(") <> (commaList p) <> (text ")") <> (renderJS xs)

--rn (JSArguments xs ) = (text "(") <> (commaListList $ map fixLiterals xs) <> (text ")")
rn (JSArguments _lb xs _rb) = (text "(") <> (rJS $ fixLiterals xs) <> (text ")")

--rn (JSBlock x)             = (text "{") <> (renderJS x) <> (text "}")
rn (JSBlock _lb x _rb)       = (text "{") <> (renderJS x) <> (text "}")

{-
rn (JSIf c (NT (JSLiteral ";") _ _)) = (text "if") <> (text "(") <> (renderJS c) <> (text ")")
rn (JSIf c t)                        = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS $ fixBlock t)

rn (JSIf c t (NT (JSLiteral ";") _ _)) = (text "if") <> (text "(") <> (renderJS c) <> (text ")")  <> (renderJS t)
                                   <> (text "else")
rn (JSIf c t e)        = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t)
                                   <> (text "else") <> (spaceOrBlock $ fixBlock e)
-}
rn (JSIf _i _lb c _rb (NT (JSLiteral ";") _ _) []) = (text "if") <> (text "(") <> (renderJS c) <> (text ")")
rn (JSIf _i _lb c _rb t                        []) = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS $ fixBlock t)

rn (JSIf _i _lb c _rb t [_e,(NT (JSLiteral ";") _ _)]) = (text "if") <> (text "(") <> (renderJS c) <> (text ")")  <> (renderJS t)
                                                      <> (text "else")
rn (JSIf _i _lb c _rb  t [_e,e])                         = (text "if") <> (text "(") <> (renderJS c) <> (text ")") <> (renderJS t)
                                                      <> (text "else") <> (spaceOrBlock $ fixBlock e)



rn (JSMemberDot xs _d y)          = (rJS xs) <> (text ".") <> (renderJS y)
rn (JSMemberSquare xs _lb x _rb)  = (rJS xs) <> (text "[") <> (renderJS x) <> (text "]")
rn (JSUnary l _s )                = text l
rn (JSArrayLiteral _lb xs _rb)    = (text "[") <> (rJS xs) <> (text "]")

--rn (JSBreak _b [] _as)            = (text "break")
rn (JSBreak _b [] _as)           = (text "break") -- <> (rJS xs) -- <> (text ";")
rn (JSBreak _b is _as)           = (text "break") <+> (rJS is) -- <> (rJS xs)

rn (JSCallExpression "()" _os xs _cs) = (rJS xs)
rn (JSCallExpression   t  _os xs _cs) = (char $ head t) <> (rJS xs) <> (if ((length t) > 1) then (char $ last t) else empty)


-- No space between 'case' and string literal. TODO: what about expression in parentheses?
--rn (JSCase (JSExpression [JSStringLiteral sepa s]) xs) = (text "case") <> (renderJS (JSStringLiteral sepa s))
rn (JSCase _ca (NN (JSExpression [(NT (JSStringLiteral sepa s) s1 c1)])) _c xs) = (text "case") <> (renderJS (NT (JSStringLiteral sepa s) s1 c1))
                                                               <> (char ':') <> (renderJS xs)
rn (JSCase _ca e _c xs)           = (text "case") <+> (renderJS e) <> (char ':') <> (renderJS xs) -- <> (text ";");


rn (JSCatch _c _lb i [] _rb s)  = (text "catch") <> (char '(') <> (renderJS i) <>  (char ')') <> (renderJS s)
rn (JSCatch _c _lb i  c _rb s)  = (text "catch") <> (char '(') <> (renderJS i) <>
                                  (text " if ") <> (rJS c) <> (char ')') <> (renderJS s)

rn (JSContinue _c is _as)  = (text "continue") <> (rJS is) -- <> (char ';')
rn (JSDefault _d _c xs)    = (text "default") <> (char ':') <> (renderJS xs)
rn (JSDoWhile _d s _w _lb e _rb _ms)     = (text "do") <> (renderJS s) <> (text "while") <> (char '(') <> (renderJS e) <> (char ')') -- <> (renderJS ms)


--rn (JSElementList xs)      = rJS xs
--rn (JSElision xs)          = (char ',') <> (rJS xs)
rn (JSElision x)          = renderJS x

rn (JSExpressionBinary o e1 _op e2) = (rJS e1) <> (text o) <> (rJS e2)
rn (JSExpressionParen _lp e _rp)    = (char '(') <> (renderJS e) <> (char ')')
rn (JSExpressionPostfix o e _op)    = (rJS e) <> (text o)
rn (JSExpressionTernary c _q v1 _c v2) = (rJS c) <> (char '?') <> (rJS v1) <> (char ':') <> (rJS v2)
rn (JSFinally _f b)                 = (text "finally") <> (renderJS b)

rn (JSFor _f _lb e1 _s1 e2 _s2 e3 _rb s) = (text "for") <> (char '(') <> (commaList e1) <> (char ';')
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS $ fixBlock s)

rn (JSForIn _f _lb e1 _i e2 _rb s)       = (text "for") <> (char '(') <> (rJS e1) <+> (text "in")
                                         <+> (renderJS e2) <> (char ')') <> (renderJS $ fixBlock s)
rn (JSForVar _f _lb _v e1 _s1 e2 _s3 e3 _rb s) = (text "for") <> (char '(') <> (text "var") <+> (commaList e1) <> (char ';')
                                         <> (rJS e2) <> (char ';') <> (rJS e3) <> (char ')') <> (renderJS $ fixBlock s)
rn (JSForVarIn _f _lb _v e1 _i e2 _rb s) = (text "for") <> (char '(') <> (text "var") <+> (renderJS e1) <+> (text "in")
                                         <+> (renderJS e2) <> (char ')') <> (renderJS $ fixBlock s)

rn (JSLabelled l _c v)           = (renderJS l) <> (text ":") <> (rJS $ fixSourceElements [fixBlock v])
rn (JSObjectLiteral _lb xs _rb)  = (text "{") <> (commaList xs) <> (text "}")
rn (JSPropertyAccessor s n _lb1 ps _rb1 _lb2 b _rb2) = (renderJS s) <+> (renderJS n) <> (char '(') <> (rJS ps) <> (text ")") <> (renderJS b)
rn (JSPropertyNameandValue n _c vs) = (renderJS n) <> (text ":") <> (rJS vs)

rn (JSReturn _r [] _as)                 = (text "return")
rn (JSReturn _r [(NT (JSLiteral ";") _ _)] _as) = (text "return;")
rn (JSReturn _r xs _as)                 = (text "return") <> (if (spaceNeeded xs) then (text " ") else (empty)) <> (rJS $ fixSourceElements xs)

rn (JSThrow _t e)                 = (text "throw") <+> (renderJS e)

rn (JSStatementBlock _lb x _rb)   = (text "{") <> (renderJS x) <> (text "}")

rn (JSStatementList xs)          = rJS (fixSourceElements $ map fixBlock xs)

rn (JSSwitch _s _lb e _rb xs)    = (text "switch") <> (char '(') <> (renderJS e) <> (char ')') <>
                                   (char '{') <> (rJS $ fixSemis xs)  <> (char '}')
rn (JSTry _t e xs)               = (text "try") <> (renderJS e) <> (rJS xs)

rn (JSVarDecl i [])              = (renderJS i)
rn (JSVarDecl i xs)              = (renderJS i) <> (rJS xs)

rn (JSVariables kw xs _as)       = (renderJS kw) <+> (commaList xs)

rn (JSWhile _w _lb e _rb (NT (JSLiteral ";") _ _))   = (text "while") <> (char '(') <> (renderJS e) <> (char ')') -- <> (renderJS s)
rn (JSWhile _w _lb e _rb s)                 = (text "while") <> (char '(') <> (renderJS e) <> (char ')') <> (renderJS s)

rn (JSWith _w _lb e _rb s)                  = (text "with") <> (char '(') <> (renderJS e) <> (char ')') <> (rJS s)

-- Helper functions
rJS :: [JSNode] -> BB.Builder
rJS xs = hcat $ map renderJS xs

commaList :: [JSNode] -> BB.Builder
commaList [] = empty
commaList xs = (hcat $ (punctuate comma (toDoc xs') ++ trail))
  where
    -- (xs', trail) = if (last xs == JSLiteral ",") then (init xs, [comma]) else (xs,[])
    (xs', trail) = if (x' == JSLiteral ",") then (init xs, [comma]) else (xs,[])
    x' = extractNode $ last xs

extractNode :: JSNode -> Node
extractNode (NT x _ _) = x
extractNode (NN x    ) = x

--commaListList :: [[JSNode]] -> BB.Builder
--commaListList xs = (hcat $ punctuate comma $ map rJS xs)

toDoc :: [JSNode] -> [BB.Builder]
toDoc xs = map renderJS xs

spaceOrBlock :: JSNode -> BB.Builder
spaceOrBlock (NN (JSBlock lb xs rb)) = rn (JSBlock lb xs rb)
spaceOrBlock (NN (JSStatementBlock lb xs rb)) = rn (JSStatementBlock lb xs rb)
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
    n = extractNode $ last xs

-- Fix semicolons in output of sourceelements and statementlist

fixSourceElements :: [JSNode] -> [JSNode]
fixSourceElements xs = fixSemis $ myFix xs

myFix :: [JSNode] -> [JSNode]
myFix []      = []

-- Sort out empty IF statements
myFix ((NN (JSIf i lb c rb  (NN (JSStatementBlock _lb (NN (JSStatementList []) ) _rb) ) e)):xs) = (NN (JSIf i lb c rb (NT (JSLiteral "") tokenPosnEmpty []) e) ) : myFix (xs)

myFix [x]     = [x]

myFix (x:(NN (JSFunction v1 v2 v3 v4 v5 v6 v7 v8) ):xs)  = x : (NT (JSLiteral "\n") tokenPosnEmpty []) : myFix ((NN (JSFunction v1 v2 v3 v4 v5 v6 v7 v8) ) : xs)

-- Messy way, but it works, until the 3rd element arrives ..
-- TODO: JSStatementBlock.  Damn.
myFix ((NN (JSExpression x) ):(NN (JSExpression y) ):xs) = (NN (JSExpression x) ):(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSExpression y) ):xs)
myFix ((NN (JSExpression x) ):(NN (JSBlock l y r) ):xs)  = (NN (JSExpression x) ):(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSBlock l y r) ):xs)
myFix ((NN (JSBlock x1 x2 x3) )     :(NN (JSBlock y1 y2 y3) ):xs)      = (NN (JSBlock x1 x2 x3) )     :(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSBlock y1 y2 y3) ):xs)
myFix ((NN (JSBlock x1 x2 x3) )     :(NN (JSExpression y) ):xs) = (NN (JSBlock x1 x2 x3) )     :(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSExpression y) ):xs)

myFix ((NN (JSExpression x) ):(NN (JSStatementBlock y1 y2 y3) ):xs)      =
  (NN (JSExpression x) ):(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSStatementBlock y1 y2 y3) ):xs)
myFix ((NN (JSStatementBlock x1 x2 x3) )     :(NN (JSStatementBlock y1 y2 y3) ):xs)      =
  (NN (JSStatementBlock x1 x2 x3) )     :(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSStatementBlock y1 y2 y3) ):xs)
myFix ((NN (JSStatementBlock x1 x2 x3) )     :(NN (JSExpression y) ):xs) =
  (NN (JSStatementBlock x1 x2 x3) )     :(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSExpression y) ):xs)

-- Merge adjacent variable declarations, but only of the same type
myFix ((NN (JSVariables t1 x1s a1) ):(NT (JSLiteral l) s2 c2):(NN (JSVariables t2 x2s a2) ):xs)
  | t1 == t2 = myFix ((NN (JSVariables t1 (x1s++x2s) a2) ):xs)
  | otherwise = (NN (JSVariables t1 x1s a1) ):myFix ((NT (JSLiteral l) s2 c2):(NN (JSVariables t2 x2s a2) ):xs)

myFix ((NN (JSVariables t1 x1s a1) ):(NN (JSVariables t2 x2s a2) ):xs)
  | t1 == t2 = myFix ((NN (JSVariables t1 (x1s++x2s) a2) ):xs)
  | otherwise = (NN (JSVariables t1 x1s a1) ):myFix ((NN (JSVariables t2 x2s a2) ):xs)

-- Merge adjacent semi colons
myFix ((NT (JSLiteral ";") s1 c1):(NT (JSLiteral ";") _s2 _c2):xs)  = myFix ((NT (JSLiteral ";") s1 c1):xs)
myFix ((NT (JSLiteral ";") s1 c1):(NT (JSLiteral "" ) _s2 _c2):xs)  = myFix ((NT (JSLiteral "" ) s1 c1):xs)


myFix (x:xs)  = x : myFix xs

-- Merge strings split over lines and using "+"
fixLiterals :: [JSNode] -> [JSNode]
fixLiterals [] = []
-- Old version
fixLiterals ((NT (JSStringLiteral d1 s1) ss1 c1):(NN (JSExpressionBinary "+" [(NT (JSStringLiteral d2 s2) ss2 c2)] op r) ):xs)
       | d1 == d2 = fixLiterals ((NT (JSStringLiteral d1 (s1++s2)) ss1 c1):(r++xs))
       | otherwise = (NT (JSStringLiteral d1 s1) ss1 c1):fixLiterals ((NN (JSExpressionBinary "+" [(NT (JSStringLiteral d2 s2) ss2 c2)] op r) ):xs)

fixLiterals ((NN (JSExpressionBinary "+" [(NT (JSStringLiteral d1 s1) ss2 c2)] o1 [(NT (JSStringLiteral d2 s2) ss3 c3)]) ):xs)
       | d1 == d2 = fixLiterals ((NT (JSStringLiteral d1 (s1++s2)) ss2 c2):xs)
       | otherwise = (NN (JSExpressionBinary "+" [(NT (JSStringLiteral d1 s1) ss2 c2)] o1 [(NT (JSStringLiteral d2 s2) ss3 c3)]) ):fixLiterals xs

fixLiterals (x:xs) = x:fixLiterals xs

-- Sort out Semicolons
fixSemis :: [JSNode] -> [JSNode]
--fixSemis xs = fixSemis' $ filter (\x -> x /= JSLiteral ";" && x /= JSLiteral "") xs
fixSemis xs = fixSemis' $ filter (\x -> (extractNode x) /= JSLiteral ";" && (extractNode x) /= JSLiteral "") xs

fixSemis' :: [JSNode] -> [JSNode]
fixSemis' [] = []
fixSemis' [(NN (JSContinue c [(NT (JSLiteral ";") _ _)] as) )] = [(NN (JSContinue c [] as) )]
fixSemis' [x] = [x]
fixSemis' ((NN (JSIf i lb c rb (NN (JSReturn r [(NT (JSLiteral ";") s1 c1)] as) ) e) ):xs)  =
           (NN (JSIf i lb c rb (NN (JSReturn r [(NT (JSLiteral ";") s1 c1)] as) ) e) ):(fixSemis' xs)

fixSemis' ((NN (JSIf i lb c rb (NN (JSContinue co [(NT (JSLiteral ";") s1 c1)] as) ) e) ):xs)    =
           (NN (JSIf i lb c rb (NN (JSContinue co [(NT (JSLiteral ";") s1 c1)] as) ) e) ):(fixSemis' xs)

fixSemis' (x:(NT (JSLiteral "\n") s1 c1):xs) = x:(NT (JSLiteral "\n") s1 c1):(fixSemis' xs) -- TODO: is this needed?

fixSemis' ((NN (JSCase ca1 e1 c1 ((NN (JSStatementList []) ))) ):(NN (JSCase ca2 e2 c2 x) ):xs) =
           (NN (JSCase ca1 e1 c1 ((NN (JSStatementList []) ))) ):fixSemis' ((NN (JSCase ca2 e2 c2 x) ):xs)
fixSemis' (x:xs) = x:(NT (JSLiteral ";") tokenPosnEmpty []):fixSemis' xs

-- Remove extraneous braces around blocks
fixBlock :: JSNode -> JSNode

fixBlock (NN (JSBlock          _lb (NN (JSStatementList []) ) _rb ) ) = (NT (JSLiteral ";") tokenPosnEmpty [])
fixBlock (NN (JSStatementBlock _lb (NN (JSStatementList []) ) _rb ) ) = (NT (JSLiteral ";") tokenPosnEmpty [])

fixBlock (NN (JSBlock          _lb (NN (JSStatementList [x]) ) _rb ) ) = fixBlock x
fixBlock (NN (JSStatementBlock _lb (NN (JSStatementList [x]) ) _rb ) ) = fixBlock x

fixBlock    (NN (JSBlock lb (NN (JSStatementList xs) ) rb ) ) =
  fixBlock' (NN (JSBlock lb (NN (JSStatementList (fixSourceElements xs)) ) rb) )

fixBlock (NN (JSStatementBlock lb (NN (JSStatementList xs) ) rb ) ) =
  fixBlock' (NN (JSStatementBlock lb (NN (JSStatementList (fixSourceElements xs)) ) rb) )

fixBlock x = x

fixBlock' :: JSNode -> JSNode
fixBlock' (NN (JSBlock          _lb (NN (JSStatementList [x]) ) _rb) ) = x
fixBlock' (NN (JSStatementBlock _lb (NN (JSStatementList [x]) ) _rb) ) = x
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

_r :: JSNode -> [Char]
_r js = map (\x -> chr (fromIntegral x)) $ LB.unpack $ BB.toLazyByteString $ renderJS js

--readJs "{{{}}}"
_case0 :: JSNode
_case0 = undefined -- NS (JSSourceElementsTop [NN (JSStatementBlock (NN (JSStatementList [NN (JSStatementBlock (NN (JSStatementList [NN (JSStatementBlock (NN (JSStatementList []) (SpanPoint {span_filename = "", span_row = 1, span_column = 3}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 3})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 3}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 2})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 2}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 1})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 1})

_case01 :: [JSNode]
_case01 = undefined  -- [NN (JSStatementBlock (NN (JSStatementList [NN (JSStatementBlock (NN (JSStatementList [NN (JSStatementBlock (NN (JSStatementList []) (SpanPoint {span_filename = "", span_row = 1, span_column = 3}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 3})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 3}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 2})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 2}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 1})]

-- readJs "if(x){}{a=2}"
_case1 :: JSNode
_case1 = undefined -- NS (JSSourceElementsTop [NN (JSIf (NN (JSExpression [NS (JSIdentifier "x") (SpanPoint {span_filename = "", span_row = 1, span_column = 4})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 4})) (NN (JSStatementBlock (NN (JSStatementList []) (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 1}),NN (JSStatementBlock (NN (JSStatementList [NS (JSExpression [NS (JSIdentifier "a") (SpanPoint {span_filename = "", span_row = 1, span_column = 9}),NS (JSOperator "=") (SpanPoint {span_filename = "", span_row = 1, span_column = 10}),NS (JSDecimal "2") (SpanPoint {span_filename = "", span_row = 1, span_column = 11})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 9})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 9}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 8})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 1})

_case11 :: [JSNode]
_case11 = undefined -- [NN (JSIf (NN (JSExpression [NS (JSIdentifier "x") (SpanPoint {span_filename = "", span_row = 1, span_column = 4})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 4})) (NN (JSStatementBlock (NN (JSStatementList []) (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 1}),NN (JSStatementBlock (NN (JSStatementList [NS (JSExpression [NS (JSIdentifier "a") (SpanPoint {span_filename = "", span_row = 1, span_column = 9}),NS (JSOperator "=") (SpanPoint {span_filename = "", span_row = 1, span_column = 10}),NS (JSDecimal "2") (SpanPoint {span_filename = "", span_row = 1, span_column = 11})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 9})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 9}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 8})]

_case12 :: JSNode
_case12 = undefined -- (NN (JSIf (NN (JSExpression [NS (JSIdentifier "x") (SpanPoint {span_filename = "", span_row = 1, span_column = 4})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 4})) (NT (JSLiteral "") (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 6}))

-- readJs "bob:if(x){}\n{a}"
_case2 :: JSNode
_case2 = undefined -- NS (JSSourceElementsTop [NS (JSLabelled (NS (JSIdentifier "bob") (SpanPoint {span_filename = "", span_row = 1, span_column = 1})) (NN (JSIf (NN (JSExpression [NS (JSIdentifier "x") (SpanPoint {span_filename = "", span_row = 1, span_column = 8})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 8})) (NN (JSStatementBlock (NN (JSStatementList []) (SpanPoint {span_filename = "", span_row = 1, span_column = 10}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 10}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 5}))) (SpanPoint {span_filename = "", span_row = 1, span_column = 1}),NN (JSStatementBlock (NN (JSStatementList [NS (JSExpression [NS (JSIdentifier "a") (SpanPoint {span_filename = "", span_row = 2, span_column = 2})]) (SpanPoint {span_filename = "", span_row = 2, span_column = 2})]) (SpanPoint {span_filename = "", span_row = 2, span_column = 2}))) (SpanPoint {span_filename = "", span_row = 2, span_column = 1})]) (SpanPoint {span_filename = "", span_row = 1, span_column = 1})

-- EOF

