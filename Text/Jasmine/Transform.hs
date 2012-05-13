{-# LANGUAGE FlexibleInstances #-}
module Text.Jasmine.Transform
    (
    transformJS
    ) where

import Data.Char
import Data.List
import Data.Monoid (Monoid, mappend, mempty, mconcat)
--import Language.JavaScript.Parser (JSNode(..),JSAnnot(..),tokenPosnEmpty,readJs,CommentAnnotation(..),TokenPosn(..))
import Language.JavaScript.Parser


class TransformJS a where
    -- Transfrom node.
    tf :: a -> a

transformJS :: JSNode -> JSNode
transformJS jsnode = tf jsnode


instance TransformJS JSNode where
    -- Terminals
    tf (JSIdentifier    annot s  ) = (JSIdentifier    (tf annot) s  )
    tf (JSDecimal       annot i  ) = (JSDecimal       (tf annot) i  )
    tf (JSLiteral       annot l  ) = (JSLiteral       (tf annot) l  )
    tf (JSHexInteger    annot i  ) = (JSHexInteger    (tf annot) i  )
    tf (JSOctal         annot i  ) = (JSOctal         (tf annot) i  )
    tf (JSStringLiteral annot s l) = (JSStringLiteral (tf annot) s l)
    tf (JSRegEx         annot s  ) = (JSRegEx         (tf annot) s  )

    -- Non-Terminals
    tf (JSArguments            lb xs rb)                                  = (JSArguments            (tf lb) (tf $ fixLiterals $ fixNew xs) (tf rb))
    tf (JSArrayLiteral         lb xs rb)                                  = (JSArrayLiteral         (tf lb) (tf xs) (tf rb))
    tf (JSAssignExpression     lhs op rhs)                                = (JSAssignExpression     (tf lhs) (tf op) (tf rhs))
    tf (JSBlock                lb xs rb)                                  = (JSBlock                (tf lb) (tf xs) (tf rb))
    tf (JSBreak                annot x1s s)                               = (JSBreak                (tf annot) (tf x1s)  (tf s))
    tf (JSCallExpression       os xs cs)                                  = (JSCallExpression       (tf os) (tf xs) (tf cs))
    tf (JSCallExpressionDot    os xs)                                     = (JSCallExpressionDot    (tf os) (tf xs))
    tf (JSCallExpressionSquare os xs cs)                                  = (JSCallExpressionSquare (tf os) (tf xs) (tf cs))
    tf (JSCase                 annot x1 c x2s)                            = (JSCase                 (tf annot) (tf x1) (tf c) (tf x2s))
    tf (JSCatch                annot lb x1 x2s rb x3)                     = (JSCatch                (tf annot) (tf lb) (tf x1) (tf x2s) (tf rb) (tf x3))
    tf (JSContinue             annot xs s)                                = (JSContinue             (tf annot) (tf xs) (tf s))
    tf (JSDefault              annot c xs)                                = (JSDefault              (tf annot) (tf c) (tf xs))
    tf (JSDoWhile              annot x1 annotw lb x2 rb x3)               = (JSDoWhile              (tf annot) (tf x1) (tf annotw) (tf lb) (tf x2) (tf rb) (tf x3))
    tf (JSElision              JSNoAnnot c)                               = (JSElision              JSNoAnnot (tf c))
    tf (JSExpression           xs)                                        = (JSExpression           (tf $ fixNew xs))
    tf (JSExpressionBinary     lhs op rhs)                                = (JSExpressionBinary     (tf lhs) (tf op) (tf rhs))
    tf (JSExpressionParen      lb e rb)                                   = (JSExpressionParen      (tf lb) (tf e) (tf rb))
    tf (JSExpressionPostfix    xs op)                                     = (JSExpressionPostfix    (tf xs) (tf op))
    tf (JSExpressionTernary    cond h v1 c v2)                            = (JSExpressionTernary    (tf cond) (tf h) (tf v1) (tf c) (tf v2))
    tf (JSFinally              annot x)                                   = (JSFinally              (tf annot) (tf x))
    tf (JSFor                  f lb x1s s1 x2s s2 x3s rb x4)              = (JSFor                  (tf f) (tf lb) (tf x1s) (tf s1) (tf x2s) (tf  s2) (tf x3s) (tf rb) (tf x4))
    tf (JSForIn                f lb x1s i x2 rb x3)                       = (JSForIn                (tf f) (tf lb) (tf x1s) (tf i) (tf x2) (tf rb) (tf x3))
    tf (JSForVar               f lb v x1s s1 x2s s2 x3s rb x4)            = (JSForVar               (tf f) (tf lb) (literal "var ") (tf x1s) (tf s1) (tf x2s) (tf s2) (tf x3s) (tf rb) (tf x4))
    tf (JSForVarIn             f lb v x1 i x2 rb x3)                      = (JSForVarIn             (tf f) (tf lb) (tf v) (tf x1) (tf i) (tf x2) (tf rb) (tf x3))
    tf (JSFunction             f x1 lb x2s rb x3)                         = (JSFunction             (literal "function ") (tf x1) (tf lb) (tf $ fixNew x2s) (tf rb) (tf $ fixFnBlock x3))
    tf (JSFunctionExpression   f x1s lb x2s rb x3)                        = (JSFunctionExpression   (tf f) (tf x1s) (tf lb) (tf $ fixNew x2s) (tf rb) (tf $ fixFnBlock x3))

    tf (JSIf                   i lb x1 rb [(JSLiteral a1 ";")] [])        = (JSIf                   (tf i) (tf lb) (tf x1) (tf rb) (tf []) (tf []))
    tf (JSIf                   i lb x1 rb x2s                  [])        = (JSIf                   (tf i) (tf lb) (tf x1) (tf rb) (tf $ fixSourceElements $ map fixBlock x2s) (tf []))
    tf (JSIf                   i lb x1 rb x2s [_e,(JSLiteral a1 ";")])    = (JSIf                   (tf i) (tf lb) (tf x1) (tf rb) (tf $ fixIfBlock x2s) (tf []))
    tf (JSIf                   i lb x1 rb x2s [_e,e])                     = (JSIf                   (tf i) (tf lb) (tf x1) (tf rb) (tf $ fixIfElse $ fixSourceElements x2s) [tf $ fixBlock e])


    tf (JSLabelled             l c v)                                     = (JSLabelled             (tf l) (tf c) (tf v))
    tf (JSMemberDot            xs dot n)                                  = (JSMemberDot            (tf xs) (tf dot) (tf n))
    tf (JSMemberSquare         xs lb e rb)                                = (JSMemberSquare         (tf xs) (tf lb) (tf e) (tf rb))
    tf (JSObjectLiteral        lb xs rb)                                  = (JSObjectLiteral        (tf lb) (tf xs) (tf rb))
    tf (JSOpAssign              n)                                        = (JSOpAssign             (tf n))
    tf (JSPropertyAccessor     JSNoAnnot s n lb1 ps rb1 b)                = (JSPropertyAccessor     JSNoAnnot (tf s) (tf n) (tf lb1) (tf ps) (tf rb1)  (tf b))
    tf (JSPropertyNameandValue JSNoAnnot n colon vs)                      = (JSPropertyNameandValue JSNoAnnot (tf n) (tf colon) (tf vs))
    tf (JSReturn               annot xs s)                                = (JSReturn               (tf annot) (tf xs) (tf s))
    tf (JSSourceElementsTop    JSNoAnnot xs)                              = (JSSourceElementsTop    JSNoAnnot (tf $ fixTop $ fixSourceElements $ map fixBlock xs))
    tf (JSSwitch               annot lb x rb x2)                          = (JSSwitch               (tf annot) (tf lb) (tf x) (tf rb) (tf x2))
    tf (JSThrow                annot x)                                   = (JSThrow                (tf annot) (tf x))
    tf (JSTry                  annot x1 x2s)                              = (JSTry                  (tf annot) (tf x1) (tf x2s))
    tf (JSUnaryExpression      op x)                                      = (JSUnaryExpression      (tf op) (tf x))
    tf (JSVarDecl              JSNoAnnot x1 x2s)                          = (JSVarDecl              JSNoAnnot (tf x1) (tf x2s))
    tf (JSVariables            JSNoAnnot n xs as)                         = (JSVariables            JSNoAnnot (tf n) (tf xs) (tf as))
    tf (JSWhile                annot lb x1 rb x2)                         = (JSWhile                (tf annot) (tf lb) (tf x1) (tf rb) (tf x2))
    tf (JSWith                 annot lb x1 rb x s)                        = (JSWith                 (tf annot) (tf lb) (tf x1) (tf rb) (tf x) (tf s))

    -- Debug helper
    tf what = JSStringLiteral JSNoAnnot 's' ("X " ++ show what ++ " X")

-- -----------------------------------------------------------------------------
-- Need an instance of TransformJS for every component of every JSNode or JSAnnot
-- constuctor.
-- -----------------------------------------------------------------------------

instance TransformJS JSAnnot where
    tf (JSAnnot p cs) = JSNoAnnot
    tf JSNoAnnot = JSNoAnnot


{-
instance TransformJS String where
    (|>) (PA (r,c) bb) s = PA (r',c') (bb <> str s)
      where
        (r',c') = foldl' (\(row,col) ch -> go (row,col) ch) (r,c) s

        go (rx,_)  '\n' = (rx+1,1)
        go (rx,cx) '\t' = (rx,cx+8)
        go (rx,cx) _    = (rx,cx+1)


instance TransformJS TokenPosn where
    (|>)  (PA (lcur,ccur) bb) (TokenPn _ ltgt ctgt) = PA (lnew,cnew) (bb <> bb')
      where
        (bbline,ccur') = if lcur < ltgt then (str (replicate (ltgt - lcur) '\n'),1) else (mempty,ccur)
        bbcol  = if ccur' < ctgt then str (replicate (ctgt - ccur') ' ') else mempty
        bb' = bbline <> bbcol
        lnew = if lcur < ltgt then ltgt else lcur
        cnew = if ccur' < ctgt then ctgt else ccur'


instance TransformJS [CommentAnnotation] where
    (|>) = foldl' (|>)


instance TransformJS CommentAnnotation where
    tf NoComment = pacc
    tf (CommentA   p s) = pacc |> p |> s
    tf (WhiteSpace p s) = pacc |> p |> s
-}

instance TransformJS [JSNode] where
    tf xs = map tf xs


instance TransformJS JSBinOp where
    tf (JSBinOpAnd        annot)  = (JSBinOpAnd        (tf annot))
    tf (JSBinOpBitAnd     annot)  = (JSBinOpBitAnd     (tf annot))
    tf (JSBinOpBitOr      annot)  = (JSBinOpBitOr      (tf annot))
    tf (JSBinOpBitXor     annot)  = (JSBinOpBitXor     (tf annot))
    tf (JSBinOpDivide     annot)  = (JSBinOpDivide     (tf annot))
    tf (JSBinOpEq         annot)  = (JSBinOpEq         (tf annot))
    tf (JSBinOpGe         annot)  = (JSBinOpGe         (tf annot))
    tf (JSBinOpGt         annot)  = (JSBinOpGt         (tf annot))
    tf (JSBinOpIn         annot)  = (JSBinOpIn         (tf annot))
    tf (JSBinOpInstanceOf annot)  = (JSBinOpInstanceOf (tf annot))
    tf (JSBinOpLe         annot)  = (JSBinOpLe         (tf annot))
    tf (JSBinOpLsh        annot)  = (JSBinOpLsh        (tf annot))
    tf (JSBinOpLt         annot)  = (JSBinOpLt         (tf annot))
    tf (JSBinOpMinus      annot)  = (JSBinOpMinus      (tf annot))
    tf (JSBinOpMod        annot)  = (JSBinOpMod        (tf annot))
    tf (JSBinOpNeq        annot)  = (JSBinOpNeq        (tf annot))
    tf (JSBinOpOr         annot)  = (JSBinOpOr         (tf annot))
    tf (JSBinOpPlus       annot)  = (JSBinOpPlus       (tf annot))
    tf (JSBinOpRsh        annot)  = (JSBinOpRsh        (tf annot))
    tf (JSBinOpStrictEq   annot)  = (JSBinOpStrictEq   (tf annot))
    tf (JSBinOpStrictNeq  annot)  = (JSBinOpStrictNeq  (tf annot))
    tf (JSBinOpTimes      annot)  = (JSBinOpTimes      (tf annot))
    tf (JSBinOpUrsh       annot)  = (JSBinOpUrsh       (tf annot))


instance TransformJS JSUnaryOp where
    tf (JSUnaryOpDecr   annot) = (JSUnaryOpDecr   (tf annot))
    tf (JSUnaryOpDelete annot) = (JSUnaryOpDelete (tf annot))
    tf (JSUnaryOpIncr   annot) = (JSUnaryOpIncr   (tf annot))
    tf (JSUnaryOpMinus  annot) = (JSUnaryOpMinus  (tf annot))
    tf (JSUnaryOpNot    annot) = (JSUnaryOpNot    (tf annot))
    tf (JSUnaryOpPlus   annot) = (JSUnaryOpPlus   (tf annot))
    tf (JSUnaryOpTilde  annot) = (JSUnaryOpTilde  (tf annot))
    tf (JSUnaryOpTypeof annot) = (JSUnaryOpTypeof (tf annot))
    tf (JSUnaryOpVoid   annot) = (JSUnaryOpVoid   (tf annot))


instance TransformJS JSAssignOp where
    tf (JSAssign       annot) = (JSAssign       (tf annot))
    tf (JSTimesAssign  annot) = (JSTimesAssign  (tf annot))
    tf (JSDivideAssign annot) = (JSDivideAssign (tf annot))
    tf (JSModAssign    annot) = (JSModAssign    (tf annot))
    tf (JSPlusAssign   annot) = (JSPlusAssign   (tf annot))
    tf (JSMinusAssign  annot) = (JSMinusAssign  (tf annot))
    tf (JSLshAssign    annot) = (JSLshAssign    (tf annot))
    tf (JSRshAssign    annot) = (JSRshAssign    (tf annot))
    tf (JSUrshAssign   annot) = (JSUrshAssign   (tf annot))
    tf (JSBwAndAssign  annot) = (JSBwAndAssign  (tf annot))
    tf (JSBwXorAssign  annot) = (JSBwXorAssign  (tf annot))
    tf (JSBwOrAssign   annot) = (JSBwOrAssign   (tf annot))


instance TransformJS JSLParen where
    tf (JSLParen annot) = (JSLParen (tf annot))

instance TransformJS JSRParen where
    tf (JSRParen annot) = (JSRParen (tf annot))

instance TransformJS JSLBrace where
    tf (JSLBrace annot) = (JSLBrace (tf annot))

instance TransformJS JSRBrace where
    tf (JSRBrace annot) = (JSRBrace (tf annot))

instance TransformJS JSLSquare where
    tf (JSLSquare annot) = (JSLSquare (tf annot))

instance TransformJS JSRSquare where
    tf (JSRSquare annot) = (JSRSquare (tf annot))

instance TransformJS JSSemi where
    tf (JSSemi annot) = (JSSemi (tf annot))
    tf JSSemiAuto     = JSSemiAuto

-- ---------------------------------------------------------------------

literal :: String -> JSNode
literal s = JSLiteral JSNoAnnot s

isLiteralVal :: String -> JSNode -> Bool
isLiteralVal stest (JSLiteral _annot s) = s == stest
isLiteralVal _ _ = False

-- ---------------------------------------------------------------------

fixTop :: [JSNode] -> [JSNode]
fixTop [] = []
fixTop xs = if (isLiteralVal ";" n) then (init xs) else (xs)
  where
    n = last xs

-- ---------------------------------------------------------------------
-- Remove extraneous braces around blocks
fixBlock :: JSNode -> JSNode

fixBlock (JSBlock lb xs rb) =
  case xs' of
    []  -> (JSLiteral JSNoAnnot ";")
    [x] -> fixBlock x
    _   -> (JSBlock lb (fixSourceElements $ map fixBlock xs') rb)
  where xs' = stripSemis xs

fixBlock x = x

-- ---------------------------------------------------------------------

fixSourceElements :: [JSNode] -> [JSNode]
fixSourceElements xs = fixSemis $ myFix $ fixNew xs

-- ---------------------------------------------------------------------

myFix :: [JSNode] -> [JSNode]
-- TODO: implement this
myFix xs = xs


-- ---------------------------------------------------------------------
-- The "new" literal always need a space after it
fixNew :: [JSNode] -> [JSNode]
fixNew []                               = []
fixNew ((JSLiteral a1 "new"):xs) = (JSLiteral a1 "new ") : fixNew xs
fixNew (x                   :xs) = x                     : fixNew xs


-- ---------------------------------------------------------------------
-- Sort out Semicolons
fixSemis :: [JSNode] -> [JSNode]
fixSemis xs = fixSemis' $ stripSemis xs

stripSemis :: [JSNode] -> [JSNode]
stripSemis xs = filter (\x -> not (isLiteralVal ";" x) && not (isLiteralVal "" x)) xs

fixSemis' :: [JSNode] -> [JSNode]
fixSemis' [] = []
fixSemis' [(JSContinue a1 [(JSLiteral a2 ";")] as)] = [(JSContinue a1 [] as)]
fixSemis' [x] = [x]

fixSemis' ((JSIf i lb c rb [(JSBlock (JSLBrace a1) [] (JSRBrace a2))] []):xs) =
           (JSIf i lb c rb [(JSBlock (JSLBrace a1) [] (JSRBrace a2))] []):(fixSemis' xs)


fixSemis' ((JSIf i lb c rb [(JSLiteral a1 ";")] []):xs)  =
           (JSIf i lb c rb [(JSLiteral a1 ";")] []):(fixSemis' xs)

fixSemis' ((JSIf i lb c rb [(JSReturn a1 [(JSLiteral a2 ";")] as)] e):xs)  =
           (JSIf i lb c rb [(JSReturn a1 [(JSLiteral a2 ";")] as)] e):(fixSemis' xs)

fixSemis' ((JSIf i lb c rb [(JSContinue a1 [(JSLiteral a2 ";")] as)] e):xs)    =
           (JSIf i lb c rb [(JSContinue a1 [(JSLiteral a2 ";")] as)] e):(fixSemis' xs)

fixSemis' (x:(JSLiteral a1 "\n"):xs) = x:(JSLiteral a1 "\n"):(fixSemis' xs) -- TODO: is this needed?

fixSemis' ((JSCase a1 e1 c1 []):(JSCase a2 e2 c2 x):xs) =
           (JSCase a1 e1 c1 []):fixSemis' ((JSCase a2 e2 c2 x):xs)

fixSemis' (x:xs) = x:(JSLiteral JSNoAnnot ";"):fixSemis' xs


-- ---------------------------------------------------------------------

fixFnBlock :: JSNode -> JSNode
fixFnBlock (JSBlock lb xs rb) = (JSBlock lb (fixSourceElements xs) rb)
fixFnBlock x = fixBlock x

-- ---------------------------------------------------------------------
-- Merge strings split over lines and using "+"
fixLiterals :: [JSNode] -> [JSNode]
fixLiterals [] = []

--fixLiterals x@((JSExpressionBinary (JSStringLiteral a1 d1 l1) (JSBinOpPlus ao) (JSStringLiteral a2 d2 l2)):xs)
--  | d1 == d2 = (fixLiterals (JSStringLiteral a1 d1 (s1++s2):xs)
--  | otherwise = x:fixLiterals xs

fixLiterals (x:xs) = x:fixLiterals xs

-- ---------------------------------------------------------------------

fixIfBlock :: [JSNode] -> [JSNode]
fixIfBlock xs =
  case xs' of
    [(JSLiteral a ";")]   -> [] -- rely on fixSemis to add this again [(NT (JSLiteral ";") p cs)]
    [(JSBlock lb [] rb)]  -> [] -- rely on fixSemis to add this again [(NT (JSLiteral ";") tokenPosnEmpty [])]
    [(JSBlock lb x2s rb)] -> [(JSBlock lb ((fixSourceElements $ map fixBlock x2s)) rb)]
    [x]                   -> fixSourceElements [x]
  where xs' = stripSemis xs

-- ---------------------------------------------------------------------

fixIfElse :: [JSNode] -> [JSNode]
fixIfElse [(JSBlock lb xs rb)] = [(JSBlock lb (fixSourceElements xs) rb)]
fixIfElse [x] = [(JSBlock (JSLBrace JSNoAnnot) (fixSourceElements [x]) (JSRBrace JSNoAnnot))]
fixIfElse xs = xs



-- EOF

