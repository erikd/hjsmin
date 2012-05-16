{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Jasmine.Transform
    (
    transformJS
    ) where

import Data.Char
import Data.List
import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Language.JavaScript.Parser


class TransformJS a where
    -- Transfrom node.
    tf :: a -> a

transformJS :: JSAST -> JSAST
transformJS jsnode = tf jsnode


instance TransformJS JSAST where
  tf (JSSourceElementsTop xs) = JSSourceElementsTop (tf $ fixTop $ fixSourceElements $ map fixStatementBlock xs)

instance TransformJS JSStatement where
    tf (JSStatementBlock blk)                       = (JSStatementBlock (tf blk))
    tf (JSBreak annot x1s s)                        = (JSBreak (tf annot) (tf x1s) (tf s))
    tf (JSContinue annot xs s)                      = (JSContinue (tf annot) (tf xs) (tf s))
    tf (JSConstant annot xs s)                      = (JSConstant (tf annot) (tf xs) (tf s))
    tf (JSDoWhile ad x1 aw alb x2 arb x3)           = (JSDoWhile (tf ad) (tf x1) (tf aw) (tf alb) (tf x2) (tf arb) (tf x3))

    tf (JSFor af alb x1s s1 x2s s2 x3s arb x4)      = (JSFor (tf af) (tf alb) (tf x1s) (tf s1) (tf x2s) (tf s2) (tf x3s) (tf arb) (tf $ fixStatementBlock x4))

    tf (JSForIn af alb x1s i x2 arb x3)             = (JSForIn (tf af) (tf alb) (tf x1s) (tf i) (tf x2) (tf arb) (tf $ fixStatementBlock x3))
    tf (JSForVar af alb v x1s s1 x2s s2 x3s arb x4) = (JSForVar (tf af) (tf alb) (addSpace v) (tf x1s) (tf s1) (tf x2s) (tf s2) (tf x3s) (tf arb) (tf $ fixStatementBlock x4))
    tf (JSForVarIn af alb v x1 i x2 arb x3)         = (JSForVarIn (tf af) (tf alb) (addSpace v) (tf x1) (tf i) (tf x2) (tf arb) (tf $ fixStatementBlock x3))
    tf (JSFunction af n alb x2s arb x3)             = (JSFunction (tf af) (addSpace n) (tf alb) (tf $ fixNew x2s) (tf arb) (tf $ fixFnBlock x3))

    tf (JSIf annot alb x1 arb x2s [JSNodeStmt (JSLiteral _ ";")])    = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf x2s) [])
    tf (JSIf annot alb x1 arb x2s [])                                = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf $ fixSourceElements $ map fixStatementBlock x2s) [])
    tf (JSIf annot alb x1 arb x2s [_e,JSNodeStmt (JSLiteral _ ";")]) = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf $ fixIfBlock x2s) ([JSNodeStmt (literal "else ")]))
    tf (JSIf annot alb x1 arb x2s [els,e                          ]) = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf $ fixIfElse $ fixSourceElements x2s)
                                                                        (els:(tf $ spaceOrBlock $ fixStatementBlock e)))
    -- tf (JSIf annot alb x1 arb x2s x3s)              = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf x2s) (tf x3s))


    tf (JSLabelled l c v)                           = (JSLabelled (tf l) (tf c) (tf v))
    tf (JSNodeStmt l)                               = (JSNodeStmt (tf l))

    tf (JSReturn annot [] s)                        = (JSReturn (tf annot) [] (tf s))
    tf (JSReturn annot xs s)                        = (JSReturn (tf annot) ((spaceIfNeeded xs)++(tf xs)) (tf s))


    tf (JSSwitch annot alp x arp alb x2 arb)        = (JSSwitch (tf annot) (tf alp) (tf x) (tf arp) (tf alb) (tf x2) (tf arb))
    tf (JSThrow annot x)                            = (JSThrow (tf annot) (tf $ addSpace x))
    tf (JSTry annot tb tcs tfi)                     = (JSTry (tf annot) (tf tb) (tf tcs) (tf tfi))
    tf (JSVarDecl x1 x2s)                           = (JSVarDecl (tf x1) (tf x2s))
    tf (JSVariable annot xs s)                      = (JSVariable (tf annot) ((JSNodeStmt (literal " ")):(tf xs)) (tf s))
    tf (JSWhile annot alp x1 arp x2)                = (JSWhile (tf annot) (tf alp) (tf x1) (tf arp) (tf x2))
    tf (JSWith annot alp x1 arp x s)                = (JSWith (tf annot) (tf alp) (tf x1) (tf arp) (tf x) (tf s))


instance TransformJS [JSStatement] where
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

instance TransformJS JSSwitchParts where
    tf (JSCase    annot x1 c x2s) = (JSCase    (tf annot) (tf x1) (tf c) (tf x2s))
    tf (JSDefault annot c xs)     = (JSDefault (tf annot) (tf c) (tf xs))

instance TransformJS [JSSwitchParts] where
    tf xs = map tf xs

instance TransformJS JSBlock where
    tf (JSBlock alb ss arb) = (JSBlock (tf alb) (tf ss) (tf arb))

instance TransformJS JSTryCatch where
    tf (JSCatch        anc alb x1 x2s arb x3) = (JSCatch  (tf anc) (tf alb) (tf x1) (tf x2s) (tf arb) (tf x3))

instance TransformJS [JSTryCatch] where
    tf xs = map tf xs

instance TransformJS JSTryFinally where
    tf (JSFinally      annot x) = (JSFinally      (tf annot) (tf x))
    tf JSNoFinally              = JSNoFinally


instance TransformJS JSNode where
    -- Terminals
    tf (JSIdentifier    annot s  ) = (JSIdentifier    (tf annot) (tf s)  )
    tf (JSDecimal       annot i  ) = (JSDecimal       (tf annot) (tf i)  )
    tf (JSLiteral       annot l  ) = (JSLiteral       (tf annot) (tf l)  )
    tf (JSHexInteger    annot i  ) = (JSHexInteger    (tf annot) (tf i)  )
    tf (JSOctal         annot i  ) = (JSOctal         (tf annot) (tf i)  )
    tf (JSStringLiteral annot s l) = (JSStringLiteral (tf annot) (tf s) (tf l))
    tf (JSRegEx         annot s  ) = (JSRegEx         (tf annot) (tf s)  )

    -- Non-Terminals
    tf (JSArguments            alp xs arp)             = (JSArguments            (tf alp) (tf $ fixLiterals $ fixNew xs) (tf arp))
    tf (JSArrayLiteral         als xs ars)             = (JSArrayLiteral         (tf als) (tf xs) (tf ars))
    tf (JSAssignExpression     lhs op rhs)             = (JSAssignExpression     (tf lhs) (tf op) (tf rhs))
    tf (JSCallExpression       os xs cs)               = (JSCallExpression       (tf os) (tf xs) (tf cs))
    tf (JSCallExpressionDot    os xs)                  = (JSCallExpressionDot    (tf os) (tf xs))
    tf (JSCallExpressionSquare als xs ars)             = (JSCallExpressionSquare (tf als) (tf xs) (tf ars))
    tf (JSElision              c)                      = (JSElision              (tf c))
    tf (JSExpression           xs)                     = (JSExpression           (tf $ fixNew xs))
    tf (JSExpressionBinary     lhs op rhs)             = (JSExpressionBinary     (tf lhs) (tf op) (tf rhs))
    tf (JSExpressionParen      alp e arp)              = (JSExpressionParen      (tf alp) (tf e) (tf arp))
    tf (JSExpressionPostfix    xs op)                  = (JSExpressionPostfix    (tf xs) (tf op))
    tf (JSExpressionTernary    cond h v1 c v2)         = (JSExpressionTernary    (tf cond) (tf h) (tf v1) (tf c) (tf v2))
    tf (JSFunctionExpression   annot x1s lb x2s rb x3) = (JSFunctionExpression   (tf annot) (tf x1s) (tf lb) (tf $ fixNew x2s) (tf rb) (tf $ fixFnBlock x3))
    tf (JSMemberDot            x dot n)                = (JSMemberDot            (tf $ head $ fixLiterals $ fixNew [x]) (tf dot) (tf n))
    tf (JSMemberSquare         x als e ars)            = (JSMemberSquare         (tf $ head $ fixLiterals $ fixNew [x]) (tf als) (tf e) (tf ars))
    tf (JSObjectLiteral        alb xs arb)             = (JSObjectLiteral        (tf alb) (tf xs) (tf arb))
    tf (JSOpAssign             n)                      = (JSOpAssign             (tf n))
    tf (JSPropertyAccessor     gs n alp ps arp b)      = (JSPropertyAccessor     (tf gs) (addSpace $ tf n) (tf alp) (tf ps) (tf arp) (tf b))
    tf (JSPropertyNameandValue n colon vs)             = (JSPropertyNameandValue (tf n) (tf colon) (tf vs))
    tf (JSUnaryExpression      op x)                   = (JSUnaryExpression      (tf op) (tf x))


instance TransformJS [JSNode] where
    tf xs = map tf xs

instance TransformJS JSAnnot where
    tf (JSAnnot p cs) = JSNoAnnot
    tf JSNoAnnot      = JSNoAnnot

instance TransformJS JSSemi where
    tf (JSSemi annot) = (JSSemi (tf annot))
    tf JSSemiAuto     = JSSemiAuto


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

instance TransformJS TokenPosn where
    tf (TokenPn x ltgt ctgt) = (TokenPn x ltgt ctgt)


instance TransformJS [CommentAnnotation] where
    tf xs = map tf xs


instance TransformJS CommentAnnotation where
    tf  NoComment = NoComment
    tf (CommentA   p s) = NoComment
    tf (WhiteSpace p s) = NoComment


instance TransformJS String where
    tf  s = s

instance TransformJS Char where
    tf  c = c


-- ---------------------------------------------------------------------

fixTop :: [JSStatement] -> [JSStatement]
fixTop [] = []
fixTop xs = if (isLiteralVal ";" n) then (init xs) else (xs)
  where
    n = last xs

-- ---------------------------------------------------------------------
-- Remove extraneous braces around blocks

fixStatementBlock :: JSStatement -> JSStatement
fixStatementBlock (JSStatementBlock (JSBlock lb xs rb)) =
  case xs' of
    []  -> (JSNodeStmt (JSLiteral JSNoAnnot ";"))
    [x] -> fixStatementBlock x
    _   -> (JSStatementBlock (JSBlock lb (fixSourceElements $ map fixStatementBlock xs') rb))
  where xs' = stripSemis xs

fixStatementBlock x = x


-- ---------------------------------------------------------------------

fixSourceElements :: [JSStatement] -> [JSStatement]
fixSourceElements xs = fixSemis $ myFix xs

-- ---------------------------------------------------------------------

myFix :: [JSStatement] -> [JSStatement]
-- TODO: implement this
myFix xs = xs


-- ---------------------------------------------------------------------
-- The "new" literal always need a space after it
fixNew :: [JSNode] -> [JSNode]
fixNew []                        = []
fixNew ((JSLiteral a1 "new"):xs) = (JSLiteral a1 "new ") : fixNew xs
fixNew (x                   :xs) = x                     : fixNew xs


-- ---------------------------------------------------------------------
-- Sort out Semicolons
fixSemis :: [JSStatement] -> [JSStatement]
fixSemis xs = fixSemis' $ stripSemis xs

stripSemis :: [JSStatement] -> [JSStatement]
stripSemis xs = filter (\x -> not (isLiteralVal ";" x) && not (isLiteralVal "" x)) xs

fixSemis' :: [JSStatement] -> [JSStatement]
fixSemis' [] = []

fixSemis' [(JSContinue a1 [(JSLiteral a2 ";")] as)] = [(JSContinue a1 [] as)]
fixSemis' [x] = [x]

fixSemis' ((JSIf i lb c rb [JSStatementBlock (JSBlock a1 [] a2)] []):xs) =
           (JSIf i lb c rb [JSStatementBlock (JSBlock a1 [] a2)] []):(fixSemis' xs)


fixSemis' ((JSIf i lb c rb [JSNodeStmt (JSLiteral a1 ";")] []):xs)  =
           (JSIf i lb c rb [JSNodeStmt (JSLiteral a1 ";")] []):(fixSemis' xs)

fixSemis' ((JSIf i lb c rb [(JSReturn a1 [(JSLiteral a2 ";")] as)] e):xs)  =
           (JSIf i lb c rb [(JSReturn a1 [(JSLiteral a2 ";")] as)] e):(fixSemis' xs)

fixSemis' ((JSIf i lb c rb [(JSContinue a1 [(JSLiteral a2 ";")] as)] e):xs)    =
           (JSIf i lb c rb [(JSContinue a1 [(JSLiteral a2 ";")] as)] e):(fixSemis' xs)

fixSemis' (x:(JSNodeStmt (JSLiteral a1 "\n")):xs) = x:(JSNodeStmt (JSLiteral a1 "\n")):(fixSemis' xs) -- TODO: is this needed?

{-
TODO: apply this to a JSSwitchParts fix
fixSemis' ((JSCase a1 e1 c1 []):(JSCase a2 e2 c2 x):xs) =
           (JSCase a1 e1 c1 []):fixSemis' ((JSCase a2 e2 c2 x):xs)
-}

fixSemis' (x:xs) = x:(JSNodeStmt (JSLiteral JSNoAnnot ";")):fixSemis' xs


-- ---------------------------------------------------------------------

fixFnBlock :: JSBlock -> JSBlock
fixFnBlock (JSBlock lb xs rb) = (JSBlock lb (fixSourceElements xs) rb)
-- fixFnBlock x = fixBlock x

-- ---------------------------------------------------------------------
-- Merge strings split over lines and using "+"
-- Merge strings split over lines and using "+"
fixLiterals :: [JSNode] -> [JSNode]
fixLiterals [] = []
{-
-- Old version
fixLiterals ((NT (JSStringLiteral d1 s1) ss1 c1):(NN (JSExpressionBinary "+" [(NT (JSStringLiteral d2 s2) ss2 c2)] op r) ):xs)
       | d1 == d2 = fixLiterals ((NT (JSStringLiteral d1 (s1++s2)) ss1 c1):(r++xs))
       | otherwise = (NT (JSStringLiteral d1 s1) ss1 c1):fixLiterals ((NN (JSExpressionBinary "+" [(NT (JSStringLiteral d2 s2) ss2 c2)] op r) ):xs)
-}
fixLiterals (x@((JSExpressionBinary (JSStringLiteral _a1 d1 s1) (JSBinOpPlus _) (JSStringLiteral a2 d2 s2))):xs)
       | d1 == d2 = fixLiterals ((JSStringLiteral JSNoAnnot d1 (s1++s2)):xs)
       | otherwise = x:fixLiterals xs

fixLiterals (x:xs) = x:fixLiterals xs



-- ---------------------------------------------------------------------


fixIfBlock :: [JSStatement] -> [JSStatement]
fixIfBlock xs =
  case xs' of
    [JSNodeStmt (JSLiteral a ";")]         -> [] -- rely on fixSemis to add this again [(NT (JSLiteral ";") p cs)]
    [JSStatementBlock (JSBlock lb [] rb)]  -> [] -- rely on fixSemis to add this again [(NT (JSLiteral ";") tokenPosnEmpty [])]
    [JSStatementBlock (JSBlock lb x2s rb)] -> [JSStatementBlock (JSBlock lb ((fixSourceElements $ map fixStatementBlock x2s)) rb)]
    [x]                   -> fixSourceElements [x]
  where xs' = stripSemis xs

-- ---------------------------------------------------------------------

fixIfElse :: [JSStatement] -> [JSStatement]
fixIfElse [JSStatementBlock (JSBlock lb xs rb)] = [JSStatementBlock (JSBlock lb (fixSourceElements xs) rb)]
fixIfElse [x] = [JSStatementBlock (JSBlock JSNoAnnot (fixSourceElements [x]) JSNoAnnot)]
fixIfElse xs = xs

-- ---------------------------------------------------------------------

spaceOrBlock :: JSStatement -> [JSStatement]
spaceOrBlock (JSStatementBlock x) = [(JSStatementBlock x)]
spaceOrBlock x                    = [JSNodeStmt (literal " "),x]

-- ---------------------------------------------------------------------

{-
addSpaceToNode
    -- Terminals
addSpaceToNode (JSIdentifier    annot s  ) = (JSIdentifier    (tf annot) (tf s)  )
addSpaceToNode (JSDecimal       annot i  ) = (JSDecimal       (tf annot) (tf i)  )
addSpaceToNode (JSLiteral       annot l  ) = (JSLiteral       (tf annot) (tf l)  )
addSpaceToNode (JSHexInteger    annot i  ) = (JSHexInteger    (tf annot) (tf i)  )
addSpaceToNode (JSOctal         annot i  ) = (JSOctal         (tf annot) (tf i)  )
addSpaceToNode (JSStringLiteral annot s l) = (JSStringLiteral (tf annot) (tf s) (tf l))
addSpaceToNode (JSRegEx         annot s  ) = (JSRegEx         (tf annot) (tf s)  )

    -- Non-Terminals
addSpaceToNode (JSArguments            alp xs arp)             = (JSArguments            (tf alp) (tf $ fixNew xs) (tf arp))
addSpaceToNode (JSArrayLiteral         als xs ars)             = (JSArrayLiteral         (tf als) (tf xs) (tf ars))
addSpaceToNode (JSAssignExpression     lhs op rhs)             = (JSAssignExpression     (tf lhs) (tf op) (tf rhs))
addSpaceToNode (JSCallExpression       os xs cs)               = (JSCallExpression       (tf os) (tf xs) (tf cs))
addSpaceToNode (JSCallExpressionDot    os xs)                  = (JSCallExpressionDot    (tf os) (tf xs))
addSpaceToNode (JSCallExpressionSquare als xs ars)             = (JSCallExpressionSquare (tf als) (tf xs) (tf ars))
addSpaceToNode (JSElision              c)                      = (JSElision              (tf c))
addSpaceToNode (JSExpression           xs)                     = (JSExpression           (tf $ fixNew xs))
addSpaceToNode (JSExpressionBinary     lhs op rhs)             = (JSExpressionBinary     (tf lhs) (tf op) (tf rhs))
addSpaceToNode (JSExpressionParen      alp e arp)              = (JSExpressionParen      (tf alp) (tf e) (tf arp))
addSpaceToNode (JSExpressionPostfix    xs op)                  = (JSExpressionPostfix    (tf xs) (tf op))
addSpaceToNode (JSExpressionTernary    cond h v1 c v2)         = (JSExpressionTernary    (tf cond) (tf h) (tf v1) (tf c) (tf v2))
addSpaceToNode (JSFunctionExpression   annot x1s lb x2s rb x3) = (JSFunctionExpression   (tf annot) (tf x1s) (tf lb) (tf $ fixNew x2s) (tf rb) (tf $ fixFnBlock x3))
addSpaceToNode (JSMemberDot            xs dot n)               = (JSMemberDot            (tf xs) (tf dot) (tf n))
addSpaceToNode (JSMemberSquare         xs als e ars)           = (JSMemberSquare         (tf xs) (tf als) (tf e) (tf ars))
addSpaceToNode (JSObjectLiteral        alb xs arb)             = (JSObjectLiteral        (tf alb) (tf xs) (tf arb))
addSpaceToNode (JSOpAssign             n)                      = (JSOpAssign             (tf n))
addSpaceToNode (JSPropertyAccessor     s n alp ps arp b)       = (JSPropertyAccessor     (tf s) (tf n) (tf alp) (tf ps) (tf arp) (tf b))
addSpaceToNode (JSPropertyNameandValue n colon vs)             = (JSPropertyNameandValue (tf n) (tf colon) (tf vs))
addSpaceToNode (JSUnaryExpression      op x)                   = (JSUnaryExpression      (tf op) (tf x))
-}

addSpace :: JSNode -> JSNode
addSpace (JSIdentifier _ s)  = (JSIdentifier (literalAnnotation " ") s)
addSpace (JSLiteral _ "var") = (JSLiteral JSNoAnnot "var ")
addSpace (JSExpression  xs)  = (JSExpression ((literal " "):xs))

-- Debug
addSpace x = (JSLiteral JSNoAnnot ("UNK|"++(show x)++"|"))


literalAnnotation s = (JSAnnot tokenPosnEmpty [WhiteSpace tokenPosnEmpty s])

-- ---------------------------------------------------------------------

literal :: String -> JSNode
literal s = JSLiteral JSNoAnnot s

isLiteralVal :: String -> JSStatement -> Bool
isLiteralVal stest (JSNodeStmt (JSLiteral _annot s)) = s == stest
isLiteralVal _ _ = False

-- ---------------------------------------------------------------------

spaceIfNeeded :: [JSNode] -> [JSNode]
spaceIfNeeded ((JSArguments _lb _xs _rb):xs)      = []
spaceIfNeeded ((JSExpressionParen _lp _x _rp):xs) = []
spaceIfNeeded _                                   = [(JSIdentifier JSNoAnnot " ")]


-- EOF

