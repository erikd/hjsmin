{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Jasmine.Transform
    (
    transformJS
    -- For debug stuff
    , literalAnnotation
    , literal
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

    tf (JSBreak annot [ ] s)                        = (JSBreak (tf annot) []                JSSemiAuto) -- (tf s))
    tf (JSBreak annot [x] s)                        = (JSBreak (tf annot) [addSpace $ tf x] JSSemiAuto) -- (tf s))

    tf (JSContinue annot xs s)                      = (JSContinue (tf annot) (tf xs) JSSemiAuto) -- (tf s))
    tf (JSConstant annot xs s)                      = (JSConstant (tf annot) ((JSExpressionStatement (literal " ")):(tf xs)) (JSSemiAuto))
    tf (JSDoWhile ad x1 aw alb x2 arb x3)           = (JSDoWhile (tf ad) (tf $ fixDoStatement x1) (tf aw) (tf alb) (tf x2) (tf arb) (JSSemiAuto))

    tf (JSFor af alb x1s s1 x2s s2 x3s arb x4)      = (JSFor (tf af) (tf alb) (tf x1s) (tf s1) (tf x2s) (tf s2) (tf x3s) (tf arb) (tf $ fixStatementBlock x4))

    tf (JSForIn af alb x1s i x2 arb x3)             = (JSForIn (tf af) (tf alb) (tf x1s) (JSBinOpIn (literalAnnotation " ")) (addSpace $ tf x2) (tf arb) (tf $ fixStatementBlock x3))
    tf (JSForVar af alb v x1s s1 x2s s2 x3s arb x4) = (JSForVar (tf af) (tf alb) (addSpace $ tf v) (tf x1s) (tf s1) (tf x2s) (tf s2) (tf x3s) (tf arb) (tf $ fixStatementBlock x4))
    tf (JSForVarIn af alb v x1 i x2 arb x3)         = (JSForVarIn (tf af) (tf alb) (addSpace $ tf v) (tf x1) (tf i) (tf x2) (tf arb) (tf $ fixStatementBlock x3))
    tf (JSFunction af n alb x2s arb x3)             = (JSFunction (tf af) (addSpace $ tf n) (tf alb) (tf $ fixNew x2s) (tf arb) (tf $ fixFnBlock x3))

    tf (JSIf annot alb x1 arb x2s [JSExpressionStatement (JSLiteral _ ";")])    = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf x2s) [])
    tf (JSIf annot alb x1 arb x2s [])                                = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf $ fixSourceElements $ map fixStatementBlock x2s) [])
    tf (JSIf annot alb x1 arb x2s [_e,JSExpressionStatement (JSLiteral _ ";")]) = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf $ fixIfBlock x2s) ([JSExpressionStatement (literal "else ")]))
    tf (JSIf annot alb x1 arb x2s [els,e                          ]) = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf $ fixIfElse $ fixSourceElements x2s)
                                                                        ((tf els):(tf $ spaceOrBlock $ fixStatementBlock e)))
    -- tf (JSIf annot alb x1 arb x2s x3s)              = (JSIf (tf annot) (tf alb) (tf x1) (tf arb) (tf x2s) (tf x3s))


    tf (JSLabelled l c v)                           = (JSLabelled (tf l) (tf c) (tf v))
    tf (JSExpressionStatement l)                               = (JSExpressionStatement (tf l))

    tf (JSReturn annot [] s)                        = (JSReturn (tf annot) [] JSSemiAuto) --  (tf s))
    tf (JSReturn annot xs s)                        = (JSReturn (tf annot) ((spaceIfNeeded xs)++(tf xs)) JSSemiAuto) -- (tf s))


    tf (JSSwitch annot alp x arp alb x2 arb)        = (JSSwitch (tf annot) (tf alp) (tf x) (tf arp) (tf alb) (fixSwitchSemis $ tf x2) (tf arb))
    tf (JSThrow annot x)                            = (JSThrow (tf annot) (addSpace $ tf x))
    tf (JSTry annot tb tcs tfi)                     = (JSTry (tf annot) (tf tb) (tf tcs) (tf tfi))
    tf (JSVarDecl x1 x2s)                           = (JSVarDecl (tf x1) (tf x2s))
    tf (JSVariable annot xs s)                      = (JSVariable (tf annot) ((JSExpressionStatement (literal " ")):(tf xs)) (JSSemiAuto))
    tf (JSWhile annot alp x1 arp x2)                = (JSWhile (tf annot) (tf alp) (tf x1) (tf arp) (tf $ head $ fixSourceElements [x2]))
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
    tf (JSCase    annot x1 c x2s) = (JSCase    (tf annot) (addSpace $ tf x1) (tf c) (tf $ fixSourceElements x2s))
    tf (JSDefault annot c xs)     = (JSDefault (tf annot) (tf c) (tf $ fixSourceElements xs))

instance TransformJS [JSSwitchParts] where
    tf xs = map tf xs

instance TransformJS JSBlock where
    tf (JSBlock alb ss arb) = (JSBlock (tf alb) (tf $ fixSourceElements ss) (tf arb))

instance TransformJS JSTryCatch where
    tf (JSCatch        anc alb x1 ifs arb x3) = (JSCatch  (tf anc) (tf alb) (tf x1) (tf $ fixCatchIf ifs) (tf arb) (tf x3))

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

    tf (JSExpressionBinary     lhs (JSBinOpInstanceOf a) rhs) = (JSExpressionBinary     (tf lhs) ((JSBinOpInstanceOf (literalAnnotation " "))) (addSpace $ tf rhs))
    tf (JSExpressionBinary     lhs (JSBinOpIn         a) rhs) = (JSExpressionBinary     (tf lhs) ((JSBinOpIn         (literalAnnotation " "))) (addSpace $ tf rhs))
    tf (JSExpressionBinary     lhs op rhs)             = (JSExpressionBinary     (tf lhs) (tf op) (tf rhs))

    tf (JSExpressionParen      alp e arp)              = (JSExpressionParen      (tf alp) (tf e) (tf arp))
    tf (JSExpressionPostfix    xs op)                  = (JSExpressionPostfix    (tf xs) (tf op))
    tf (JSExpressionTernary    cond h v1 c v2)         = (JSExpressionTernary    (tf cond) (tf h) (tf v1) (tf c) (tf v2))

    tf (JSFunctionExpression   annot  [] lb x2s rb x3) = (JSFunctionExpression   (tf annot) ([])                     (tf lb) (tf $ fixNew x2s) (tf rb) (tf $ fixFnBlock x3))
    tf (JSFunctionExpression   annot x1s lb x2s rb x3) = (JSFunctionExpression   (tf annot) ((literal " "):(tf x1s)) (tf lb) (tf $ fixNew x2s) (tf rb) (tf $ fixFnBlock x3))

    tf (JSMemberDot            x dot n)                = (JSMemberDot            (tf $ head $ fixLiterals $ fixNew [x]) (tf dot) (tf n))
    tf (JSMemberSquare         x als e ars)            = (JSMemberSquare         (tf $ head $ fixLiterals $ fixNew [x]) (tf als) (tf e) (tf ars))
    tf (JSObjectLiteral        alb xs arb)             = (JSObjectLiteral        (tf alb) (tf xs) (tf arb))
    tf (JSOpAssign             n)                      = (JSOpAssign             (tf n))
    tf (JSPropertyAccessor     gs n alp ps arp b)      = (JSPropertyAccessor     (tf gs) (addSpace $ tf n) (tf alp) (tf ps) (tf arp) (tf b))
    tf (JSPropertyNameandValue n colon vs)             = (JSPropertyNameandValue (tf n) (tf colon) (tf vs))

    tf (JSUnaryExpression      (JSUnaryOpTypeof _) x)  = (JSUnaryExpression      (JSUnaryOpTypeof JSNoAnnot) (addSpace $ tf x))
    tf (JSUnaryExpression      (JSUnaryOpDelete _) x)  = (JSUnaryExpression      (JSUnaryOpDelete JSNoAnnot) (addSpace $ tf x))
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
    []  -> (JSExpressionStatement (JSLiteral JSNoAnnot ";"))
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
myFix []      = []

myFix [x]     = [x]

myFix (x:(JSFunction v1 v2 v3 v4 v5 v6):xs)  = x : (JSExpressionStatement (literal "\n")) : myFix ((JSFunction v1 v2 v3 v4 v5 v6) : xs)

{-
-- Messy way, but it works, until the 3rd element arrives ..
-- TODO: JSStatementBlock.  Damn.
myFix ((NN (JSExpression x) ):(NN (JSExpression y) ):xs) = (NN (JSExpression x) ):(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSExpression y) ):xs)
myFix ((NN (JSExpression x) ):(NN (JSBlock l y r) ):xs)  = (NN (JSExpression x) ):(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSBlock l y r) ):xs)
myFix ((NN (JSBlock x1 x2 x3) )     :(NN (JSBlock y1 y2 y3) ):xs)      = (NN (JSBlock x1 x2 x3) )     :(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSBlock y1 y2 y3) ):xs)
myFix ((NN (JSBlock x1 x2 x3) )     :(NN (JSExpression y) ):xs) = (NN (JSBlock x1 x2 x3) )     :(NT (JSLiteral ";") tokenPosnEmpty []):myFix ((NN (JSExpression y) ):xs)
-}

-- Merge adjacent variable declarations, but only of the same type
myFix ((JSVariable a1 x1s s1):(JSVariable a2 x2s s2):xs) = myFix ((JSVariable a1 (x1s++[(JSExpressionStatement (JSLiteral a1 ","))]++x2s) s1):xs)
myFix ((JSConstant a1 x1s s1):(JSConstant a2 x2s s2):xs) = myFix ((JSConstant a1 (x1s++[(JSExpressionStatement (JSLiteral a1 ","))]++x2s) s1):xs)

{-
-- Merge adjacent semi colons
myFix ((NT (JSLiteral ";") s1 c1):(NT (JSLiteral ";") _s2 _c2):xs)  = myFix ((NT (JSLiteral ";") s1 c1):xs)
myFix ((NT (JSLiteral ";") s1 c1):(NT (JSLiteral "" ) _s2 _c2):xs)  = myFix ((NT (JSLiteral "" ) s1 c1):xs)
-}

myFix (x:xs)  = x : myFix xs


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


fixSemis' ((JSIf i lb c rb [JSExpressionStatement (JSLiteral a1 ";")] []):xs)  =
           (JSIf i lb c rb [JSExpressionStatement (JSLiteral a1 ";")] []):(fixSemis' xs)

fixSemis' ((JSIf i lb c rb [(JSReturn a1 [(JSLiteral a2 ";")] as)] e):xs)  =
           (JSIf i lb c rb [(JSReturn a1 [(JSLiteral a2 ";")] as)] e):(fixSemis' xs)

fixSemis' ((JSIf i lb c rb [(JSContinue a1 [(JSLiteral a2 ";")] as)] e):xs)    =
           (JSIf i lb c rb [(JSContinue a1 [(JSLiteral a2 ";")] as)] e):(fixSemis' xs)

fixSemis' (x:(JSExpressionStatement (JSLiteral a1 "\n")):xs) = x:(JSExpressionStatement (JSLiteral a1 "\n")):(fixSemis' xs) -- TODO: is this needed?

{-
TODO: apply this to a JSSwitchParts fix
fixSemis' ((JSCase a1 e1 c1 []):(JSCase a2 e2 c2 x):xs) =
           (JSCase a1 e1 c1 []):fixSemis' ((JSCase a2 e2 c2 x):xs)
-}

fixSemis' (x:xs) = x:(JSExpressionStatement (JSLiteral JSNoAnnot ";")):fixSemis' xs

-- ---------------------------------------------------------------------

fixSwitchSemis :: [JSSwitchParts] -> [JSSwitchParts]
fixSwitchSemis [] = []
fixSwitchSemis [x] = [x]
fixSwitchSemis ((JSCase a1 x1 c1 []):(JSCase a2 x2 c2 x2s):xs) = ((JSCase a1 x1 c1 []): fixSwitchSemis ((JSCase a2 x2 c2 x2s):xs))
fixSwitchSemis ((JSCase a x1 c x2s):xs) = ((JSCase a x1 c (x2s++[JSExpressionStatement (JSLiteral a ";")])): (fixSwitchSemis xs))
fixSwitchSemis ((JSDefault a c x1s):xs) = ((JSDefault a c (x1s++[JSExpressionStatement (JSLiteral a ";")])): (fixSwitchSemis xs))


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

fixDoStatement :: JSStatement -> JSStatement
fixDoStatement (JSStatementBlock (JSBlock a1 xs a2)) = (JSStatementBlock (JSBlock JSNoAnnot (fixSourceElements xs) JSNoAnnot))
fixDoStatement x = (JSStatementBlock (JSBlock JSNoAnnot (fixSourceElements [x]) JSNoAnnot))

-- ---------------------------------------------------------------------

fixIfBlock :: [JSStatement] -> [JSStatement]
fixIfBlock xs =
  case xs' of
    [JSExpressionStatement (JSLiteral a ";")]         -> [] -- rely on fixSemis to add this again [(NT (JSLiteral ";") p cs)]
    [JSStatementBlock (JSBlock lb [] rb)]  -> [] -- rely on fixSemis to add this again [(NT (JSLiteral ";") tokenPosnEmpty [])]
    [JSStatementBlock (JSBlock lb x2s rb)] -> [JSStatementBlock (JSBlock lb ((fixSourceElements $ map fixStatementBlock x2s)) rb)]
    [x]                   -> fixSourceElements [x]
  where xs' = stripSemis xs

-- ---------------------------------------------------------------------

fixIfElse :: [JSStatement] -> [JSStatement]
fixIfElse [JSStatementBlock (JSBlock lb xs rb)] = [JSStatementBlock (JSBlock JSNoAnnot (fixSourceElements xs) JSNoAnnot)]
fixIfElse [x] = [JSStatementBlock (JSBlock JSNoAnnot (fixSourceElements [x]) JSNoAnnot)]
fixIfElse xs = xs

-- ---------------------------------------------------------------------

fixCatchIf :: [JSNode] -> [JSNode]
fixCatchIf [] = []
fixCatchIf [i,c] = [literal " if ",c]

-- ---------------------------------------------------------------------

spaceOrBlock :: JSStatement -> [JSStatement]
spaceOrBlock (JSStatementBlock x) = [(JSStatementBlock x)]
spaceOrBlock x                    = [JSExpressionStatement (literal " "),x]

--fixExpression :: JSNode -> JSNode
--JSExpressionStatement (JSLiteral Annot (TokenPn 13 2 27) [NoComment] ";")
-- ---------------------------------------------------------------------

addSpace :: JSNode -> JSNode

    -- Terminals
addSpace (JSIdentifier _ s)  = (JSIdentifier (literalAnnotation " ") s)

addSpace (JSDecimal       _ i  ) = (JSDecimal       (literalAnnotation " ") i  )
addSpace (JSLiteral   _ "var"  ) = (JSLiteral JSNoAnnot "var ")
addSpace (JSLiteral       _ l  ) = (JSLiteral       (literalAnnotation " ") l  )
addSpace (JSHexInteger    _ i  ) = (JSHexInteger    (literalAnnotation " ") i  )
addSpace (JSOctal         _ i  ) = (JSOctal         (literalAnnotation " ") i  )

--addSpace (JSStringLiteral _ s l) = (JSStringLiteral (literalAnnotation " ") s l)
addSpace (JSStringLiteral _ s l) = (JSStringLiteral JSNoAnnot s l)

addSpace (JSRegEx         _ s  ) = (JSRegEx         (literalAnnotation " ") s  )

addSpace (JSExpressionBinary lhs op rhs)  = (JSExpressionBinary (addSpace lhs) op rhs)

addSpace (JSArguments            alp xs arp)             = (JSArguments            (literalAnnotation " ") xs arp)
addSpace (JSArrayLiteral         als xs ars)             = (JSArrayLiteral         (literalAnnotation " ") xs ars)
addSpace (JSAssignExpression     lhs op rhs)             = (JSAssignExpression     (addSpace lhs) op rhs)
addSpace (JSCallExpressionDot    os xs)                  = (JSCallExpressionDot    (tf os) (tf xs))
addSpace (JSCallExpressionSquare als xs ars)             = (JSCallExpressionSquare (literalAnnotation " ") xs ars)
addSpace (JSElision              c)                      = (JSElision              (addSpace c))
addSpace (JSExpression           xs)                     = (JSExpression           ((literal " "):xs))
addSpace (JSExpressionParen      alp e arp)              = (JSExpressionParen      (literalAnnotation " ") e arp)
addSpace (JSExpressionPostfix    xs op)                  = (JSExpressionPostfix    (addSpace xs) op)
addSpace (JSExpressionTernary    cond h v1 c v2)         = (JSExpressionTernary    (addSpace cond) h v1 c v2)
addSpace (JSFunctionExpression   annot x1s lb x2s rb x3) = (JSFunctionExpression   (literalAnnotation " ") x1s lb x2s rb x3)
addSpace (JSMemberDot            xs dot n)               = (JSMemberDot            (addSpace xs) dot n)
addSpace (JSMemberSquare         xs als e ars)           = (JSMemberSquare         (addSpace xs) als e ars)
addSpace (JSObjectLiteral        alb xs arb)             = (JSObjectLiteral        (literalAnnotation " ") xs arb)

addSpace (JSOpAssign             (JSAssign _))           = (JSOpAssign        (JSAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSTimesAssign _))      = (JSOpAssign        (JSTimesAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSDivideAssign _))     = (JSOpAssign        (JSDivideAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSModAssign _))        = (JSOpAssign        (JSModAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSPlusAssign _))       = (JSOpAssign        (JSPlusAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSMinusAssign _))      = (JSOpAssign        (JSMinusAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSLshAssign _))        = (JSOpAssign        (JSLshAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSRshAssign _))        = (JSOpAssign        (JSRshAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSUrshAssign _))       = (JSOpAssign        (JSUrshAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSBwAndAssign _))      = (JSOpAssign        (JSBwAndAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSBwXorAssign _))      = (JSOpAssign        (JSBwXorAssign (literalAnnotation " ")))
addSpace (JSOpAssign             (JSBwOrAssign _))       = (JSOpAssign        (JSBwOrAssign (literalAnnotation " ")))


addSpace (JSPropertyAccessor     s n alp ps arp b)       = (JSPropertyAccessor     (addSpace s) n alp ps arp b)
addSpace (JSPropertyNameandValue n colon vs)             = (JSPropertyNameandValue (addSpace n) colon vs)

addSpace (JSUnaryExpression      (JSUnaryOpDecr _) n)    = (JSUnaryExpression      (JSUnaryOpDecr (literalAnnotation " ")) n)
addSpace (JSUnaryExpression      (JSUnaryOpDelete _) n)  = (JSUnaryExpression      (JSUnaryOpDelete (literalAnnotation " ")) n)
addSpace (JSUnaryExpression      (JSUnaryOpIncr _) n)    = (JSUnaryExpression      (JSUnaryOpIncr (literalAnnotation " ")) n)
addSpace (JSUnaryExpression      (JSUnaryOpMinus _) n)   = (JSUnaryExpression      (JSUnaryOpMinus (literalAnnotation " ")) n)
addSpace (JSUnaryExpression      (JSUnaryOpNot _) n)     = (JSUnaryExpression      (JSUnaryOpNot (literalAnnotation " ")) n)
addSpace (JSUnaryExpression      (JSUnaryOpPlus _) n)    = (JSUnaryExpression      (JSUnaryOpPlus (literalAnnotation " ")) n)
addSpace (JSUnaryExpression      (JSUnaryOpTilde _) n)   = (JSUnaryExpression      (JSUnaryOpTilde (literalAnnotation " ")) n)
addSpace (JSUnaryExpression      (JSUnaryOpTypeof _) n)  = (JSUnaryExpression      (JSUnaryOpTypeof (literalAnnotation " ")) n)
addSpace (JSUnaryExpression      (JSUnaryOpVoid _) n)    = (JSUnaryExpression      (JSUnaryOpVoid (literalAnnotation " ")) n)


-- Debug
addSpace x = (JSLiteral JSNoAnnot ("UNK|"++(show x)++"|"))


literalAnnotation s = (JSAnnot tokenPosnEmpty [WhiteSpace tokenPosnEmpty s])

-- ---------------------------------------------------------------------

literal :: String -> JSNode
literal s = JSLiteral JSNoAnnot s

isLiteralVal :: String -> JSStatement -> Bool
isLiteralVal stest (JSExpressionStatement (JSLiteral _annot s)) = s == stest
isLiteralVal _ _ = False

-- ---------------------------------------------------------------------

spaceIfNeeded :: [JSNode] -> [JSNode]
spaceIfNeeded ((JSArguments _lb _xs _rb):xs)      = []
spaceIfNeeded ((JSExpressionParen _lp _x _rp):xs) = []
spaceIfNeeded ((JSExpressionTernary (JSExpressionParen _lp _x _rp) _ _ _ _):xs) = []
spaceIfNeeded ((JSMemberSquare      (JSExpressionParen _lp _x _rp) _ _ _)  :xs) = []
spaceIfNeeded ((JSMemberDot         (JSExpressionParen _lp _x _rp) _ _)    :xs) = []
spaceIfNeeded ((JSAssignExpression (JSMemberSquare (JSExpressionParen _lp _x _rp) _ _ _) _ _):xs) = []
spaceIfNeeded _                                   = [(JSIdentifier JSNoAnnot " ")]

{-
Expression
  -> AssignmentExpression
     -> ConditionalExpression
        -> LogicalOrExpression
           -> LogicalAndExpression
              -> BitwiseOrExpression
                 -> BitwiseXOrExpression
                    -> BitwiseAndExpression
                       -> EqualityExpression
                          -> RelationalExpression
                             -> ShiftExpression
                                -> AdditiveExpression
                                   -> MultiplicativeExpression
                                      -> UnaryExpression
                                         -> PostfixExpression
                                            -> LeftHandSideExpression
                                               -> NewExpression
                                                  -> MemberExpression
                                                     -> PrimaryExpression
                                                        -> AST.JSExpressionParen***
                                               -> CallExpression
                                                  -> MemberExpression seen
     -> LeftHandSideExpression

-}

-- EOF

