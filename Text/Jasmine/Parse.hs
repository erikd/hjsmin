{-# LANGUAGE DeriveDataTypeable #-}
module Text.Jasmine.Parse
    (       
      readJs
    , readJsm  
    , JasmineSettings (..)
    , defaultJasmineSettings
    , JSNode(..)  
    , parseFile  
    , parseString  
    -- For testing      
    , doParse
    , program  
    , functionDeclaration
    , identifier
    , statementList  
    , iterationStatement  
    , main  
    ) where

-- ---------------------------------------------------------------------

import Control.Applicative ( (<|>) )
import Control.Monad
import Data.Attoparsec.Lazy (eitherResult,parse,Result(..))
import Data.Attoparsec.Char8 (char, satisfy, try, Parser, (<?>), endOfInput, many, sepBy, sepBy1, many1, takeWhile)
import Data.Char
import Data.Data
import Data.List hiding (takeWhile)
import Prelude hiding (catch, takeWhile)
import System.Environment
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Text.Jasmine.Token as P
import qualified Data.ByteString.Char8 as S8

-- ---------------------------------------------------------------------
{-
data Result v = Error String | Ok v
    deriving (Show, Eq, Read, Data, Typeable)
instance Monad Result where
    return = Ok
    Error s >>= _ = Error s
    Ok v >>= f = f v
    fail = Error
instance Functor Result where
    fmap = liftM
instance Applicative Result where
    pure = return
    (<*>) = ap
-}

-- ---------------------------------------------------------------------

data JSNode = JSArguments [[JSNode]]  
              | JSArrayLiteral [JSNode]
              | JSBlock JSNode
              | JSBreak [JSNode] [JSNode]
              | JSCallExpression String [JSNode] -- type : ., (), []; rest  
              | JSCase JSNode JSNode
              | JSCatch JSNode [JSNode] JSNode
              | JSContinue [JSNode]
              | JSDecimal Integer   
              | JSDefault JSNode
              | JSDoWhile JSNode JSNode JSNode
              | JSElement String [JSNode]
              | JSElementList [JSNode]  
              | JSElision [JSNode]  
              | JSEmpty JSNode
              | JSExpression [JSNode]
              | JSExpressionBinary String [JSNode] [JSNode]
              | JSExpressionParen JSNode
              | JSExpressionPostfix String [JSNode]
              | JSExpressionTernary [JSNode] [JSNode] [JSNode]
              | JSFinally JSNode  
              | JSFor [JSNode] [JSNode] [JSNode] JSNode                
              | JSForIn [JSNode] JSNode JSNode
              | JSForVar [JSNode] [JSNode] [JSNode] JSNode                
              | JSForVarIn JSNode JSNode JSNode 
              | JSFunction JSNode [JSNode] JSNode -- name, parameter list, body
              | JSFunctionBody [JSNode]
              | JSFunctionExpression [JSNode] JSNode -- name, parameter list, body                
              | JSHexInteger Integer  
              | JSIdentifier String
              | JSIf JSNode JSNode  
              | JSIfElse JSNode JSNode JSNode 
              | JSLabelled JSNode JSNode  
              | JSLiteral String  
              | JSMemberDot [JSNode]  
              | JSMemberSquare JSNode [JSNode]  
              | JSObjectLiteral [JSNode]  
              | JSOperator String  
              | JSPropertyNameandValue JSNode [JSNode]
              | JSRegEx String
              | JSReturn [JSNode]
              | JSSourceElements [JSNode]
              | JSSourceElementsTop [JSNode]
              | JSStatementList [JSNode]
              | JSStringLiteral Char [Char]
              | JSSwitch JSNode [JSNode]
              | JSThrow JSNode  
              | JSTry JSNode [JSNode]  
              | JSUnary String  
              | JSVarDecl JSNode [JSNode]
              | JSVariables String [JSNode]  
              | JSWhile JSNode JSNode
              | JSWith JSNode [JSNode]
    deriving (Show, Eq, Read, Data, Typeable)


-- ---------------------------------------------------------------------
-- | Settings for parsing of a javascript document.
data JasmineSettings = JasmineSettings
    {
      -- | Placeholder in the structure, no actual settings yet
      hjsminPlaceholder :: String
    }

-- ---------------------------------------------------------------------
-- | Defaults settings: settings not currently used
defaultJasmineSettings :: JasmineSettings
defaultJasmineSettings = JasmineSettings "foo"

-- ---------------------------------------------------------------------
-- Interface to the Tokeniser

identifier :: Parser JSNode
identifier  = do{ val <- P.identifier;
                  return (JSIdentifier val)}

autoSemi :: Parser JSNode
autoSemi = fmap JSLiteral P.autoSemi

autoSemi' :: Parser JSNode
autoSemi' = fmap JSLiteral P.autoSemi'

-- TODO: Maybe? get rid of these, and call them directly via the full name.
--       BUT, compiler optimisation should get rid of them anyway
rOp :: String -> Parser ()
rOp = P.rOp

lChar :: Char -> Parser ()
lChar = P.lChar

-- ---------------------------------------------------------------------
-- Make Attoparsec work with parsec

letter :: Parser Char
letter = satisfy isAlpha <?> "letter"

eof :: Parser ()
eof = endOfInput

-- ---------------------------------------------------------------------
-- The parser, based on the gold parser for Javascript
-- http://www.devincook.com/GOLDParser/grammars/files/JavaScript.zip
 
-- ------------------------------------------------------------
--Modified from HJS

stringLiteral :: Parser JSNode
stringLiteral =
    P.lexeme $  go '"' <|> go '\''
  where
    go c = do
        _ <- char c
        s <- takeWhile $ \x -> isPrint x && x /= c
        _ <- char c
        return $ JSStringLiteral c $ S8.unpack s

-- ------------------------------------------------------------


decimalLiteral :: Parser Integer
decimalLiteral = P.dec

hexIntegerLiteral :: Parser Integer
hexIntegerLiteral = P.hex  

-- {String Chars1} = {Printable} + {HT} - ["\] 
-- {RegExp Chars} = {Letter}+{Digit}+['^']+['$']+['*']+['+']+['?']+['{']+['}']+['|']+['-']+['.']+[',']+['#']+['[']+[']']+['_']+['<']+['>']
-- {Non Terminator} = {String Chars1} - {CR} - {LF}
-- RegExp         = '/' ({RegExp Chars} | '\' {Non Terminator})+ '/' ( 'g' | 'i' | 'm' )*

-- ---------------------------------------------------------------------
-- From HJS

regex :: Parser [Char]
regex = do { _ <- char '/'; body <- do { c <- firstchar; cs <- many otherchar; return $ concat (c:cs) }; _ <- char '/'; 
             flg <- identPart; return $ ("/"++body++"/"++flg) }

firstchar :: Parser [Char]
firstchar = do { c <- satisfy (\c -> isPrint c && c /= '*' && c /= '\\' && c /= '/'); 
                 return [c]} <|> escapeseq
            

escapeseq :: Parser [Char]
escapeseq = do { _ <- char '\\'; c <- satisfy (\cc -> isPrint cc); return ['\\',c]}

otherchar :: Parser [Char]
otherchar = do { c <- satisfy (\c -> isPrint c && c /= '\\' && c /= '/'); 
                 return [c]} <|> escapeseq

identPart :: Parser [Char]
identPart = many letter

-- ---------------------------------------------------------------------


regExp :: Parser JSNode
regExp = P.lexeme $ do { v1 <- regex;       
              return (JSRegEx v1)}

-- <Literal> ::= <Null Literal>
--             | <Boolean Literal>
--             | <Numeric Literal>
--             | StringLiteral
literal :: Parser JSNode
literal = nullLiteral
      <|> booleanLiteral
      <|> numericLiteral
      <|> stringLiteral

-- <Null Literal>    ::= null
nullLiteral :: Parser JSNode
nullLiteral = do { _ <- P.reserved "null"; 
                   return (JSLiteral "null")}

-- <Boolean Literal> ::= 'true'
--                     | 'false'
booleanLiteral :: Parser JSNode
booleanLiteral = do{ _ <- P.reserved "true" ; 
                     return (JSLiteral "true")}
             <|> do{ P.reserved "false"; 
                     return (JSLiteral "false")}

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
numericLiteral :: Parser JSNode
numericLiteral = do {val <- decimalLiteral; 
                     return (JSDecimal val)}
             <|> do {val <- hexIntegerLiteral; 
                     return (JSHexInteger val)}


-- <Regular Expression Literal> ::= RegExp 
regularExpressionLiteral :: Parser JSNode
regularExpressionLiteral = regExp


-- <Primary Expression> ::= 'this'
--                        | Identifier
--                        | <Literal> 
--                        | <Array Literal>
--                        | <Object Literal>
--                        | '(' <Expression> ')'
--                        | <Regular Expression Literal>
primaryExpression :: Parser JSNode
primaryExpression = do {P.reserved "this"; 
                        -- return [""]} 
                        return (JSLiteral "this")}
                <|> identifier
                <|> literal
                <|> arrayLiteral
                <|> objectLiteral
                <|> do{ rOp "("; val <- expression; rOp ")"; 
                        return (JSExpressionParen val)}
                <|> regularExpressionLiteral

-- ---------------------------------------------------------------------
-- Rework array literal


-- <Array Literal> ::= '[' ']'
--                   | '[' <Elision> ']'
--                   | '[' <Element List> ']'
--                   | '[' <Element List> ',' <Elision> ']'

-- <Elision> ::= ','
--             | <Elision> ','

-- <Element List> ::= <Elision> <Assignment Expression>
--                  | <Element List> ',' <Elision>  <Assignment Expression>
--                  | <Element List> ',' <Assignment Expression>
--                  | <Assignment Expression>

--------
--so

-- <Array Literal> ::= '[' many (',' <|> assignment) ']'


-- ---------------------------------------------------------------------

-- <Array Literal> ::= '[' ']'
--                   | '[' <Elision> ']'
--                   | '[' <Element List> ']'
--                   | '[' <Element List> ',' <Elision> ']'
arrayLiteral :: Parser JSNode
arrayLiteral = do {rOp "["; v1 <- many (do { rOp ","; return [(JSElision [])]} <|> assignmentExpression); rOp "]";
                   return (JSArrayLiteral (flatten v1)) }

{-
arrayLiteral :: GenParser Char P.JSPState JSNode
arrayLiteral = do {rOp "["; 
                   do {
                        do { rOp "]"; 
                             return (JSArrayLiteral [])}
                    <|> do { v1 <- elision; rOp "]"; 
                             return (JSArrayLiteral [v1])}
                    <|> do { v1 <- elementList; rOp "]"; 
                             do {
                               do { rOp ","; v2 <- elision; rOp "]"; 
                                    return (JSArrayLiteral (v1++[v2]))}
                               <|> return (JSArrayLiteral v1)
                               }
                             }
                        }
                   }


-- <Elision> ::= ','
--             | <Elision> ','
elision :: GenParser Char P.JSPState JSNode
elision = do{ rOp ",";
              return (JSElision [])}
      <|> do{ v1 <- elision; rOp ",";
              return (JSElision [v1])}
    

-- <Element List> ::= <Elision> <Assignment Expression>
--                  | <Element List> ',' <Elision>  <Assignment Expression>
--                  | <Element List> ',' <Assignment Expression>
--                  | <Assignment Expression>
elementList :: GenParser Char P.JSPState [JSNode]
elementList = do { v1 <- elision; v2 <- assignmentExpression; v3 <-rest;
                   return [(JSElementList (v1:(v2++v3)))] }
          <|> do { v1 <- assignmentExpression; v2 <- rest;
                   return [(JSElementList (v1++v2))]}
          where   
            rest =
                  do {rOp ",";
                      do { 
                            do { v2 <- elision; v3 <- assignmentExpression;
                                return [] {- ([v2]++v3)-}}
                        <|> do { v2 <- assignmentExpression;
                                return [] {-v2-}}
                         }
                      }
                  <|> do {return []}
-}   

-- <Object Literal> ::= '{' <Property Name and Value List> '}'
objectLiteral :: Parser JSNode
objectLiteral = do{ rOp "{"; val <- propertyNameandValueList; rOp "}"; 
                   return (JSObjectLiteral val)}

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>
propertyNameandValueList :: Parser [JSNode]
propertyNameandValueList = do{ val <- sepBy propertyNameandValue (rOp ","); -- Note: can be zero elements
                               return val}
                           
-- Seems we can have function declarations in the value part too                           
propertyNameandValue :: Parser JSNode
propertyNameandValue = do{ v1 <- propertyName; rOp ":"; 
                           do {
                                do {v2 <- assignmentExpression;
                                    return (JSPropertyNameandValue v1 v2)}
                            <|> do {v2 <- functionDeclaration;    
                                    return (JSPropertyNameandValue v1 [v2])}
                                }
                           }

-- <Property Name> ::= Identifier
--                   | StringLiteral
--                   | <Numeric Literal>
propertyName :: Parser JSNode
propertyName = identifier
           <|> stringLiteral
           <|> numericLiteral


-- <Member Expression > ::= <Primary Expression>
--                        | <Function Expression>
--                        | <Member Expression> '[' <Expression> ']'
--                        | <Member Expression> '.' Identifier
--                        | 'new' <Member Expression> <Arguments>
memberExpression :: Parser [JSNode]
memberExpression = (do{ P.reserved "new"; v1 <- memberExpression; v2 <- arguments; 
                        return (((JSLiteral "new "):v1)++[v2])}) -- xxxx
                <|> memberExpression'

--memberExpression' :: GenParser Char P.JSPState [JSNode]
memberExpression' :: Parser [JSNode]
memberExpression' = do
    v1 <- functionExpression <|> primaryExpression
    v2 <- rest
    return $ v1 : v2

                where
                  rest = do{ rOp "["; v1 <- expression; rOp "]"; v2 <- rest;
                             return [JSMemberSquare v1 v2]}
                     <|> do{ rOp "."; v1 <- identifier ; v2 <- rest;
                             return [JSMemberDot (v1:v2)]}
                     <|> return []
                         


-- <New Expression> ::= <Member Expression>
--                    | new <New Expression>
newExpression :: Parser [JSNode]
newExpression = memberExpression
           <|> do{ P.reserved "new"; val <- newExpression;
                   return ((JSLiteral "new "):val)}

-- <Call Expression> ::= <Member Expression> <Arguments>
--                     | <Call Expression> <Arguments> 
--                     | <Call Expression> '[' <Expression> ']'
--                     | <Call Expression> '.' Identifier

callExpression :: Parser [JSNode]
callExpression = do{ v1 <- memberExpression; v2 <- arguments; 
                      do { v3 <- rest; 
                          return (v1++[v2]++v3)}
                  <|> do {return (v1++[v2]    )}
                   }
                 where
                   rest =
                         do{ v4 <- arguments ; v5 <- rest;
                             return ([(JSCallExpression "()" [v4])]++v5)}
                     <|> do{ rOp "["; v4 <- expression; rOp "]"; v5 <- rest;
                             return ([JSCallExpression "[]" [v4]]++v5)}
                     <|> do{ rOp "."; v4 <- identifier; v5 <- rest;
                             return ([JSCallExpression "." [v4]]++v5)}
                     <|> return [] -- As per HJS, seems to extend the syntax

-- ---------------------------------------------------------------------
-- From HJS
{-                   
callExpr = do { x <- memberExpr;
               do {rOp "("; whiteSpace; args <- commaSep assigne; whiteSpace; rOp ")"; rest $ CallMember x args } 
           <|> do { return $ CallPrim x } 
           <|> do { rOp "++"; return $ CallPrim x }
              }
           where
                  rest x = 
                               try (do { rOp "("; args <- commaSep assigne; rOp ")" ; rest $ CallCall x args })
                           <|> try (do { rOp "."; i <- identifier; rest $ CallDot x i })
                           <|> try (do { rOp "["; e <- expr; rOp "]"; rest $ CallSquare x e })
                           <|> return x 
-}
-- ---------------------------------------------------------------------

lparen, rparen, comma :: Char
lparen = '('
rparen = ')'
comma  = ','

-- <Arguments> ::= '(' ')'
--               | '(' <Argument List> ')'
arguments :: Parser JSNode
arguments = do
    _ <- lChar lparen
    x <- ((lChar rparen) >> return [[]]) 
         <|> (do x <- argumentList
                 _ <- lChar rparen
                 return x)
    return $ JSArguments x

-- <Argument List> ::= <Assignment Expression>
--                   | <Argument List> ',' <Assignment Expression>
argumentList :: Parser [[JSNode]]
argumentList = sepBy1 assignmentExpression $ lChar comma

-- <Left Hand Side Expression> ::= <New Expression> 
--                               | <Call Expression>
leftHandSideExpression :: Parser [JSNode]
leftHandSideExpression = callExpression
                     <|> newExpression
                     <?> "leftHandSideExpression"


-- <Postfix Expression> ::= <Left Hand Side Expression>
--                        | <Postfix Expression> '++'
--                        | <Postfix Expression> '--'
postfixExpression :: Parser [JSNode]
postfixExpression = do{ v1 <- leftHandSideExpression;
                        do {
                              do{ rOp "++"; return [(JSExpressionPostfix "++" v1)]}
                          <|> do{ rOp "--"; return [(JSExpressionPostfix "--" v1)]}
                          <|> return v1
                           }
                        }

-- <Unary Expression> ::= <Postfix Expression>
--                      | 'delete' <Unary Expression>
--                      | 'void' <Unary Expression>
--                      | 'typeof' <Unary Expression>
--                      | '++' <Unary Expression>
--                      | '--' <Unary Expression>
--                      | '+' <Unary Expression>
--                      | '-' <Unary Expression>
--                      | '~' <Unary Expression>
--                      | '!' <Unary Expression>
unaryExpression :: Parser [JSNode]
unaryExpression = do{ v1 <- postfixExpression; 
                      return v1}
              <|> do{ P.reserved "delete"; v1 <- unaryExpression;
                      return ((JSUnary "delete "):v1)}
              <|> do{ P.reserved "void";   v1 <- unaryExpression;
                      return ((JSUnary "void"):v1)}
              <|> do{ P.reserved "typeof"; v1 <- unaryExpression; 
                      return ((JSUnary "typeof "):v1)} -- TODO: should the space always be there?
              <|> do{ rOp "++"; v1 <- unaryExpression;
                      return ((JSUnary "++"):v1)}
              <|> do{ rOp "--"; v1 <- unaryExpression;
                      return ((JSUnary "--"):v1)}
              <|> do{ rOp "+"; v1 <- unaryExpression;
                      return ((JSUnary "+"):v1)}
              <|> do{ rOp "-"; v1 <- unaryExpression;
                      return ((JSUnary "-"):v1)}
              <|> do{ rOp "~"; v1 <- unaryExpression;
                      return ((JSUnary "~"):v1)}
              <|> do{ rOp "!"; v1 <- unaryExpression;
                      return ((JSUnary "!"):v1)}

-- <Multiplicative Expression> ::= <Unary Expression>
--                               | <Unary Expression> '*' <Multiplicative Expression> 
--                               | <Unary Expression> '/' <Multiplicative Expression>                               
--                               | <Unary Expression> '%' <Multiplicative Expression> 
multiplicativeExpression :: Parser [JSNode]
multiplicativeExpression = do{ v1 <- unaryExpression; v2 <- rest;
                               return (v1++v2)}
                           where
                             rest =
                                  do{ rOp "*"; v2 <- multiplicativeExpression; v3 <- rest;
                                      return [(JSExpressionBinary "*" v2 v3)]}
                                  <|> do{ rOp "/"; v2 <- multiplicativeExpression; v3 <- rest;
                                          return [(JSExpressionBinary "/" v2 v3)]}
                                  <|> do{ rOp "%"; v2 <- multiplicativeExpression; v3 <- rest;
                                          return [(JSExpressionBinary "%" v2 v3)]}
                                  <|> return []


-- <Additive Expression> ::= <Additive Expression>'+'<Multiplicative Expression> 
--                         | <Additive Expression>'-'<Multiplicative Expression>  
--                         | <Multiplicative Expression>
additiveExpression :: Parser [JSNode]
additiveExpression = do{ v1 <- multiplicativeExpression; v2 <- rest;
                         return (v1++v2)}
                     where
                       rest =
                             do { rOp "+"; v2 <- multiplicativeExpression; v3 <- rest;
                                  return ([(JSExpressionBinary "+" v2 v3)])}
                         <|> do { rOp "-"; v2 <- multiplicativeExpression; v3 <- rest;
                                  return ([(JSExpressionBinary "-" v2 v3)])}
                         <|>  return []


-- <Shift Expression> ::= <Shift Expression> '<<' <Additive Expression>
--                      | <Shift Expression> '>>' <Additive Expression>
--                      | <Shift Expression> '>>>' <Additive Expression>
--                      | <Additive Expression>
shiftExpression :: Parser [JSNode]
shiftExpression = do{ v1 <- additiveExpression; v2 <- rest;   
                      return (v1++v2)}
                  where
                    rest =
                          do{ rOp "<<"; v2 <- additiveExpression; v3 <- rest;
                              return [(JSExpressionBinary "<<" v2 v3)]}
                      <|> do{ rOp ">>>"; v2 <- additiveExpression; v3 <- rest;
                              return [(JSExpressionBinary ">>>" v2 v3)]}
                      <|> do{ rOp ">>"; v2 <- additiveExpression; v3 <- rest;
                              return [(JSExpressionBinary ">>" v2 v3)]}
                      <|> return []


-- <Relational Expression>::= <Shift Expression> 
--                          | <Relational Expression> '<' <Shift Expression> 
--                          | <Relational Expression> '>' <Shift Expression> 
--                          | <Relational Expression> '<=' <Shift Expression> 
--                          | <Relational Expression> '>=' <Shift Expression> 
--                          | <Relational Expression> 'instanceof' <Shift Expression> 
relationalExpression :: Parser [JSNode]
relationalExpression = do{ v1 <- shiftExpression; v2 <- rest;
                           return (v1++v2)}
                       where
                         rest =
                                 do{ rOp "<="; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary "<=" v2 v3)]}
                             <|> do{ rOp ">="; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary ">=" v2 v3)]}

                             <|> do{ rOp "<"; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary "<" v2 v3)]}
                             <|> do{ rOp ">"; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary ">" v2 v3)]}
                             <|> do{ P.reserved "instanceof"; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary " instanceof " v2 v3)]}
                             -- Strictly speaking should have all the NoIn variants of expressions,    
                             -- but we assume syntax is checked so no problem. Cross fingers.    
                             <|> do{ P.reserved "in"; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary " in " v2 v3)]}
                             <|> return []



-- <Equality Expression> ::= <Relational Expression>
--                         | <Equality Expression> '==' <Relational Expression>
--                         | <Equality Expression> '!=' <Relational Expression>
--                         | <Equality Expression> '===' <Relational Expression>
--                         | <Equality Expression> '!==' <Relational Expression>
equalityExpression :: Parser [JSNode]
equalityExpression = do
    v1 <- relationalExpression
    v2 <- rest
    return $ v1 ++ v2
  where
    rest = equals <|> exclaim <|> return []
    equals = do
        _ <- char '='
        _ <- char '='
        sign <- P.lexeme $ (char '=' >> return "===") <|> return "=="
        v2 <- relationalExpression
        v3 <- rest
        return [JSExpressionBinary sign v2 v3]
    exclaim = do
        _ <- char '!'
        _ <- char '='
        sign <- P.lexeme $ (char '=' >> return "!==") <|> return "!="
        v2 <- relationalExpression
        v3 <- rest
        return [JSExpressionBinary sign v2 v3]

bitwiseExpression :: Parser [JSNode] -> Char -> Parser [JSNode]
bitwiseExpression prev op = do
    v1 <- prev
    v2 <- rest
    return $ v1 ++ v2
  where
    rest = (do
        _ <- P.lexeme $ char op
        v2 <- equalityExpression
        v3 <- rest
        return [JSExpressionBinary [op] v2 v3]) <|> return []

-- <Bitwise And Expression> ::= <Equality Expression>
--                            | <Bitwise And Expression> '&' <Equality Expression>
bitwiseAndExpression :: Parser [JSNode]
bitwiseAndExpression = bitwiseExpression equalityExpression '&'

-- <Bitwise XOr Expression> ::= <Bitwise And Expression>
--                            | <Bitwise XOr Expression> '^' <Bitwise And Expression>
bitwiseXOrExpression :: Parser [JSNode]
bitwiseXOrExpression = bitwiseExpression bitwiseAndExpression '^'

-- <Bitwise Or Expression> ::= <Bitwise XOr Expression>
--                           | <Bitwise Or Expression> '|' <Bitwise XOr Expression>
bitwiseOrExpression :: Parser [JSNode]
bitwiseOrExpression = bitwiseExpression bitwiseXOrExpression '|'

-- <Logical And Expression> ::= <Bitwise Or Expression>
--                            | <Logical And Expression> '&&' <Bitwise Or Expression>
logicalAndExpression :: Parser [JSNode]
logicalAndExpression = do{ v1 <- bitwiseOrExpression; v2 <- rest;
                           return (v1++v2)}
                       where
                         rest =
                               do{ rOp "&&"; v2 <- bitwiseOrExpression; v3 <- rest;
                                   return [(JSExpressionBinary "&&" v2 v3)]}
                            <|> return []




-- <Logical Or Expression> ::= <Logical And Expression>
--                           | <Logical Or Expression> '||' <Logical And Expression>
logicalOrExpression :: Parser [JSNode]
logicalOrExpression =  do{ v1 <- logicalAndExpression; v2 <- rest;
                           return (v1++v2)}
                       where
                         rest =
                                  (do{ rOp "||"; v2 <- logicalAndExpression; v3 <- rest;
                                   return [(JSExpressionBinary "||" v2 v3)]})
                            <|> return []


-- <Conditional Expression> ::= <Logical Or Expression> 
--                            | <Logical Or Expression> '?' <Assignment Expression> ':' <Assignment Expression>
conditionalExpression :: Parser [JSNode]
conditionalExpression = do{ v1 <- logicalOrExpression;
                            do {
                                 do{ rOp "?"; v2 <- assignmentExpression; rOp ":"; v3 <- assignmentExpression;
                                     return [(JSExpressionTernary v1 v2 v3)]}
                             <|> return v1
                               }
                            }


-- <Assignment Expression> ::= <Conditional Expression>
--                           | <Left Hand Side Expression> <Assignment Operator> <Assignment Expression> 
assignmentExpression :: Parser [JSNode]
assignmentExpression = try (do {v1 <- assignmentStart; v2 <- assignmentExpression; -- FIXME unintuitive, but tests pass after removing this try
                           return [(JSElement "assignmentExpression" (v1++v2))]})
                    <|> conditionalExpression
                       
assignmentStart :: Parser [JSNode]
assignmentStart = do {v1 <- leftHandSideExpression; v2 <- assignmentOperator; 
                           return (v1++[v2])}

-- <Assignment Operator> ::= '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
-- TODO : unwind this lot into some sort of deterministic processing, DFA style.
assignmentOperator :: Parser JSNode
assignmentOperator = rOp' "=" <|> rOp' "*=" <|> rOp' "/=" <|> rOp' "%=" <|> rOp' "+=" <|> rOp' "-="
                 <|> rOp' "<<=" <|> rOp' ">>=" <|> rOp' ">>>=" <|> rOp' "&=" <|> rOp' "^=" <|> rOp' "|="
                     
rOp' :: String -> Parser JSNode
rOp' x = do{ rOp x; return $ JSOperator x}

-- <Expression> ::= <Assignment Expression>
--                | <Expression> ',' <Assignment Expression>
expression :: Parser JSNode
expression = do{ val <- sepBy1 assignmentExpression (rOp ",");
                 return (JSExpression (flattenExpression val))}


flattenExpression :: [[JSNode]] -> [JSNode]
flattenExpression val = flatten $ intersperse litComma val
                        where
                          litComma :: [JSNode]
                          litComma = [(JSLiteral ",")]

-- <Statement> ::= <Block>
--               | <Variable Statement>
--               | <Empty Statement>
--               | <If Statement>
--               | <If Else Statement>
--               | <Iteration Statement>
--               | <Continue Statement>
--               | <Break Statement>
--               | <Return Statement>
--               | <With Statement>
--               | <Labelled Statement>
--               | <Switch Statement>
--               | <Throw Statement>
--               | <Try Statement>
--               | <Expression> 
statement :: Parser JSNode
statement = statementBlock
        <|> labelledStatement
        <|> expression 
        <|> variableStatement
        <|> emptyStatement 
        <|> ifElseStatement
        <|> ifStatement
        <|> iterationStatement
        <|> continueStatement 
        <|> breakStatement    
        <|> returnStatement     
        <|> withStatement
        <|> switchStatement
        <|> throwStatement 
        <|> tryStatement
        <?> "statement"


statementBlock :: Parser JSNode
statementBlock = do {v1 <- statementBlock'; return (if (v1 == []) then (JSLiteral ";") else (head v1))}

-- <Block > ::= '{' '}'
--            | '{' <Statement List> '}'
statementBlock' :: Parser [JSNode]
statementBlock' = (do
    _ <- P.lexeme $ char '{'
    (P.lexeme (char '}') >> return []) <|> (do
        val <- statementList
        _ <- P.lexeme $ char '}'
        return $
            if val == JSStatementList [JSLiteral ";"]
                then []
                else [JSBlock val])
    ) <?> "statementBlock"

-- <Block > ::= '{' '}'
--            | '{' <Statement List> '}'
block :: Parser JSNode
block = (do
    _ <- P.lexeme $ char '{'
    x <- (P.lexeme (char '}') >> return (JSStatementList [])) <|> (do
        val <- statementList
        _ <- P.lexeme $ char '}'
        return val)
    return $ JSBlock x) <?> "block"

-- <Statement List> ::= <Statement>
--                    | <Statement List> <Statement>
statementList :: Parser JSNode
statementList = do {v1 <- many1 statement;
                    return (JSStatementList v1)}


-- <Variable Statement> ::= var <Variable Declaration List> ';'
-- Note: Mozilla introduced const declarations, not part of official spec
variableStatement :: Parser JSNode
variableStatement = do {P.reserved "var"; val <- variableDeclarationList;
                        return (JSVariables "var" val)}
                <|> do {P.reserved "const"; val <- variableDeclarationList;
                        return (JSVariables "const" val)}

                    
-- <Variable Declaration List> ::= <Variable Declaration>
--                               | <Variable Declaration List> ',' <Variable Declaration>
variableDeclarationList :: Parser [JSNode]
variableDeclarationList = do{ val <- sepBy1 variableDeclaration (rOp ","); 
                              return val }


-- <Variable Declaration> ::= Identifier
--                          | Identifier <Initializer>
variableDeclaration :: Parser JSNode
variableDeclaration = do{ v1 <- identifier; 
                          do {
                            do {v2 <- initializer; 
                                return (JSVarDecl v1 v2)}
                            <|> return (JSVarDecl v1 [])
                               }
                          }

-- <Initializer> ::= '=' <Assignment Expression>
initializer :: Parser [JSNode]
initializer = do {rOp "="; val <- assignmentExpression; 
                  return val}

-- <Empty Statement> ::= ';'
emptyStatement :: Parser JSNode
--emptyStatement = do { v1 <- autoSemi'; return (JSEmpty v1)}
emptyStatement = do { v1 <- autoSemi'; return v1}


-- <If Statement> ::= 'if' '(' <Expression> ')' <Statement> 
ifStatement :: Parser JSNode
ifStatement = do{ P.reserved "if"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement;
                  return (JSIf v1 v2) }

-- <If Else Statement> ::= 'if' '(' <Expression> ')' <Statement> 'else' <Statement>
ifElseStatement :: Parser JSNode
ifElseStatement = do{ try $ P.reserved "if"; rOp "("; v1 <- expression; rOp ")"; v2 <- statementSemi; P.reserved "else"; v3 <- statement ; -- FIXME can this try be removed?
                      return (JSIfElse v1 v2 v3) }

statementSemi :: Parser JSNode
statementSemi = do { v1 <- statement;
                     do { 
                            do { rOp ";"; return (JSBlock (JSStatementList [v1]))} 
                        <|> return v1
                        }
                     }


-- <Iteration Statement> ::= 'do' <Statement> 'while' '(' <Expression> ')' ';'
--                         | 'while' '(' <Expression> ')' <Statement> 
--                         | 'for' '(' <Expression> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration List> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' <Left Hand Side Expression> in <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration> in <Expression> ')' <Statement> 
iterationStatement :: Parser JSNode
iterationStatement = do{ P.reserved "do"; v1 <- statement; P.reserved "while"; rOp "("; v2 <- expression; rOp ")"; v3 <- autoSemi ; 
                         return (JSDoWhile v1 v2 v3)}
                 <|> do{ P.reserved "while"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement; 
                         return (JSWhile v1 v2)}
                     
                 <|> do{ P.reserved "for"; rOp "("; 
                         do {
                           do{ P.reserved "var"; v1 <- variableDeclaration; 
                             do {
                               do { rOp ","; v1' <- variableDeclarationList; rOp ";"; v2 <- optionalExpression ";"; 
                                    v3 <- optionalExpression ")"; v4 <- statement; 
                                    return (JSForVar (v1:v1') v2 v3 v4)}
                           <|> do { rOp ";"; v2 <- optionalExpression ";"; 
                                    v3 <- optionalExpression ")"; v4 <- statement; 
                                    return (JSForVar [v1] v2 v3 v4)}
                           <|> do { P.reserved "in"; v2 <- expression; rOp ")"; v3 <- statement; 
                                    return (JSForVarIn v1 v2 v3)}
                                }
                             }
                         <|> try(do{ v1 <- leftHandSideExpression; P.reserved "in"; v2 <- expression; rOp ")"; 
                                     v3 <- statement; 
                                     return (JSForIn v1 v2 v3)})
                         <|> do { v1 <- optionalExpression ";"; v2 <- optionalExpression ";"; 
                                  v3 <- optionalExpression ")"; v4 <- statement; 
                                  return (JSFor v1 v2 v3 v4)}
                           }
                       }
                 <?> "iterationStatement"    
                     
optionalExpression :: [Char] -> Parser [JSNode]
optionalExpression s = do { rOp s; 
                            return []}
                   <|> do { v1 <- expression; rOp s ;
                            return [v1]}

-- <Continue Statement> ::= 'continue' ';'
--                        | 'continue' Identifier ';'
continueStatement :: Parser JSNode
continueStatement = do {P.reserved "continue"; v1 <- autoSemi; 
                        return (JSContinue [v1])}
                <|> do {P.reserved "continue"; v1 <- identifier; v2 <- autoSemi; 
                        return (JSContinue [v1,v2])}


-- <Break Statement> ::= 'break' ';'
--                        | 'break' Identifier ';'
breakStatement :: Parser JSNode
breakStatement = do {P.reserved "break"; 
                     do {
                          do {v1 <- identifier; v2 <- autoSemi; 
                              return (JSBreak [v1] [v2])}
                     <|>  do {v1 <- autoSemi; 
                              return (if (v1 == JSLiteral "") then (JSBreak [] []) else (JSBreak [] [v1]))}
                        }
                     }


-- <Return Statement> ::= 'return' ';'
--                        | 'return' <Expression> ';'
returnStatement :: Parser JSNode
returnStatement = do {P.reserved "return"; 
                      do{
                            do {v1 <- expression; v2 <- autoSemi; 
                                return (JSReturn [v1,v2])}
                        <|> do {v1 <- autoSemi; return (JSReturn [v1])}
                        
                        }
                      }


-- <With Statement> ::= 'with' '(' <Expression> ')' <Statement> ';'
withStatement :: Parser JSNode
withStatement = do{ P.reserved "with"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement; v3 <- autoSemi; 
                    return (JSWith v1 [v2,v3])}


-- <Switch Statement> ::= 'switch' '(' <Expression> ')' <Case Block>  
switchStatement :: Parser JSNode
switchStatement = do{ P.reserved "switch"; rOp "("; v1 <- expression; rOp ")"; v2 <- caseBlock;
                      return (JSSwitch v1 v2)}

-- <Case Block> ::= '{' '}'
--                | '{' <Case Clauses> '}'
--                | '{' <Case Clauses> <Default Clause> '}'
--                | '{' <Case Clauses> <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> '}'
-- TODO: get rid of the try clauses by unwinding this
caseBlock :: Parser [JSNode]
caseBlock = do
    _ <- P.lexeme $ char '{'
    a <- caseClauses <|> return []
    b <- fmap return defaultClause <|> return []
    c <- caseClauses <|> return []
    _ <- P.lexeme $ char '}'
    return $ concat [a, b, c]

-- <Case Clauses> ::= <Case Clause>
--                  | <Case Clauses> <Case Clause>
caseClauses :: Parser [JSNode]
caseClauses = do{ val <- many1 caseClause;
                  return val}

-- <Case Clause> ::= 'case' <Expression> ':' <Statement List>
--                 | 'case' <Expression> ':'
caseClause :: Parser JSNode
caseClause = do { P.reserved "case"; v1 <- expression; rOp ":"; 
                  do {
                       do { v2 <- statementList;
                            return (JSCase v1 v2)}
                   <|> return (JSCase v1 (JSStatementList []))
                     }
                  }

-- <Default Clause> ::= 'default' ':' 
--                    | 'default' ':' <Statement List>
defaultClause :: Parser JSNode
defaultClause = do{ P.reserved "default"; rOp ":"; v1 <- statementList;
                    return (JSDefault v1)}
            <|> do{ P.reserved "default"; rOp ":"; 
                    return (JSDefault (JSStatementList []))}

-- <Labelled Statement> ::= Identifier ':' <Statement> 
labelledStatement :: Parser JSNode
labelledStatement = do { v1 <- try identifier; rOp ":"; v2 <- statement; -- FIXME can this try be removed?
                         return (JSLabelled v1 v2)}

-- <Throw Statement> ::= 'throw' <Expression>
throwStatement :: Parser JSNode
throwStatement = do{ P.reserved "throw"; val <- expression;
                     return (JSThrow val)}

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
-- <Try Statement> ::= 'try' <Block> <Catch>
--                   | 'try' <Block> <Finally>
--                   | 'try' <Block> <Catch> <Finally>
tryStatement :: Parser JSNode
tryStatement = do{ P.reserved "try"; v1 <- block; 
                   do {
                     do { v2 <- many1 catch;
                        do { v3 <- finally;
                             return (JSTry v1 (v2++[v3]))}
                        <|> return (JSTry v1 v2)
                        }
                 <|> do{ v2 <- finally;
                         return (JSTry v1 [v2])}
                     }
                 }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--   becomes
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--           | 'catch' '(' Identifier 'if' Condition ')' <Block>
catch :: Parser JSNode
catch = do{ P.reserved "catch"; rOp "("; v1 <- identifier; 
            do {
                  do { rOp ")"; v3 <- block;
                       return (JSCatch v1 [] v3)}
              <|> do { P.reserved "if"; v2 <- conditionalExpression; rOp ")"; v3 <- block;
                       return (JSCatch v1 v2 v3)}
                  }
            }

-- <Finally> ::= 'finally' <Block>
finally :: Parser JSNode
finally = do{ P.reserved "finally"; v1 <- block;
            return (JSFinally v1)}

-- <Function Declaration> ::= 'function' Identifier '(' <Formal Parameter List> ')' '{' <Function Body> '}'
--                          | 'function' Identifier '(' ')' '{' <Function Body> '}'
functionDeclaration :: Parser JSNode
functionDeclaration = do {P.reserved "function"; v1 <- identifier; rOp "("; v2 <- formalParameterList; rOp ")"; 
                          v3 <- functionBody; 
                          return (JSFunction v1 v2 v3) } 
                  <?> "functionDeclaration"


-- <Function Expression> ::= 'function' '(' ')' '{' <Function Body> '}'
---                        | 'function' '(' <Formal Parameter List> ')' '{' <Function Body> '}'
functionExpression :: Parser JSNode
functionExpression = do{ P.reserved "function"; rOp "("; 
                         do {
                           do { rOp ")"; v2 <- functionBody; 
                                return (JSFunctionExpression [] v2)}
                           <|> do {v1 <- formalParameterList; rOp ")"; v2 <- functionBody; 
                                   return (JSFunctionExpression v1 v2)}
                           }
                         }

-- <Formal Parameter List> ::= Identifier
--                           | <Formal Parameter List> ',' Identifier
formalParameterList :: Parser [JSNode]
formalParameterList = sepBy identifier (rOp ",")

-- <Function Body> ::= '{' <Source Elements> '}'
--                   | '{' '}'
functionBody :: Parser JSNode
functionBody = do{ rOp "{"; 
                 do {         
                      do{ rOp "}";
                       return (JSFunctionBody []) }
                  <|> do{ v1 <- sourceElements; rOp "}";
                          return (JSFunctionBody [v1]) }
                      }
                 }
           <?> "functionBody"    

-- <Program> ::= <Source Elements>
program :: Parser JSNode
program = do {P.whiteSpace; val <- sourceElementsTop; eof; 
              return val}


-- <Source Elements> ::= <Source Element>
--                     | <Source Elements>  <Source Element>
sourceElements :: Parser JSNode
sourceElements = do{ val <- many1 sourceElement;
                     return (JSSourceElements val)}
                 
sourceElementsTop :: Parser JSNode
sourceElementsTop = do{ val <- many1 sourceElement;
                        return (JSSourceElementsTop val)}


-- <Source Element> ::= <Statement>
--                    | <Function Declaration>
sourceElement :: Parser JSNode
sourceElement = functionDeclaration
            <|> statement
            <?> "sourceElement"

                       
                       
-- ---------------------------------------------------------------
-- Testing

-- ---------------------------------------------------------------------

flatten :: [[a]] -> [a]
flatten xs = foldl' (++) [] xs

-- ---------------------------------------------------------------------

main :: IO ()
main =
  do args <- getArgs
     x <- LB.readFile (args !! 0)
     putStrLn (show $ doParse program x)            

    
-- ---------------------------------------------------------------------     
          
readJs :: LB.ByteString -> JSNode
readJs input = case doParse program input of
    Fail _unparsed contexts err -> error("Parse failed" ++ show(contexts) ++ ":" ++ show err)
    -- Partial _f -> error("Unexpected partial")
    Done _unparsed val -> val

-- ---------------------------------------------------------------------     
          
{-
readJsm :: (Monad m) => B.ByteString -> m JSNode
readJsm input = case eitherResult $ doParse program input of
    Left msg  -> fail ("Parse failed:" ++ msg)
    Right val -> return val
-}

readJsm :: LB.ByteString -> Either String JSNode
readJsm input = eitherResult $ doParse program input 

-- ---------------------------------------------------------------------
    
{-                
_doParse' :: Parser a -> String -> a
_doParse' p input = case parse (p' p) (LB.fromChunks [E.encodeUtf8 $ T.pack input]) of
    Fail _unparsed contexts err -> error("Parse failed" ++ show(contexts) ++ ":" ++ show err)
    Partial _f -> error("Unexpected partial")
    Done _unparsed val -> val
-}

{-
doParseStrict :: Parser r -> B.ByteString -> Result r
doParseStrict p input = {-maybeResult $-} feed (parse (p' p) input) LB.empty
-}                       

doParse :: Parser r -> LB.ByteString -> Result r
doParse p input = parse (p' p) input

parseString :: Parser r -> String -> Result r
parseString p input = doParse p (LB.fromChunks [E.encodeUtf8 $ T.pack input])

-- ---------------------------------------------------------------------
    
p' :: Parser b -> Parser b
p' p = do {val <- p; eof; return val}

-- ---------------------------------------------------------------------

_showFile :: FilePath -> IO String
_showFile filename =
  do 
     x <- readFile (filename)
     return $ (show x)

-- ---------------------------------------------------------------------

parseFile :: FilePath -> IO JSNode
parseFile filename =
  do 
     x <- LB.readFile (filename)
     return $ (readJs x)

-- EOF
