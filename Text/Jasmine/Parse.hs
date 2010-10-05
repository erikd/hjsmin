{-# LANGUAGE DeriveDataTypeable #-}
module Text.Jasmine.Parse
    (       
    --  parseScript
      readJs
    , JasmineSettings (..)
    , defaultJasmineSettings
    ) where

-- ---------------------------------------------------------------------

import Prelude hiding (catch)
import System.Environment
import Control.Applicative (Applicative (..))
import Control.Monad
--import Control.Arrow
import Data.Data
--import Data.List (intercalate)
import Text.ParserCombinators.Parsec hiding (Line)

-- import Text.Parsec
--import qualified Text.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
--import Text.Parsec.Language (haskellDef)


-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------

data JSNode = JSNode JSType JSValue [JSNode] [JSFunDecl] [JSVarDecl]
    deriving (Show, Eq, Read, Data, Typeable)

data JSType = JS_SCRIPT | JS_BLOCK | JS_LABEL | JS_FOR_IN | JS_CALL | JS_NEW_WITH_ARGS
            | JS_INDEX | JS_ARRAY_INIT | JS_OBJECT_INIT | JS_PROPERTY_INIT | JS_GETTER
            | JS_SETTER | JS_GROUP | JS_LIST
            -- Lower case is AZ additions to understand the thing
            | JS_value 
    deriving (Show, Eq, Read, Data, Typeable)

data JSValue = NoValue | JSValue String
    deriving (Show, Eq, Read, Data, Typeable)

data JSFunDecl = JSFunDecl String
    deriving (Show, Eq, Read, Data, Typeable)

data JSVarDecl = JSVarDecl String
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
-- | A lexer for the javascript language.

javascript :: P.TokenParser st
javascript  = P.makeTokenParser javascriptDef

-- | The language definition for the language Javascript

javascriptDef :: LanguageDef st
javascriptDef = javaStyle
		{ reservedNames = [ 
                     "break",
                     "case", "catch", "const", "continue",
                     "debugger", "default", "delete", "do",
                     "else", "enum",
                     "false", "finally", "for", "function",
                     "if", "in", "instanceof",
                     "new", "null",
                     "return",
                     "switch",
                     "this", "throw", "true", "try", "typeof",
                     "var", "void",
                     "while", "with"
                     ]
                -- TODO: make the following constants, so the parser defn is simpler. e,g op_COMMA = ","
                , reservedOpNames= [
                     ";"	, -- "SEMICOLON",
                     ","	, -- "COMMA",
                     "?"	, -- "HOOK",
                     ":"	, -- "COLON",
                     "||"	, -- "OR",
                     "&&"	, -- "AND",
                     "|"	, -- "BITWISE_OR",
                     "^"	, -- "BITWISE_XOR",
                     "&"	, -- "BITWISE_AND",
                     "==="	, -- "STRICT_EQ",
                     "=="	, -- "EQ",
                     "="	, -- "ASSIGN",
                     "!=="	, -- "STRICT_NE",
                     "!="	, -- "NE",
                     "<<"	, -- "LSH",
                     "<="	, -- "LE",
                     "<"	, -- "LT",
                     ">>>"	, -- "URSH",
                     ">>"	, -- "RSH",
                     ">="	, -- "GE",
                     ">"	, -- "GT",
                     "++"	, -- "INCREMENT",
                     "--"	, -- "DECREMENT",
                     "+"	, -- "PLUS",
                     "-"	, -- "MINUS",
                     "*"	, -- "MUL",
                     "/"	, -- "DIV",
                     "%"	, -- "MOD",
                     "!"	, -- "NOT",
                     "~"	, -- "BITWISE_NOT",
                     "."	, -- "DOT",
                     "["	, -- "LEFT_BRACKET",
                     "]"	, -- "RIGHT_BRACKET",
                     "{"	, -- "LEFT_CURLY",
                     "}"	, -- "RIGHT_CURLY",
                     "("	, -- "LEFT_PAREN",
                     ")"	, -- "RIGHT_PAREN",
                     "@*/"	  -- "CONDCOMMENT_END"
                ]
                                  
                , caseSensitive  = True
		}
        
 -- The lexer
lexer :: P.TokenParser st
lexer       = P.makeTokenParser javascriptDef
      
parens :: CharParser st a -> CharParser st a
parens      = P.parens lexer

braces :: CharParser st a -> CharParser st a
braces      = P.braces lexer

identifier :: CharParser st JSNode
identifier  = do{ val <- P.identifier lexer;
                  return (JSNode JS_value (JSValue (show val)) [] [] [])}

reserved :: String -> CharParser st ()
reserved    = P.reserved lexer

rOp x = do{ val <- P.reservedOp lexer x;
           return (JSNode JS_value (JSValue (show val)) [] [] [])}
       
-- ---------------------------------------------------------------------
-- The parser, based on the gold parser for Javascript
-- http://www.devincook.com/GOLDParser/grammars/files/JavaScript.zip
 
-- Filling the gaps       
stringLiteral :: CharParser st JSNode
stringLiteral = do{ val <- P.stringLiteral lexer;
                   return (JSNode JS_value (JSValue "null") [] [] [])}


decimalLiteral :: CharParser st Integer
decimalLiteral = P.decimal lexer

hexIntegerLiteral :: CharParser st Integer
hexIntegerLiteral = P.hexadecimal lexer -- TODO: check prefix etc for Javascript convention

-- {String Chars1} = {Printable} + {HT} - ["\] 
-- {RegExp Chars} = {Letter}+{Digit}+['^']+['$']+['*']+['+']+['?']+['{']+['}']+['|']+['-']+['.']+[',']+['#']+['[']+[']']+['_']+['<']+['>']
-- {Non Terminator} = {String Chars1} - {CR} - {LF}
-- RegExp         = '/' ({RegExp Chars} | '\' {Non Terminator})+ '/' ( 'g' | 'i' | 'm' )*

-- TODO: do this properly, it is late now, want to move on
regExp :: GenParser Char st JSNode
regExp = do { rOp "/"; v1 <- many (noneOf "/"); rOp "/"; v2 <- optional (oneOf "gim");
              return (JSNode JS_value (JSValue ("/" ++ v1 ++ "/" ++ (show(v2)))) [] [] [])}
       

-- <Literal> ::= <Null Literal>
--             | <Boolean Literal>
--             | <Numeric Literal>
--             | StringLiteral
literal :: GenParser Char st JSNode
literal = nullLiteral
      <|> booleanLiteral
      <|> numericLiteral
      <|> stringLiteral

-- <Null Literal>    ::= null
nullLiteral :: GenParser Char st JSNode
nullLiteral = do { reserved "null"; 
                   -- return [""]} 
                   return (JSNode JS_value (JSValue "null") [] [] [])}

-- <Boolean Literal> ::= 'true'
--                     | 'false'
booleanLiteral :: GenParser Char st JSNode
booleanLiteral = do{ reserved "true" ; 
                     -- return [""]} 
                     return (JSNode JS_value (JSValue "true") [] [] [])}
             <|> do{ reserved "false"; 
                     -- return [""]} 
                     return (JSNode JS_value (JSValue "false") [] [] [])}

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
numericLiteral :: GenParser Char st JSNode
numericLiteral = do {val <- decimalLiteral; 
                     -- return [""]} -- TODO: proper return
                     return (JSNode JS_value (JSValue (show val)) [] [] [])}
             <|> do {val <- hexIntegerLiteral; 
                     -- return [""]} -- TODO: proper return
                     return (JSNode JS_value (JSValue (show val)) [] [] [])}


-- <Regular Expression Literal> ::= RegExp 
regularExpressionLiteral :: GenParser Char st JSNode
regularExpressionLiteral = regExp


-- <Primary Expression> ::= 'this'
--                        | Identifier
--                        | <Literal> 
--                        | <Array Literal>
--                        | <Object Literal>
--                        | '(' <Expression> ')'
--                        | <Regular Expression Literal>
primaryExpression :: GenParser Char st JSNode
primaryExpression = do {reserved "this"; 
                        -- return [""]} 
                        return (JSNode JS_value (JSValue "this") [] [] [])}
                <|> identifier
                <|> literal
                <|> arrayLiteral
                <|> objectLiteral
                <|> do{ rOp "("; val <- expression; rOp ")"; 
                        -- return [""]} 
                        return (JSNode JS_value (JSValue "()") [val] [] [])}
                <|> regularExpressionLiteral


-- <Array Literal> ::= '[' ']'
--                   | '[' <Elision> ']'
--                   | '[' <Element List> ']'
--                   | '[' <Element List> ',' <Elision> ']'
arrayLiteral :: GenParser Char st JSNode
arrayLiteral = do {rOp "["; rOp "]"; 
                   return (JSNode JS_value (JSValue "[]") [] [] [])}
           <|> do {rOp "["; val <- elision; rOp "]"; 
                   return (JSNode JS_value (JSValue "[]") [val] [] [])}
           <|> do {rOp "["; val <- elementList; rOp "]"; 
                   return (JSNode JS_value (JSValue "[]") [val] [] [])}
           <|> do {rOp "["; v1 <- elementList; rOp ","; v2 <- elision; rOp "]"; 
                   return (JSNode JS_value (JSValue "[]") [v1,v2] [] [])}


-- <Elision> ::= ','
--             | <Elision> ','
elision :: GenParser Char st JSNode
elision = do{ val <- many1 (rOp ",");
              return (JSNode JS_value (JSValue (show val)) [] [] [])}


-- <Element List> ::= <Elision> <Assignment Expression>
--                  | <Element List> ',' <Elision>  <Assignment Expression>
--                  | <Element List> ',' <Assignment Expression>
--                  | <Assignment Expression>
elementList :: GenParser Char st JSNode
elementList = do{ v1 <- elision; v2 <- assignmentExpression;
              return (JSNode JS_value (JSValue "elementList") [v1,v2] [] [])}
          <|> do{ v1 <- elementList; rOp ","; v2 <- elision; v3 <- assignmentExpression;
              return (JSNode JS_value (JSValue "elementList") [v1,v2,v3] [] [])}
          <|> do{ v1 <- elementList; rOp ","; v2 <- assignmentExpression;
              return (JSNode JS_value (JSValue "elementList") [v1,v2] [] [])}
          <|> assignmentExpression


-- <Object Literal> ::= '{' <Property Name and Value List> '}'
objectLiteral :: GenParser Char st JSNode
objectLiteral = do{ rOp "{"; val <- propertyNameandValueList; rOp "}"; 
                    --return [""]} -- TODO: proper return
                   return (JSNode JS_value (JSValue "{}") [val] [] [])}

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>
propertyNameandValueList :: GenParser Char st JSNode
propertyNameandValueList = do{ val <- sepBy1 propertyNameandValueList (rOp ",");
                           return (JSNode JS_value (JSValue "propertyNameandValueList") val [] [])}
                           
propertyNameandValue :: GenParser Char st JSNode
propertyNameandValue = do{ v1 <- propertyName; rOp ":"; v2 <- assignmentExpression;
                           return (JSNode JS_value (JSValue "propertyNameandValue") [v1,v2] [] [])}

-- <Property Name> ::= Identifier
--                   | StringLiteral
--                   | <Numeric Literal>
propertyName :: GenParser Char st JSNode
propertyName = identifier
           <|> stringLiteral
           <|> numericLiteral


-- <Member Expression > ::= <Primary Expression>
--                        | <Function Expression>
--                        | <Member Expression> '[' <Expression> ']'
--                        | <Member Expression> '.' Identifier
--                        | 'new' <Member Expression> <Arguments>
memberExpression :: GenParser Char st JSNode
memberExpression = primaryExpression
                <|> functionExpression
                <|> do{ v1 <- memberExpression; rOp "["; v2 <- expression; rOp "]"; 
                        return (JSNode JS_value (JSValue "memberExpression[]") [v1,v2] [] [])}
                <|> do{ v1 <- memberExpression; rOp "."; v2 <- identifier ; 
                        return (JSNode JS_value (JSValue "memberExpression.") [v1,v2] [] [])}
                <|> do{ reserved "new"; v1 <- memberExpression; v2 <- arguments; 
                        return (JSNode JS_value (JSValue "memberExpression.new") [v1,v2] [] [])}


-- <New Expression> ::= <Member Expression>
--                    | new <New Expression>
newExpression :: GenParser Char st JSNode
newExpression = memberExpression
           <|> do{ reserved "new"; val <- newExpression;
                   return (JSNode JS_value (JSValue "new") [val] [] [])}

-- <Call Expression> ::= <Member Expression> <Arguments>
--                     | <Call Expression> <Arguments> 
--                     | <Call Expression> '[' <Expression> ']'
--                     | <Call Expression> '.' Identifier
callExpression :: GenParser Char st JSNode
callExpression = do{ v1 <- memberExpression; v2 <- arguments; 
                     return (JSNode JS_value (JSValue "callExpression") [v1,v2] [] [])}
             <|> do{ v1 <- callExpression; v2 <- arguments ; 
                     return (JSNode JS_value (JSValue "callExpression") [v1,v2] [] [])}
             <|> do{ v1 <- callExpression; rOp "["; v2 <- expression; rOp "]"; 
                     return (JSNode JS_value (JSValue "callExpression[]") [v1,v2] [] [])}
             <|> do{ v1 <- callExpression; rOp "."; v2 <- identifier; 
                     return (JSNode JS_value (JSValue "callExpression.") [v1,v2] [] [])}


-- <Arguments> ::= '(' ')'
--               | '(' <Argument List> ')'
arguments :: GenParser Char st JSNode
arguments = do{ rOp "("; v1 <- optional argumentList; rOp ")";
                     return (JSNode JS_value (JSValue "arguments") [v1] [] [])}

-- <Argument List> ::= <Assignment Expression>
--                   | <Argument List> ',' <Assignment Expression>
argumentList = do{ vals <- sepBy1 assignmentExpression (rOp ",");
                     -- return (JSNode JS_value (JSValue "argumentList") [vals] [] [])} -- TODO: restore this return value
                     return (JSNode JS_value (JSValue "argumentList") [] [] [])}
                   



-- <Left Hand Side Expression> ::= <New Expression> 
--                               | <Call Expression>
leftHandSideExpression :: GenParser Char st JSNode
leftHandSideExpression = newExpression
                     <|> callExpression


-- <Postfix Expression> ::= <Left Hand Side Expression>
--                        | <Postfix Expression> '++'
--                        | <Postfix Expression> '--'
postfixExpression :: GenParser Char st [[Char]]
postfixExpression = leftHandSideExpression
                <|> do{ postfixExpression; rOp "++"; return [""]} -- TODO: proper return
                <|> do{ postfixExpression; rOp "--"; return [""]} -- TODO: proper return


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
unaryExpression :: GenParser Char st [[Char]]
unaryExpression = postfixExpression
              <|> do{ reserved "delete"; unaryExpression}
              <|> do{ reserved "void"; unaryExpression}
              <|> do{ reserved "typeof"; unaryExpression}
              <|> do{ rOp "++"; unaryExpression}
              <|> do{ rOp "--"; unaryExpression}
              <|> do{ rOp "+"; unaryExpression}
              <|> do{ rOp "-"; unaryExpression}
              <|> do{ rOp "~"; unaryExpression}
              <|> do{ rOp "!"; unaryExpression}


-- <Multiplicative Expression> ::= <Unary Expression>
--                               | <Unary Expression> '*' <Multiplicative Expression> 
--                               | <Unary Expression> '/' <Multiplicative Expression>                               
--                               | <Unary Expression> '%' <Multiplicative Expression> 
multiplicativeExpression :: GenParser Char st [[Char]]
multiplicativeExpression = unaryExpression
                       <|> do{ unaryExpression; rOp "*"; multiplicativeExpression}
                       <|> do{ unaryExpression; rOp "/"; multiplicativeExpression}
                       <|> do{ unaryExpression; rOp "%"; multiplicativeExpression}


-- <Additive Expression> ::= <Additive Expression>'+'<Multiplicative Expression> 
--                         | <Additive Expression>'-'<Multiplicative Expression>  
--                         | <Multiplicative Expression>
additiveExpression :: GenParser Char st [[Char]]
additiveExpression = do {additiveExpression; rOp "+"; multiplicativeExpression}
                 <|> do {additiveExpression; rOp "-"; multiplicativeExpression}
                 <|> multiplicativeExpression

-- <Shift Expression> ::= <Shift Expression> '<<' <Additive Expression>
--                      | <Shift Expression> '>>' <Additive Expression>
--                      | <Shift Expression> '>>>' <Additive Expression>
--                      | <Additive Expression>
shiftExpression :: GenParser Char st [[Char]]
shiftExpression = do{ shiftExpression; rOp "<<"; additiveExpression}
              <|> do{ shiftExpression; rOp ">>"; additiveExpression}
              <|> do{ shiftExpression; rOp ">>>"; additiveExpression}
              <|> additiveExpression    

-- <Relational Expression>::= <Shift Expression> 
--                          | <Relational Expression> '<' <Shift Expression> 
--                          | <Relational Expression> '>' <Shift Expression> 
--                          | <Relational Expression> '<=' <Shift Expression> 
--                          | <Relational Expression> '>=' <Shift Expression> 
--                          | <Relational Expression> 'instanceof' <Shift Expression> 
relationalExpression :: GenParser Char st [[Char]]
relationalExpression = shiftExpression
                  <|> do{ relationalExpression; rOp "<"; shiftExpression}
                  <|> do{ relationalExpression; rOp ">"; shiftExpression}
                  <|> do{ relationalExpression; rOp "<="; shiftExpression}
                  <|> do{ relationalExpression; rOp ">="; shiftExpression}
                  <|> do{ relationalExpression; reserved "instanceof"; shiftExpression}


-- <Equality Expression> ::= <Relational Expression>
--                         | <Equality Expression> '==' <Relational Expression>
--                         | <Equality Expression> '!=' <Relational Expression>
--                         | <Equality Expression> '===' <Relational Expression>
--                         | <Equality Expression> '!==' <Relational Expression>
equalityExpression :: GenParser Char st [[Char]]
equalityExpression = relationalExpression
                <|> do{ equalityExpression; rOp "=="; relationalExpression}
                <|> do{ equalityExpression; rOp "!="; relationalExpression}
                <|> do{ equalityExpression; rOp "==="; relationalExpression}
                <|> do{ equalityExpression; rOp "!=="; relationalExpression}


-- <Bitwise And Expression> ::= <Equality Expression>
--                            | <Bitwise And Expression> '&' <Equality Expression>
bitwiseAndExpression :: GenParser Char st [[Char]]
bitwiseAndExpression = equalityExpression
                 <|> do{ bitwiseAndExpression; rOp "&"; equalityExpression}



-- <Bitwise XOr Expression> ::= <Bitwise And Expression>
--                            | <Bitwise XOr Expression> '^' <Bitwise And Expression>
bitwiseXOrExpression :: GenParser Char st [[Char]]
bitwiseXOrExpression = bitwiseAndExpression
                   <|> do { bitwiseXOrExpression; rOp "^"; bitwiseAndExpression}


-- <Bitwise Or Expression> ::= <Bitwise XOr Expression>
--                           | <Bitwise Or Expression> '|' <Bitwise XOr Expression>
bitwiseOrExpression :: GenParser Char st [[Char]]
bitwiseOrExpression = bitwiseXOrExpression
                  <|> do{ bitwiseOrExpression; rOp "|"; bitwiseXOrExpression}


-- <Logical And Expression> ::= <Bitwise Or Expression>
--                            | <Logical And Expression> '&&' <Bitwise Or Expression>
logicalAndExpression :: GenParser Char st [[Char]]
logicalAndExpression = bitwiseOrExpression
                      <|> do{ logicalAndExpression; rOp "&&"; bitwiseOrExpression}



-- <Logical Or Expression> ::= <Logical And Expression>
--                           | <Logical Or Expression> '||' <Logical And Expression>
logicalOrExpression :: GenParser Char st [[Char]]
logicalOrExpression =  logicalAndExpression
                 <|> do{ logicalOrExpression; rOp "||"; logicalAndExpression}


-- <Conditional Expression> ::= <Logical Or Expression> 
--                            | <Logical Or Expression> '?' <Assignment Expression> ':' <Assignment Expression>
conditionalExpression :: GenParser Char st JSNode
conditionalExpression = logicalOrExpression
                    <|> do{ v1 <- logicalOrExpression; rOp "?"; v2 <- assignmentExpression; rOp ":"; v3 <- assignmentExpression;
                           return (JSNode JS_value (JSValue "conditionalExpression.ternary") [v1,v2,v3] [] [])}


-- <Assignment Expression> ::= <Conditional Expression>
--                           | <Left Hand Side Expression> <Assignment Operator> <Assignment Expression> 
assignmentExpression :: GenParser Char st JSNode
assignmentExpression = conditionalExpression
                   <|> do {v1 <- leftHandSideExpression; v2 <- assignmentOperator; v3 <- assignmentExpression;
                           return (JSNode JS_value (JSValue "assignmentExpression") [v1,v2,v3] [] [])}


-- <Assignment Operator> ::= '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
assignmentOperator :: GenParser Char st JSNode
assignmentOperator = rOp "="   <|> rOp "*="   <|> rOp "/=" <|> rOp "%=" <|> rOp "+=" <|> rOp "-=" <|> rOp "<<=" 
                 <|> rOp ">>=" <|> rOp ">>>=" <|> rOp "&=" <|> rOp "^=" <|> rOp "|="

-- <Expression> ::= <Assignment Expression>
--                | <Expression> ',' <Assignment Expression>
expression = do{ val <- sepBy1 assignmentExpression (rOp ",");
                 return (JSNode JS_value (JSValue "expression") val [] [])}


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
statement :: GenParser Char st [[Char]]
statement = block
        <|> variableStatement
        <|> emptyStatement 
        <|> ifElseStatement
        <|> ifStatement
        <|> iterationStatement
        <|> continueStatement 
        <|> breakStatement    
        <|> returnStatement     
        <|> withStatement
        <|> labelledStatement
        <|> switchStatement
        <|> throwStatement 
        <|> tryStatement
        <|> expression 


-- <Block > ::= '{' '}'
--            | '{' <Statement List> '}'
block :: GenParser Char st [[Char]]
block = do {rOp "{"; val <- optional statementList; rOp "}"; 
            --return (JSNode JS_BLOCK NoValue val [] []) }
            return [""]} -- TODO: proper return


-- <Statement List> ::= <Statement>
--                    | <Statement List> <Statement>
statementList :: GenParser Char st [JSNode]
statementList = do{ val <- many1 statement;
                    return [JSNode JS_BLOCK NoValue {-val-}[] [] []] }

-- <Variable Statement> ::= var <Variable Declaration List> ';'
variableStatement :: GenParser Char st [[Char]]
variableStatement = do {reserved "var"; variableDeclarationList;}
-- <Variable Declaration List> ::= <Variable Declaration>
--                               | <Variable Declaration List> ',' <Variable Declaration>
variableDeclarationList :: GenParser Char st [[Char]]
variableDeclarationList = do{ sepBy1 variableDeclaration (rOp ","); return [""]} -- TODO: proper return


-- <Variable Declaration> ::= Identifier
--                          | Identifier <Initializer>
variableDeclaration :: GenParser Char st [[Char]]
variableDeclaration = do{ identifier; optional initializer; return [""]} -- TODO: proper return

-- <Initializer> ::= '=' <Assignment Expression>
initializer :: GenParser Char st [[Char]]
initializer = do {rOp "="; assignmentExpression; return [""]} -- TODO: proper return

-- <Empty Statement> ::= ';'
emptyStatement :: GenParser Char st [[Char]]
emptyStatement = do { rOp ";"; return [""]} -- TODO: fix return


-- <If Statement> ::= 'if' '(' <Expression> ')' <Statement> 
ifStatement :: GenParser Char st [[Char]]
ifStatement = do{ reserved "if"; rOp "("; expression; rOp ")"; statement }
-- <If Else Statement> ::= 'if' '(' <Expression> ')' <Statement> 'else' <Statement>
ifElseStatement :: GenParser Char st [[Char]]
ifElseStatement = do{ reserved "if"; rOp "("; expression; rOp ")"; statement; reserved "else"; statement }


-- <Iteration Statement> ::= 'do' <Statement> 'while' '(' <Expression> ')' ';'
--                         | 'while' '(' <Expression> ')' <Statement> 
--                         | 'for' '(' <Expression> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration List> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' <Left Hand Side Expression> in <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration> in <Expression> ')' <Statement> 
iterationStatement :: GenParser Char st [[Char]]
iterationStatement = do{ reserved "do"; statement; reserved "while"; rOp "("; expression; rOp ")"; rOp ";" ; return [""] } -- TODO: proper return
                 <|> do{ reserved "while"; rOp "("; expression; rOp ")"; statement; return [""]} -- TODO: proper return
                 <|> do{ reserved "for"; rOp "("; expression; rOp ";"; expression; rOp ";"; expression; rOp ")"; statement; return [""]} -- TODO: proper return
                 <|> do{ reserved "for"; rOp "("; reserved "var"; variableDeclarationList; rOp ";"; expression; rOp ";";expression; rOp ")"; statement; return [""]} -- TODO: proper return
                 <|> do{ reserved "for"; rOp "("; leftHandSideExpression; reserved "in"; expression; rOp ")"; statement; return [""]} -- TODO: proper return
                 <|> do{ reserved "for"; rOp "("; reserved "var"; variableDeclaration; reserved "in"; expression; rOp ")"; statement; return [""] } -- TODO: proper return



-- <Continue Statement> ::= 'continue' ';'
--                        | 'continue' Identifier ';'
continueStatement :: GenParser Char st [[Char]]
continueStatement = do {reserved "continue"; optional identifier; rOp ";"; return [""]} -- TODO: fix return


-- <Break Statement> ::= 'break' ';'
--                        | 'break' Identifier ';'
breakStatement :: GenParser Char st [[Char]]
breakStatement = do {reserved "break"; optional identifier; rOp ";"; return [""]} -- TODO: fix return


-- <Return Statement> ::= 'return' ';'
--                        | 'return' <Expression> ';'
returnStatement :: GenParser Char st [[Char]]
returnStatement = do {reserved "return"; optional expression; rOp ";"; return [""]} -- TODO: fix return


-- <With Statement> ::= 'with' '(' <Expression> ')' <Statement> ';'
withStatement :: GenParser Char st [[Char]]
withStatement = do{ reserved "with"; rOp "("; expression; rOp ")"; statement; rOp ";"; return [""]} -- TODO: proper return


-- <Switch Statement> ::= 'switch' '(' <Expression> ')' <Case Block>  
switchStatement :: GenParser Char st [[Char]]
switchStatement = do{ reserved "switch"; rOp "("; expression; rOp ")"; caseBlock}


-- <Case Block> ::= '{' '}'
--                | '{' <Case Clauses> '}'
--                | '{' <Case Clauses> <Default Clause> '}'
--                | '{' <Case Clauses> <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> '}'
caseBlock :: GenParser Char st [[Char]]
caseBlock = do{ rOp "{"; rOp "}"; return [""]} -- TODO: proper return
        <|> do{ rOp "{"; caseClauses; rOp "}"; return [""]} -- TODO: proper return
        <|> do{ rOp "{"; caseClauses; defaultClause; rOp "}"; return [""]} -- TODO: proper return
        <|> do{ rOp "{"; caseClauses; defaultClause; caseClauses; rOp "}"; return [""]} -- TODO: proper return
        <|> do{ rOp "{"; defaultClause; caseClauses; rOp "}"; return [""]} -- TODO: proper return
        <|> do{ rOp "{"; defaultClause; rOp "}"; return [""]} -- TODO: proper return


-- <Case Clauses> ::= <Case Clause>
--                  | <Case Clauses> <Case Clause>
caseClauses :: GenParser Char st [()]
caseClauses = many1 caseClause


-- <Case Clause> ::= 'case' <Expression> ':' <Statement List>
--                 | 'case' <Expression> ':'
caseClause :: GenParser Char st ()
caseClause = do { reserved "case"; expression; optional statementList }


-- <Default Clause> ::= 'default' ':' 
--                    | 'default' ':' <Statement List>
defaultClause :: GenParser Char st ()
defaultClause = do{ reserved "default"; rOp ":"; optional statementList}

-- <Labelled Statement> ::= Identifier ':' <Statement> 
labelledStatement :: GenParser Char st [[Char]]
labelledStatement = do { identifier; rOp ":"; statement }

-- <Throw Statement> ::= 'throw' <Expression>
throwStatement :: GenParser Char st [String]
throwStatement = do{ reserved "throw"; expression }

-- <Try Statement> ::= 'try' <Block> <Catch>
--                   | 'try' <Block> <Finally>
--                   | 'try' <Block> <Catch> <Finally>
tryStatement :: GenParser Char st [[Char]]
tryStatement = do{ reserved "try"; block; catch}
           <|> do{ reserved "try"; block; finally}
           <|> do{ reserved "try"; block; catch; finally}


-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
catch :: GenParser Char st [[Char]]
catch = do{ reserved "catch"; rOp "("; identifier; rOp ")"; block }

-- <Finally> ::= 'finally' <Block>
finally :: GenParser Char st [[Char]]
finally = do{ reserved "finally"; block}


-- <Function Declaration> ::= 'function' Identifier '(' <Formal Parameter List> ')' '{' <Function Body> '}'
--                          | 'function' Identifier '(' ')' '{' <Function Body> '}'
functionDeclaration :: GenParser Char st [[Char]]
functionDeclaration = try (do {reserved "function"; identifier; rOp "("; formalParameterList; rOp ")"; rOp "{"; functionBody; rOp "}"; return [""]}) -- TODO: fix return
                    <|>    do {reserved "function"; identifier; rOp "(";                      rOp ")"; rOp "{"; functionBody; rOp "}"; return [""]} -- TODO: proper return

-- <Function Expression> ::= 'function' '(' ')' '{' <Function Body> '}'
---                        | 'function' '(' <Formal Parameter List> ')' '{' <Function Body> '}'
functionExpression :: GenParser Char st JSNode
functionExpression = do{ reserved "function"; rOp "("; optional formalParameterList; rOp ")"; rOp "{"; functionBody; rOp "}"}

-- <Formal Parameter List> ::= Identifier
--                           | <Formal Parameter List> ',' Identifier
formalParameterList :: GenParser Char st [JSNode]
formalParameterList = sepBy1 identifier (rOp ",")

-- <Function Body> ::= <Source Elements>
--                   | 
functionBody :: GenParser Char st ()
functionBody = optional sourceElements

-- <Program> ::= <Source Elements>
program :: GenParser Char st [[[[Char]]]]
program = do {val <- many sourceElements; eof; return val}

-- <Source Elements> ::= <Source Element>
--                     | <Source Elements>  <Source Element>
sourceElements :: GenParser Char st [[[Char]]]
sourceElements = many1 sourceElement

-- <Source Element> ::= <Statement>
--                    | <Function Declaration>
sourceElement :: GenParser Char st [[Char]]
sourceElement = statement
             <|> functionDeclaration

-- ---------------------------------------------------------------
-- Testing

main :: IO ()
main = do args <- getArgs
          putStrLn (readJs (args !! 0))

-- ---------------------------------------------------------------------
          
readJs :: String -> String
readJs input = case parse program "js" input of
    Left err -> "No match: " ++ show err
    Right val -> "Parsed" ++ show(val)

symbolp :: Parser Char
symbolp = oneOf "!#$%&|*+-/:<=>?@^_~"

-- EOF
