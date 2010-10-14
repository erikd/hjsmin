{-# LANGUAGE DeriveDataTypeable #-}
module Text.Jasmine.Parse
    (       
    --  parseScript
      readJs
    , JasmineSettings (..)
    , defaultJasmineSettings
    , JSNode(..)  
    , JSType(..)  
    , JSValue(..)  
    -- For testing      
    , doParse
    , program  
    , functionDeclaration
    , identifier
    , statementList  
    , m  
    , main  
    ) where

-- ---------------------------------------------------------------------


-- import Text.Parsec
--import Control.Arrow
--import Text.Parsec.Language (haskellDef)
--import qualified Text.Parsec.Token as P
import Control.Applicative (Applicative (..))
import Control.Monad
import Data.Char
import Data.Data
import Data.List 
import Prelude hiding (catch)
import System.Environment
import Text.ParserCombinators.Parsec hiding (Line)
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P


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
              | JSElement String [JSNode]
              | JSFunction JSNode [JSNode]
              | JSFunctionBody [JSNode]
              | JSExpression [JSNode]
              | JSEmpty
              | JSStringLiteral Char String  
              | JSIdentifier String  
              | JSDecimal Integer   
              | JSOperator String  
    deriving (Show, Eq, Read, Data, Typeable)

data JSType = JS_SCRIPT | JS_BLOCK | JS_LABEL | JS_FOR_IN | JS_CALL | JS_NEW_WITH_ARGS
            | JS_INDEX | JS_ARRAY_INIT | JS_OBJECT_INIT | JS_PROPERTY_INIT | JS_GETTER
            | JS_SETTER | JS_GROUP | JS_LIST
            -- Lower case is AZ additions to understand the thing
            | JS_value 
    deriving (Show, Eq, Read, Data, Typeable)

data JSValue = NoValue | JSValue String | JSHexInteger Integer
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
               , opLetter = oneOf "!%&()*+,-./:;<=>?[]^{|}~"
               , opStart = opLetter javascriptDef 
                                  
                , caseSensitive  = True
		}
                
        
-- ---------------------------------------------------------------------
-- | A lexer for the javascript language.

lexer :: P.TokenParser st
lexer = P.makeTokenParser javascriptDef
      
identifier :: CharParser st JSNode
identifier  = do{ val <- P.identifier lexer;
                  return (JSIdentifier val)}

reserved :: String -> CharParser st ()
reserved    = P.reserved lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer


-- Do not use the lexer, it is greedy and consumes following symbols, e.g. "!"
rOp :: [Char] -> CharParser st ()
rOp []     = fail "trying to parse empty token"
rOp [x]    = do{ _ <- char x; optional whiteSpace; return () }
rOp (x:xs) = do{ _ <- char x; rOp xs;}
                 
-- ---------------------------------------------------------------------
-- The parser, based on the gold parser for Javascript
-- http://www.devincook.com/GOLDParser/grammars/files/JavaScript.zip
 
-- ------------------------------------------------------------
--Modified from HJS

stringLiteral :: GenParser Char st JSNode
stringLiteral = try( do { _ <- char '"'; val<- many stringCharDouble; _ <- char '"';
                     return (JSStringLiteral '"' val)})
            <|> do { _ <- char '\''; val<- many stringCharSingle; _ <- char '\'';
                     return (JSStringLiteral '\'' val)}

stringCharDouble :: CharParser st Char
stringCharDouble = satisfy (\c -> isPrint c && c /= '"')

stringCharSingle :: CharParser st Char
stringCharSingle = satisfy (\c -> isPrint c && c /= '\'')



-- ------------------------------------------------------------


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
regExp = do { rOp "/"; v1 <- many (noneOf "/"); rOp "/"; v2 <- optional (oneOf "gim"); -- TODO: remove optional
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
                     return (JSDecimal val)}
             <|> do {val <- hexIntegerLiteral; 
                     -- return [""]} -- TODO: proper return
                     return (JSNode JS_value (JSHexInteger val) [] [] [])}


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
                   return (JSNode JS_value (JSValue "[]") val [] [])}
           <|> do {rOp "["; v1 <- elementList; rOp ","; v2 <- elision; rOp "]"; 
                   return (JSNode JS_value (JSValue "[]") (v1++[v2]) [] [])}


-- <Elision> ::= ','
--             | <Elision> ','
elision :: GenParser Char st JSNode
elision = do{ rOp ",";
              return (JSNode JS_value (JSValue "elision") [] [] [])}
      <|> do{ v1 <- elision; rOp ",";
              return (JSNode JS_value (JSValue "elisions") [v1] [] [])}
    

-- <Element List> ::= <Elision> <Assignment Expression>
--                  | <Element List> ',' <Elision>  <Assignment Expression>
--                  | <Element List> ',' <Assignment Expression>
--                  | <Assignment Expression>
elementList :: GenParser Char st [JSNode]
elementList = do{ v1 <- elision; v2 <- assignmentExpression;
              return [(JSNode JS_value (JSValue "elementList") (v1:v2) [] [])]}
          <|> do{ v1 <- elementList; rOp ","; v2 <- elision; v3 <- assignmentExpression;
              return [(JSNode JS_value (JSValue "elementList") (v1++[v2]++v3) [] [])]}
          <|> do{ v1 <- elementList; rOp ","; v2 <- assignmentExpression;
              return [(JSNode JS_value (JSValue "elementList") (v1++v2) [] [])]}
          <|> assignmentExpression


-- <Object Literal> ::= '{' <Property Name and Value List> '}'
objectLiteral :: GenParser Char st JSNode
objectLiteral = do{ rOp "{"; val <- propertyNameandValueList; rOp "}"; 
                    --return [""]} -- TODO: proper return
                   return (JSNode JS_value (JSValue "{}") [val] [] [])}

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>
propertyNameandValueList :: GenParser Char st JSNode
propertyNameandValueList = do{ val <- sepBy1 propertyNameandValue (rOp ",");
                           return (JSNode JS_value (JSValue "propertyNameandValueList") val [] [])}
                           
propertyNameandValue :: GenParser Char st JSNode
propertyNameandValue = do{ v1 <- propertyName; rOp ":"; v2 <- assignmentExpression;
                           return (JSNode JS_value (JSValue "propertyNameandValue") (v1:v2) [] [])}

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
memberExpression :: GenParser Char st [JSNode]
memberExpression = try(do{ reserved "new"; v1 <- memberExpression; v2 <- arguments; 
                        return (((JSNode JS_value (JSValue "memberExpression.new") [] [] []):v1)++[v2])})
                <|> memberExpression'

memberExpression' :: GenParser Char st [JSNode]
memberExpression' = try(do{v1 <- primaryExpression; v2 <- rest;
                        return (v1:v2)})
                <|> try(do{v1 <- functionExpression; v2 <- rest;
                        return (v1:v2)})

                where
                  rest = do{ rOp "["; v1 <- expression; rOp "]"; v2 <- rest;
                             return [(JSNode JS_value (JSValue "memberExpression[]") (v1:v2) [] [])]}
                     <|> do{ rOp "."; v1 <- identifier ; v2 <- rest;
                             return [(JSNode JS_value (JSValue "memberExpression.") (v1:v2) [] [])]}
                     <|> return []
                         


-- <New Expression> ::= <Member Expression>
--                    | new <New Expression>
newExpression :: GenParser Char st [JSNode]
newExpression = memberExpression
           <|> do{ reserved "new"; val <- newExpression;
                   return ((JSNode JS_value (JSValue "new") [] [] []):val)}

-- <Call Expression> ::= <Member Expression> <Arguments>
--                     | <Call Expression> <Arguments> 
--                     | <Call Expression> '[' <Expression> ']'
--                     | <Call Expression> '.' Identifier

callExpression :: GenParser Char st [JSNode]
callExpression = do{ v1 <- memberExpression; v2 <- arguments; 
                      do { v3 <- rest; 
                          return (v1++[v2,v3])}
                  <|> do {return (v1++[v2   ])}
                   }
                 where
                   rest =
                         do{ v1 <- arguments ; v2 <- rest;
                             return (JSNode JS_value (JSValue "callExpression.args") [v1,v2] [] [])}
                     <|> do{ rOp "["; v1 <- expression; rOp "]"; v2 <- rest;
                             return (JSNode JS_value (JSValue "callExpression[]") [v1,v2] [] [])}
                     <|> do{ rOp "."; v1 <- identifier; v2 <- rest;
                             return (JSNode JS_value (JSValue "callExpression.") [v1,v2] [] [])}

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


-- <Arguments> ::= '(' ')'
--               | '(' <Argument List> ')'
arguments :: GenParser Char st JSNode
arguments = try(do{ rOp "(";  rOp ")";
                return (JSNode JS_value (JSValue "arguments") [] [] [])})
        <|> do{ rOp "("; v1 <- argumentList; rOp ")";
                return (JSNode JS_value (JSValue "arguments") [v1] [] [])}

-- <Argument List> ::= <Assignment Expression>
--                   | <Argument List> ',' <Assignment Expression>
argumentList :: GenParser Char st JSNode
argumentList = do{ vals <- sepBy1 assignmentExpression (rOp ",");
                   return (JSNode JS_value (JSValue "argumentList") (flatten vals) [] [])}


-- <Left Hand Side Expression> ::= <New Expression> 
--                               | <Call Expression>
leftHandSideExpression :: GenParser Char st [JSNode]
leftHandSideExpression = try (callExpression)
                     <|> newExpression
                     <?> "leftHandSideExpression"


-- <Postfix Expression> ::= <Left Hand Side Expression>
--                        | <Postfix Expression> '++'
--                        | <Postfix Expression> '--'
postfixExpression :: GenParser Char st [JSNode]
postfixExpression = do{ v1 <- leftHandSideExpression;
                        do {
                              do{ rOp "++"; return ((JSNode JS_value (JSValue "postfixExpression++") [] [] []):v1)}
                          <|> do{ rOp "--"; return ((JSNode JS_value (JSValue "postfixExpression--") [] [] []):v1)}
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
unaryExpression :: GenParser Char st [JSNode]
unaryExpression = do{ v1 <- postfixExpression; 
                      return v1}
              <|> do{ reserved "delete"; v1 <- unaryExpression;
                      return ((JSNode JS_value (JSValue "unaryExpression.delete") [] [] []):v1)}
              <|> do{ reserved "void";   v1 <- unaryExpression;
                      return ((JSNode JS_value (JSValue "unaryExpression.void") [] [] []):v1)}
              <|> do{ reserved "typeof"; v1 <- unaryExpression;
                      return ((JSNode JS_value (JSValue "unaryExpression.typeof") [] [] []):v1)}
              <|> do{ rOp "++"; v1 <- unaryExpression;
                      return ((JSNode JS_value (JSValue "unaryExpression++") [] [] []):v1)}
              <|> do{ rOp "--"; v1 <- unaryExpression;
                      return ((JSNode JS_value (JSValue "unaryExpression--") [] [] []):v1)}
              <|> do{ rOp "+"; v1 <- unaryExpression;
                      return ((JSNode JS_value (JSValue "unaryExpression+") [] [] []):v1)}
              <|> do{ rOp "-"; v1 <- unaryExpression;
                      return ((JSNode JS_value (JSValue "unaryExpression-") [] [] []):v1)}
              <|> do{ rOp "~"; v1 <- unaryExpression;
                      return ((JSNode JS_value (JSValue "unaryExpression~") [] [] []):v1)}
              <|> do{ rOp "!"; v1 <- unaryExpression;
                      return ((JSNode JS_value (JSValue "unaryExpression!") [] [] []):v1)}


-- <Multiplicative Expression> ::= <Unary Expression>
--                               | <Unary Expression> '*' <Multiplicative Expression> 
--                               | <Unary Expression> '/' <Multiplicative Expression>                               
--                               | <Unary Expression> '%' <Multiplicative Expression> 
multiplicativeExpression :: GenParser Char st [JSNode]
multiplicativeExpression = do{ v1 <- unaryExpression;
                               do {
                                  do{ rOp "*"; v2 <- multiplicativeExpression;
                                      return (v1++[(JSNode JS_value (JSValue "multiplicativeExpression*") v2 [] [])])}
                                  <|> do{ rOp "/"; v2 <- multiplicativeExpression;
                                          return (v1++[(JSNode JS_value (JSValue "multiplicativeExpression/") v2 [] [])])}
                                  <|> do{ rOp "%"; v2 <- multiplicativeExpression;
                                          return (v1++[(JSNode JS_value (JSValue "multiplicativeExpression%") v2 [] [])])}
                                  <|> return v1
                                  }
                               }


-- <Additive Expression> ::= <Additive Expression>'+'<Multiplicative Expression> 
--                         | <Additive Expression>'-'<Multiplicative Expression>  
--                         | <Multiplicative Expression>
additiveExpression :: GenParser Char st [JSNode]
additiveExpression = do{ v1 <- multiplicativeExpression;
                         do {
                             do { rOp "+"; v2 <- multiplicativeExpression;
                                  return [(JSNode JS_value (JSValue "additiveExpression+") (v1++v2) [] [])]}
                         <|> do { rOp "-"; v2 <- multiplicativeExpression;
                                  return [(JSNode JS_value (JSValue "additiveExpression-") (v1++v2) [] [])]}
                         <|>  return v1
                            }
                         }
                 

-- <Shift Expression> ::= <Shift Expression> '<<' <Additive Expression>
--                      | <Shift Expression> '>>' <Additive Expression>
--                      | <Shift Expression> '>>>' <Additive Expression>
--                      | <Additive Expression>
shiftExpression :: GenParser Char st [JSNode]
shiftExpression = do{ v1 <- additiveExpression;    
                    do {
                        do{ rOp "<<"; v2 <- additiveExpression;
                            return [(JSNode JS_value (JSValue "shiftExpression<<") (v1++v2) [] [])]}
                    <|> do{ rOp ">>"; v2 <- additiveExpression;
                            return [(JSNode JS_value (JSValue "shiftExpression>>") (v1++v2) [] [])]}
                    <|> do{ rOp ">>>"; v2 <- additiveExpression;
                            return [(JSNode JS_value (JSValue "shiftExpression>>>") (v1++v2) [] [])]}
                    <|> return v1
                       }
                    }


-- <Relational Expression>::= <Shift Expression> 
--                          | <Relational Expression> '<' <Shift Expression> 
--                          | <Relational Expression> '>' <Shift Expression> 
--                          | <Relational Expression> '<=' <Shift Expression> 
--                          | <Relational Expression> '>=' <Shift Expression> 
--                          | <Relational Expression> 'instanceof' <Shift Expression> 
relationalExpression :: GenParser Char st [JSNode]
relationalExpression = do{ v1 <- shiftExpression;
                           do {
                               do{ rOp "<"; v2 <- shiftExpression;
                                   return [(JSNode JS_value (JSValue "relationalExpression<") (v1++v2) [] [])]}
                           <|> do{ rOp ">"; v2 <- shiftExpression;
                                   return [(JSNode JS_value (JSValue "relationalExpression>") (v1++v2) [] [])]}
                           <|> do{ rOp "<="; v2 <- shiftExpression;
                                   return [(JSNode JS_value (JSValue "relationalExpression<=") (v1++v2) [] [])]}
                           <|> do{ rOp ">="; v2 <- shiftExpression;
                                   return [(JSNode JS_value (JSValue "relationalExpression>=") (v1++v2) [] [])]}
                           <|> do{ reserved "instanceof"; v2 <- shiftExpression;
                                   return [(JSNode JS_value (JSValue "relationalExpression.instanceof") (v1++v2) [] [])]}
                           <|> return v1
                              }
                           }



-- <Equality Expression> ::= <Relational Expression>
--                         | <Equality Expression> '==' <Relational Expression>
--                         | <Equality Expression> '!=' <Relational Expression>
--                         | <Equality Expression> '===' <Relational Expression>
--                         | <Equality Expression> '!==' <Relational Expression>
equalityExpression :: GenParser Char st [JSNode]
equalityExpression = do{ v1 <- relationalExpression;
                         do {
                              try(do{ rOp "=="; v2 <- relationalExpression;
                                  return [(JSNode JS_value (JSValue "equalityExpression==") (v1++v2) [] [])]})
                          <|> try(do{ rOp "!="; v2 <- relationalExpression;
                                  return [(JSNode JS_value (JSValue "equalityExpression!=") (v1++v2) [] [])]})
                          <|> try(do{ rOp "==="; v2 <- relationalExpression;
                                  return [(JSNode JS_value (JSValue "equalityExpression===") (v1++v2) [] [])]})
                          <|> try(do{ rOp "!=="; v2 <- relationalExpression;
                                  return [(JSNode JS_value (JSValue "equalityExpression!==") (v1++v2) [] [])]})
                          <|> return v1
                            }
                         }
                        

-- <Bitwise And Expression> ::= <Equality Expression>
--                            | <Bitwise And Expression> '&' <Equality Expression>
bitwiseAndExpression :: GenParser Char st [JSNode]
bitwiseAndExpression = do{ v1 <- equalityExpression;
                           do {
                                do{ rOp "&"; v2 <- equalityExpression;
                                    return [(JSNode JS_value (JSValue "bitwiseAndExpression") (v1++v2) [] [])]}
                             <|> return v1
                              }
                           }


-- <Bitwise XOr Expression> ::= <Bitwise And Expression>
--                            | <Bitwise XOr Expression> '^' <Bitwise And Expression>
bitwiseXOrExpression :: GenParser Char st [JSNode]
bitwiseXOrExpression = do{ v1 <- bitwiseAndExpression;
                           do {
                                do{ rOp "^"; v2 <- bitwiseAndExpression;
                                    return [(JSNode JS_value (JSValue "bitwiseXOrExpression") (v1++v2) [] [])]}
                            <|> return v1
                              }
                           }


-- <Bitwise Or Expression> ::= <Bitwise XOr Expression>
--                           | <Bitwise Or Expression> '|' <Bitwise XOr Expression>
bitwiseOrExpression :: GenParser Char st [JSNode]
bitwiseOrExpression = do{ v1 <- bitwiseXOrExpression;
                          do {
                               do{ rOp "|"; v2 <- bitwiseXOrExpression;
                                   return [(JSNode JS_value (JSValue "bitwiseOrExpression") (v1++v2) [] [])]}
                           <|> return v1
                             }
                          }


-- <Logical And Expression> ::= <Bitwise Or Expression>
--                            | <Logical And Expression> '&&' <Bitwise Or Expression>
logicalAndExpression :: GenParser Char st [JSNode]
logicalAndExpression = do{ v1 <- bitwiseOrExpression;
                           do {
                               do{ rOp "&&"; v2 <- bitwiseOrExpression;
                                   return [(JSNode JS_value (JSValue "logicalAndExpression") (v1++v2) [] [])]}
                            <|> return v1
                              }
                           }



-- <Logical Or Expression> ::= <Logical And Expression>
--                           | <Logical Or Expression> '||' <Logical And Expression>
logicalOrExpression :: GenParser Char st [JSNode]
logicalOrExpression =  do{ v1 <- logicalAndExpression;
                           do {
                               do{ rOp "||"; v2 <- logicalAndExpression;
                                   return [(JSNode JS_value (JSValue "logicalOrExpression") (v1++v2) [] [])]}
                            <|> return v1
                              }
                           }

-- <Conditional Expression> ::= <Logical Or Expression> 
--                            | <Logical Or Expression> '?' <Assignment Expression> ':' <Assignment Expression>
conditionalExpression :: GenParser Char st [JSNode]
conditionalExpression = do{ v1 <- logicalOrExpression;
                            do {
                                 do{ rOp "?"; v2 <- assignmentExpression; rOp ":"; v3 <- assignmentExpression;
                                     return [(JSNode JS_value (JSValue "conditionalExpression.ternary") (v1++v2++v3) [] [])]}
                             <|> return v1
                               }
                            }


-- <Assignment Expression> ::= <Conditional Expression>
--                           | <Left Hand Side Expression> <Assignment Operator> <Assignment Expression> 
assignmentExpression :: GenParser Char st [JSNode]
assignmentExpression = try (do {v1 <- assignmentStart; v2 <- assignmentExpression;
                           return [(JSElement "assignmentExpression" (v1++v2))]})
                    <|> conditionalExpression
                       
assignmentStart :: GenParser Char st1 [JSNode]
assignmentStart = do {v1 <- leftHandSideExpression; v2 <- assignmentOperator; 
                           return (v1++[v2])}

-- <Assignment Operator> ::= '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
assignmentOperator :: GenParser Char st JSNode
assignmentOperator = rOp' "=" <|> rOp' "*=" <|> rOp' "/=" <|> rOp' "%=" <|> rOp' "+=" <|> rOp' "-="
                 <|> rOp' "<<=" <|> rOp' ">>=" <|> rOp' ">>>=" <|> rOp' "&=" <|> rOp' "^=" <|> rOp' "|="
                     
rOp' :: String -> GenParser Char st JSNode
rOp' x = do{ rOp x; return $ JSOperator x}

-- <Expression> ::= <Assignment Expression>
--                | <Expression> ',' <Assignment Expression>
expression :: GenParser Char st JSNode
expression = do{ val <- sepBy1 assignmentExpression (rOp ",");
                 return (JSExpression (flatten val))}


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
statement :: GenParser Char st JSNode
statement = block
        <|> expression 
        <|> variableStatement
        <|> emptyStatement 
        <|> try(ifElseStatement)
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
        <?> "statement"


-- <Block > ::= '{' '}'
--            | '{' <Statement List> '}'
block :: GenParser Char st JSNode
block = try (do {rOp "{"; rOp "}"; 
            return (JSNode JS_BLOCK NoValue [] [] []) })
    <|> do {rOp "{"; val <- statementList; rOp "}"; 
            return (JSNode JS_BLOCK NoValue val [] []) }
    <?> "block"


-- <Statement List> ::= <Statement>
--                    | <Statement List> <Statement>
statementList :: GenParser Char st [JSNode]
{-
statementList = try(do { v1 <- statement;
                    return [JSNode JS_BLOCK NoValue [v1] [] []] })
             <|> try(do { v1 <- statementList; v2 <- statement;
                    return [JSNode JS_BLOCK NoValue (v1++[v2]) [] []] })
-}
statementList = many1 statement

-- <Variable Statement> ::= var <Variable Declaration List> ';'
variableStatement :: GenParser Char st JSNode
variableStatement = do {reserved "var"; val <- variableDeclarationList;
                        return (JSNode JS_value (JSValue "var") [val] [] []) }

                    
-- <Variable Declaration List> ::= <Variable Declaration>
--                               | <Variable Declaration List> ',' <Variable Declaration>
variableDeclarationList :: GenParser Char st JSNode
variableDeclarationList = do{ val <- sepBy1 variableDeclaration (rOp ","); 
                              return (JSNode JS_value (JSValue "varlist") val [] []) }


-- <Variable Declaration> ::= Identifier
--                          | Identifier <Initializer>
variableDeclaration :: GenParser Char st JSNode
variableDeclaration = do{ val <- identifier; 
                          return (JSNode JS_value (JSValue "vardecl") [val] [] []) }
                  <|> do{ v1 <- identifier; v2 <- initializer; 
                          return (JSNode JS_value (JSValue "vardecl") [v1,v2] [] []) }

-- <Initializer> ::= '=' <Assignment Expression>
initializer :: GenParser Char st JSNode
initializer = do {rOp "="; val <- assignmentExpression; 
                  return (JSNode JS_value (JSValue "initializer") val [] []) }

-- <Empty Statement> ::= ';'
emptyStatement :: GenParser Char st JSNode
emptyStatement = do { rOp ";"; return JSEmpty }


-- <If Statement> ::= 'if' '(' <Expression> ')' <Statement> 
ifStatement :: GenParser Char st JSNode
ifStatement = do{ reserved "if"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement;
                  return (JSNode JS_value (JSValue "if") [v1,v2] [] []) }

-- <If Else Statement> ::= 'if' '(' <Expression> ')' <Statement> 'else' <Statement>
ifElseStatement :: GenParser Char st JSNode
ifElseStatement = do{ reserved "if"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement; reserved "else"; v3 <- statement ;
                      return (JSNode JS_value (JSValue "if_else") [v1,v2,v3] [] []) }


-- <Iteration Statement> ::= 'do' <Statement> 'while' '(' <Expression> ')' ';'
--                         | 'while' '(' <Expression> ')' <Statement> 
--                         | 'for' '(' <Expression> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration List> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' <Left Hand Side Expression> in <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration> in <Expression> ')' <Statement> 
iterationStatement :: GenParser Char st JSNode
iterationStatement = do{ reserved "do"; v1 <- statement; reserved "while"; rOp "("; v2 <- expression; rOp ")"; rOp ";" ; 
                         return (JSNode JS_value (JSValue "do_while") [v1,v2] [] []) }
                 <|> do{ reserved "while"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement; 
                         return (JSNode JS_value (JSValue "while") [v1,v2] [] []) }
                 <|> do{ reserved "for"; rOp "("; v1 <- expression; rOp ";"; v2 <- expression; rOp ";"; 
                         v3 <- expression; rOp ")"; v4 <- statement; 
                         return (JSNode JS_value (JSValue "for") [v1,v2,v3,v4] [] []) }
                 <|> do{ reserved "for"; rOp "("; reserved "var"; v1 <- variableDeclarationList; rOp ";"; v2 <- expression; 
                         rOp ";"; v3 <- expression; rOp ")"; v4 <- statement; 
                         return (JSNode JS_value (JSValue "for_var") [v1,v2,v3,v4] [] []) }
                 <|> do{ reserved "for"; rOp "("; v1 <- leftHandSideExpression; reserved "in"; v2 <- expression; rOp ")"; 
                         v3 <- statement; 
                         return (JSNode JS_value (JSValue "for_in") (v1++[v2,v3]) [] []) }
                 <|> do{ reserved "for"; rOp "("; reserved "var"; v1 <- variableDeclaration; reserved "in"; 
                         v2 <- expression; rOp ")"; v3 <- statement; 
                         return (JSNode JS_value (JSValue "for_in_var") [v1,v2,v3] [] []) }


-- <Continue Statement> ::= 'continue' ';'
--                        | 'continue' Identifier ';'
continueStatement :: GenParser Char st JSNode
continueStatement = do {reserved "continue"; rOp ";"; 
                        return (JSNode JS_value (JSValue "continue") [] [] []) }
                <|> do {reserved "continue"; val <- identifier; rOp ";"; 
                        return (JSNode JS_value (JSValue "continue") [val] [] []) }


-- <Break Statement> ::= 'break' ';'
--                        | 'break' Identifier ';'
breakStatement :: GenParser Char st JSNode
breakStatement = do {reserved "break"; rOp ";"; 
                     return (JSNode JS_value (JSValue "break") [] [] []) }
            <|>  do {reserved "break"; val <- identifier; rOp ";"; 
                     return (JSNode JS_value (JSValue "return") [val] [] []) }


-- <Return Statement> ::= 'return' ';'
--                        | 'return' <Expression> ';'
returnStatement :: GenParser Char st JSNode
returnStatement = do {reserved "return"; rOp ";"; 
                      return (JSNode JS_value (JSValue "return") [] [] []) }
              <|> do {reserved "return"; val <- expression; rOp ";"; 
                      return (JSNode JS_value (JSValue "return") [val] [] []) }


-- <With Statement> ::= 'with' '(' <Expression> ')' <Statement> ';'
withStatement :: GenParser Char st JSNode
withStatement = do{ reserved "with"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement; rOp ";"; 
                    return (JSNode JS_value (JSValue "with") [v1,v2] [] []) }


-- <Switch Statement> ::= 'switch' '(' <Expression> ')' <Case Block>  
switchStatement :: GenParser Char st JSNode
switchStatement = do{ reserved "switch"; rOp "("; v1 <- expression; rOp ")"; v2 <- caseBlock;
                      return (JSNode JS_value (JSValue "switch") [v1,v2] [] []) }


-- <Case Block> ::= '{' '}'
--                | '{' <Case Clauses> '}'
--                | '{' <Case Clauses> <Default Clause> '}'
--                | '{' <Case Clauses> <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> '}'
caseBlock :: GenParser Char st JSNode
caseBlock = do{ rOp "{"; rOp "}"; 
                return (JSNode JS_value (JSValue "case") [] [] []) }
        <|> do{ rOp "{"; val <- caseClauses; rOp "}"; 
                return (JSNode JS_value (JSValue "case") [val] [] []) }
        <|> do{ rOp "{"; v1 <- caseClauses; v2 <- defaultClause; rOp "}"; 
                return (JSNode JS_value (JSValue "case_default") [v1,v2] [] []) }
        <|> do{ rOp "{"; v1 <- caseClauses; v2 <- defaultClause; v3 <- caseClauses; rOp "}"; 
                return (JSNode JS_value (JSValue "case_default2") [v1,v2,v3] [] []) }
        <|> do{ rOp "{"; v1 <- defaultClause; v2 <- caseClauses; rOp "}"; 
                return (JSNode JS_value (JSValue "case_default3") [v1,v2] [] []) }
        <|> do{ rOp "{"; v1 <- defaultClause; rOp "}"; 
                return (JSNode JS_value (JSValue "case_default4") [v1] [] []) }


-- <Case Clauses> ::= <Case Clause>
--                  | <Case Clauses> <Case Clause>
caseClauses :: GenParser Char st JSNode
caseClauses = do{ val <- many1 caseClause;
                return (JSNode JS_value (JSValue "case_clauses") val [] []) }

-- <Case Clause> ::= 'case' <Expression> ':' <Statement List>
--                 | 'case' <Expression> ':'
caseClause :: GenParser Char st JSNode
caseClause = do { reserved "case"; val <- expression; rOp ":"; 
                return (JSNode JS_value (JSValue "case_clause") [val] [] []) }
         <|> do { reserved "case"; v1 <- expression; rOp ":"; v2 <- statementList;
                return (JSNode JS_value (JSValue "case_clause") (v1:v2) [] []) }


-- <Default Clause> ::= 'default' ':' 
--                    | 'default' ':' <Statement List>
defaultClause :: GenParser Char st JSNode
defaultClause = do{ reserved "default"; rOp ":"; 
                    return (JSNode JS_value (JSValue "default") [] [] []) }
            <|> do{ reserved "default"; rOp ":"; v1 <- statementList;
                    return (JSNode JS_value (JSValue "default") v1 [] []) }

-- <Labelled Statement> ::= Identifier ':' <Statement> 
labelledStatement :: GenParser Char st JSNode
labelledStatement = do { v1 <- identifier; rOp ":"; v2 <- statement;
                         return (JSNode JS_value (JSValue "labelled") [v1,v2] [] []) }

-- <Throw Statement> ::= 'throw' <Expression>
throwStatement :: GenParser Char st JSNode
throwStatement = do{ reserved "throw"; val <- expression;
                   return (JSNode JS_value (JSValue "throw") [val] [] []) }

-- <Try Statement> ::= 'try' <Block> <Catch>
--                   | 'try' <Block> <Finally>
--                   | 'try' <Block> <Catch> <Finally>
tryStatement :: GenParser Char st JSNode
tryStatement = do{ reserved "try"; v1 <- block; v2 <- catch;
                   return (JSNode JS_value (JSValue "try_catch") [v1,v2] [] []) }
           <|> do{ reserved "try"; v1 <- block; v2 <- finally;
                   return (JSNode JS_value (JSValue "try_finally") [v1,v2] [] []) }
           <|> do{ reserved "try"; v1 <- block; v2 <- catch; v3 <- finally;
                   return (JSNode JS_value (JSValue "try_catch_finally") [v1,v2,v3] [] []) }

-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
catch :: GenParser Char st JSNode
catch = do{ reserved "catch"; rOp "("; v1 <- identifier; rOp ")"; v2 <- block;
            return (JSNode JS_value (JSValue "catch") [v1,v2] [] []) }

-- <Finally> ::= 'finally' <Block>
finally :: GenParser Char st JSNode
finally = do{ reserved "finally"; v1 <- block;
            return (JSNode JS_value (JSValue "finally") [v1] [] []) }


-- <Function Declaration> ::= 'function' Identifier '(' <Formal Parameter List> ')' '{' <Function Body> '}'
--                          | 'function' Identifier '(' ')' '{' <Function Body> '}'
functionDeclaration :: GenParser Char st JSNode
functionDeclaration = do {reserved "function"; v1 <- identifier; rOp "("; v2 <- formalParameterList; rOp ")"; 
                          v3 <- functionBody; 
                          return (JSFunction v1 (v2++[v3])) } 
                  <?> "functionDeclaration"


-- <Function Expression> ::= 'function' '(' ')' '{' <Function Body> '}'
---                        | 'function' '(' <Formal Parameter List> ')' '{' <Function Body> '}'
functionExpression :: GenParser Char st JSNode
functionExpression = do{ reserved "function"; rOp "("; optional formalParameterList; rOp ")"; functionBody; }

-- <Formal Parameter List> ::= Identifier
--                           | <Formal Parameter List> ',' Identifier
formalParameterList :: GenParser Char st [JSNode]
formalParameterList = sepBy identifier (rOp ",")

-- <Function Body> ::= '{' <Source Elements> '}'
--                   | '{' '}'
functionBody :: GenParser Char st JSNode
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
program :: GenParser Char st JSNode
program = do {val <- sourceElements; eof; return val}
--program = sourceElements

-- <Source Elements> ::= <Source Element>
--                     | <Source Elements>  <Source Element>
sourceElements :: GenParser Char st JSNode

sourceElements = do{ val <- many1 sourceElement;
                     return (JSNode JS_BLOCK NoValue val [] []) }
{-
sourceElements = sourceElement
          <|> do{ v1 <- sourceElements; v2 <- sourceElement;
                  return (JSNode JS_BLOCK NoValue [v1,v2] [] []) }
          <?> "sourceElements"
-}                  

-- <Source Element> ::= <Statement>
--                    | <Function Declaration>
sourceElement :: GenParser Char st JSNode
sourceElement = functionDeclaration
            <|> statement
            <?> "sourceElement"

-- ---------------------------------------------------------------
-- Testing

m :: IO ()
m = do args <- getArgs
       putStrLn (readJs (args !! 0))


-- ---------------------------------------------------------------------

flatten :: [[a]] -> [a]
flatten xs = foldl' (++) [] xs

-- ---------------------------------------------------------------------

main :: IO ()
main =
  do args <- getArgs
     x <- readFile (args !! 0)
     putStrLn (readJs x)            
    
-- ---------------------------------------------------------------------
          
readJs :: String -> String
readJs input = case parse program "js" input of
    Left err -> "No match: " ++ show err
    Right val -> "Parsed" ++ show(val)

-- ---------------------------------------------------------------------
    
doParse :: (Show a,Show tok) => GenParser tok () a -> [tok] -> [Char]
doParse p input = case parse (p' p) "js" input of
    Left err -> "No match: " ++ show err
    Right val -> "Parsed:" ++ show(val)

-- ---------------------------------------------------------------------
    
--p' :: GenParser Char st JSNode
p' :: (Show tok) => GenParser tok st b -> GenParser tok st b
p' p = do {val <- p; eof; return val}

-- EOF
