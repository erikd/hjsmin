{-# LANGUAGE DeriveDataTypeable #-}
module Text.Jasmine.Parse
    (       
      parseScript
    , JasmineSettings (..)
    , defaultJasmineSettings
    ) where

-- ---------------------------------------------------------------------

import Prelude hiding (catch)
import System.Environment
import Control.Applicative ((<$>), Applicative (..))
import Control.Monad
import Control.Arrow
import Data.Data
import Data.List (intercalate)
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

data Script = ScriptForall [String]
    deriving (Show, Eq, Read, Data, Typeable)

data Statement = Statement 
    deriving (Show, Eq, Read, Data, Typeable)

data Line = LineForall String
    deriving (Eq, Show, Read)

-- ---------------------------------------------------------------------

parseScript :: JasmineSettings -> String -> Result [(Int,Line)]
parseScript set s = do
    ls <- parseLines set s
    return ls 

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

parseLines :: JasmineSettings -> String -> Result [(Int, Line)]
parseLines set s =
    case parse (many $ parseLine set) s s of
        Left e -> Error $ show e
        Right x -> Ok x

-- ---------------------------------------------------------------------

parseLine :: JasmineSettings -> Parser (Int, Line)
parseLine set = do
    ss <- fmap sum $ many ((char ' ' >> return 1) <|>
                           (char '\t' >> return 4))
    x <- comment
    
    return (ss,x)
  where
    eol' = (char '\n' >> return ()) <|> (string "\r\n" >> return ())
    eol = eof <|> eol'
    
    comment = do
        _ <- try $ string "$#"
        _ <- many $ noneOf "\r\n"
        eol
        return $ LineForall ""

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
lexer       = P.makeTokenParser javascriptDef
      
parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
rOp  = P.reservedOp lexer
        
-- ---------------------------------------------------------------------
-- The parser, based on the gold parser for Javascript
-- http://www.devincook.com/GOLDParser/grammars/files/JavaScript.zip
 
-- Filling the gaps       
stringLiteral = P.stringLiteral lexer

decimalLiteral = P.decimal lexer

hexIntegerLiteral = P.hexadecimal lexer -- TODO: check prefix etc for Javascript convention

-- {String Chars1} = {Printable} + {HT} - ["\] 
-- {RegExp Chars} = {Letter}+{Digit}+['^']+['$']+['*']+['+']+['?']+['{']+['}']+['|']+['-']+['.']+[',']+['#']+['[']+[']']+['_']+['<']+['>']
-- {Non Terminator} = {String Chars1} - {CR} - {LF}
-- RegExp         = '/' ({RegExp Chars} | '\' {Non Terminator})+ '/' ( 'g' | 'i' | 'm' )*

-- TODO: do this properly, it is late now, want to move on
regExp = do { rOp "/" many (noneOf "/"); rOp "/"; optional (oneOf "gim")}
    
       

-- <Literal> ::= <Null Literal>
--             | <Boolean Literal>
--             | <Numeric Literal>
--             | StringLiteral
literal = nullLiteral
      <|> booleanLiteral
      <|> numericLiteral
      <|> stringLiteral

-- <Null Literal>    ::= null
nullLiteral = reserved "null"

-- <Boolean Literal> ::= 'true'
--                     | 'false'
booleanLiteral = reserved "true" <|> reserved "false"

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
numericLiteral = decimalLiteral
             <|> hexIntegerLiteral

-- <Regular Expression Literal> ::= RegExp 
regularExpressionLiteral = regExp


-- <Primary Expression> ::= 'this'
--                        | Identifier
--                        | <Literal> 
--                        | <Array Literal>
--                        | <Object Literal>
--                        | '(' <Expression> ')'
--                        | <Regular Expression Literal>
primaryExpression = reserved "this"
                <|> identifier
                <|> literal
                <|> arrayLiteral
                <|> objectLiteral
                <|> do{ rOp "("; expression; rOp ")"}
                <|> regularExpressionLiteral


-- <Array Literal> ::= '[' ']'
--                   | '[' <Elision> ']'
--                   | '[' <Element List> ']'
--                   | '[' <Element List> ',' <Elision> ']'
arrayLiteral = do {rOp "["; rOp "]"}
           <|> do {rOp "["; elision; rOp "]"}
           <|> do {rOp "["; elementList; rOp "]"}
           <|> do {rOp "["; elementList; rOp ","; elision; rOp "]"}


-- <Elision> ::= ','
--             | <Elision> ','
elision = many1 (rOp ",")


-- <Element List> ::= <Elision> <Assignment Expression>
--                  | <Element List> ',' <Elision>  <Assignment Expression>
--                  | <Element List> ',' <Assignment Expression>
--                  | <Assignment Expression>
elementList = do{ elision; assignmentExpression}
          <|> do{ elementList; rOp ","; elision; assignmentExpression;}
          <|> do{ elementList; rOp ","; assignmentExpression;}
          <|> assignmentExpression

-- <Object Literal> ::= '{' <Property Name and Value List> '}'
objectLiteral = do{ rOp "{"; propertyNameandValueList; rOp "}"}


-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>
propertyNameandValueList = sepBy1 (do{ propertyName; rOp ":"; assignmentExpression}) (rOp ",")


-- <Property Name> ::= Identifier
--                   | StringLiteral
--                   | <Numeric Literal>
propertyName = identifier
            <|> stringLiteral
            <|> numericLiteral


-- <Member Expression > ::= <Primary Expression>
--                        | <Function Expression>
--                        | <Member Expression> '[' <Expression> ']'
--                        | <Member Expression> '.' Identifier
--                        | 'new' <Member Expression> <Arguments>
memberExpression = primaryExpression
                <|> functionExpression
                <|> do{ memberExpression; rOp "["; expression; rOp "]"}
                <|> do{ memberExpression; rOp "."; identifier}
                <|> do{ reserved "new"; memberExpression; arguments}


-- <New Expression> ::= <Member Expression>
--                    | new <New Expression>
newExpression = memberExpression
           <|> do{ reserved "new"; newExpression}

-- <Call Expression> ::= <Member Expression> <Arguments>
--                     | <Call Expression> <Arguments> 
--                     | <Call Expression> '[' <Expression> ']'
--                     | <Call Expression> '.' Identifier
callExpression = do{ memberExpression; arguments}
             <|> do{ callExpression; arguments }
             <|> do{ callExpression; rOp "["; expression; rOp "]" }
             <|> do{ callExpression; rOp "."; identifier }


-- <Arguments> ::= '(' ')'
--               | '(' <Argument List> ')'
arguments = do{ rOp "("; optional argumentList; rOp ")"}

-- <Argument List> ::= <Assignment Expression>
--                   | <Argument List> ',' <Assignment Expression>
argumentList = sepBy1 assignmentExpression (rOp ",")



-- <Left Hand Side Expression> ::= <New Expression> 
--                               | <Call Expression>
leftHandSideExpression = newExpression
                     <|> callExpression


-- <Postfix Expression> ::= <Left Hand Side Expression>
--                        | <Postfix Expression> '++'
--                        | <Postfix Expression> '--'
postfixExpression = leftHandSideExpression
                <|> do{ postfixExpression; rOp "++"}
                <|> do{ postfixExpression; rOp "--"}


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
multiplicativeExpression = unaryExpression
                       <|> do{ unaryExpression; rOp "*"; multiplicativeExpression}
                       <|> do{ unaryExpression; rOp "/"; multiplicativeExpression}
                       <|> do{ unaryExpression; rOp "%"; multiplicativeExpression}


-- <Additive Expression> ::= <Additive Expression>'+'<Multiplicative Expression> 
--                         | <Additive Expression>'-'<Multiplicative Expression>  
--                         | <Multiplicative Expression>
additiveExpression = do {additiveExpression; rOp "+"; multiplicativeExpression}
                 <|> do {additiveExpression; rOp "-"; multiplicativeExpression}
                 <|> multiplicativeExpression

-- <Shift Expression> ::= <Shift Expression> '<<' <Additive Expression>
--                      | <Shift Expression> '>>' <Additive Expression>
--                      | <Shift Expression> '>>>' <Additive Expression>
--                      | <Additive Expression>
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
equalityExpression = relationalExpression
                <|> do{ equalityExpression; rOp "=="; relationalExpression}
                <|> do{ equalityExpression; rOp "!="; relationalExpression}
                <|> do{ equalityExpression; rOp "==="; relationalExpression}
                <|> do{ equalityExpression; rOp "!=="; relationalExpression}


-- <Bitwise And Expression> ::= <Equality Expression>
--                            | <Bitwise And Expression> '&' <Equality Expression>
bitwiseAndExpression = equalityExpression
                 <|> do{ bitwiseAndExpression; rOp "&"; equalityExpression}



-- <Bitwise XOr Expression> ::= <Bitwise And Expression>
--                            | <Bitwise XOr Expression> '^' <Bitwise And Expression>
bitwiseXOrExpression = bitwiseAndExpression
                   <|> do { bitwiseXOrExpression; rOp "^"; bitwiseAndExpression}


-- <Bitwise Or Expression> ::= <Bitwise XOr Expression>
--                           | <Bitwise Or Expression> '|' <Bitwise XOr Expression>
bitwiseOrExpression = bitwiseXOrExpression
                  <|> do{ bitwiseOrExpression; rOp "|"; bitwiseXOrExpression}


-- <Logical And Expression> ::= <Bitwise Or Expression>
--                            | <Logical And Expression> '&&' <Bitwise Or Expression>
logicalAndExpression = bitwiseOrExpression
                      <|> do{ logicalAndExpression; rOp "&&"; bitwiseOrExpression}



-- <Logical Or Expression> ::= <Logical And Expression>
--                           | <Logical Or Expression> '||' <Logical And Expression>
logicalOrExpression =  logicalAndExpression
                 <|> do{ logicalOrExpression; rOp "||"; logicalAndExpression}


-- <Conditional Expression> ::= <Logical Or Expression> 
--                            | <Logical Or Expression> '?' <Assignment Expression> ':' <Assignment Expression>
conditionalExpression = logicalOrExpression
                    <|> do{ logicalOrExpression; rOp "?"; assignmentExpression; rOp ":"; assignmentExpression }


-- <Assignment Expression> ::= <Conditional Expression>
--                           | <Left Hand Side Expression> <Assignment Operator> <Assignment Expression> 
assignmentExpression = conditionalExpression
                   <|> do {leftHandSideExpression; assignmentOperator; assignmentExpression}


-- <Assignment Operator> ::= '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
assignmentOperator = rOp "="   <|> rOp "*="   <|> rOp "/=" <|> rOp "%=" <|> rOp "+=" <|> rOp "-=" <|> rOp "<<=" 
                 <|> rOp ">>=" <|> rOp ">>>=" <|> rOp "&=" <|> rOp "^=" <|> rOp "|="

-- <Expression> ::= <Assignment Expression>
--                | <Expression> ',' <Assignment Expression>
expression = sepBy1 assignmentExpression (rOp ",")


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
block = do {rOp "{"; optional statementList; rOp "}"}


-- <Statement List> ::= <Statement>
--                    | <Statement List> <Statement>
statementList = many1 statement


-- <Variable Statement> ::= var <Variable Declaration List> ';'
variableStatement = do {reserved "var"; variableDeclarationList;}
-- <Variable Declaration List> ::= <Variable Declaration>
--                               | <Variable Declaration List> ',' <Variable Declaration>
variableDeclarationList = sepBy1 variableDeclaration


-- <Variable Declaration> ::= Identifier
--                          | Identifier <Initializer>
variableDeclaration = identifier
                   <|> do {identifier; initializer}

-- <Initializer> ::= '=' <Assignment Expression>
initializer = do {rOp "="; assignmentExpression}

-- <Empty Statement> ::= ';'
emptyStatement = rOp ";"


-- <If Statement> ::= 'if' '(' <Expression> ')' <Statement> 
ifStatement = do{ reserved "if"; rOp "("; expression; rOp ")"; statement }
-- <If Else Statement> ::= 'if' '(' <Expression> ')' <Statement> 'else' <Statement>
ifElseStatement = do{ reserved "if"; rOp "("; expression; rOp ")"; statement; reserved "else"; statement }


-- <Iteration Statement> ::= 'do' <Statement> 'while' '(' <Expression> ')' ';'
--                         | 'while' '(' <Expression> ')' <Statement> 
--                         | 'for' '(' <Expression> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration List> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' <Left Hand Side Expression> in <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration> in <Expression> ')' <Statement> 
iterationStatement = do{ reserved "do"; statement; reserved "while"; rOp "("; expression; rOp ")"; rOp ";" }
                 <|> do{ reserved "while"; rOp "("; expression; rOp ")"; statement}
                 <|> do{ reserved "for"; rOp "("; expression; rOp ";"; expression; rOp ";"; expression; rOp ")"; statement}
                 <|> do{ reserved "for"; rOp "("; reserved "var"; variableDeclarationList; rOp ";"; expression; rOp ";";expression; rOp ")"; statement} 
                 <|> do{ reserved "for"; rOp "("; leftHandSideExpression; reserved "in"; expression; rOp ")"; statement}
                 <|> do{ reserved "for"; rOp "("; reserved "var"; variableDeclaration; reserved "in"; expression; rOp ")"; statement}



-- <Continue Statement> ::= 'continue' ';'
--                        | 'continue' Identifier ';'
continueStatement = do {reserved "continue"; optional identifier; rOp ";"}


-- <Break Statement> ::= 'break' ';'
--                        | 'break' Identifier ';'
breakStatement = do {reserved "break"; optional identifier; rOp ";"}


-- <Return Statement> ::= 'return' ';'
--                        | 'return' <Expression> ';'
returnStatement = do {reserved "return"; optional expression; rOp ";"}


-- <With Statement> ::= 'with' '(' <Expression> ')' <Statement> ';'
withStatement = do{ reserved "with"; rOp "("; expression; rOp ")"; statement; rOp ";"}


-- <Switch Statement> ::= 'switch' '(' <Expression> ')' <Case Block>  
switchStatement = do{ reserved "switch"; rOp "("; expression; rOp ")"; caseBlock}


-- <Case Block> ::= '{' '}'
--                | '{' <Case Clauses> '}'
--                | '{' <Case Clauses> <Default Clause> '}'
--                | '{' <Case Clauses> <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> '}'
caseBlock = do{ rOp "{"; rOp "}"}
        <|> do{ rOp "{"; caseClauses; rOp "}"}
        <|> do{ rOp "{"; caseClauses; defaultClause; rOp "}"}
        <|> do{ rOp "{"; caseClauses; defaultClause; caseClauses; rOp "}"}
        <|> do{ rOp "{"; defaultClause; caseClauses; rOp "}"}
        <|> do{ rOp "{"; defaultClause; rOp "}"}


-- <Case Clauses> ::= <Case Clause>
--                  | <Case Clauses> <Case Clause>
caseClauses = many1 caseClause


-- <Case Clause> ::= 'case' <Expression> ':' <Statement List>
--                 | 'case' <Expression> ':'
caseClause = do { reserved "case"; expression; optional statementList }


-- <Default Clause> ::= 'default' ':' 
--                    | 'default' ':' <Statement List>
defaultClause = do{ reserved "default"; rOp ":"; optional statementList}

-- <Labelled Statement> ::= Identifier ':' <Statement> 
labelledStatement = do { identifier; rOp ":"; statement }

-- <Throw Statement> ::= 'throw' <Expression>
throwStatement = do{ reserved "throw"; expression }

-- <Try Statement> ::= 'try' <Block> <Catch>
--                   | 'try' <Block> <Finally>
--                   | 'try' <Block> <Catch> <Finally>
tryStatement = do{ reserved "try"; block; catch}
           <|> do{ reserved "try"; block; finally}
           <|> do{ reserved "try"; block; catch; finally}


-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
catch = do{ reserved "catch"; rOp "("; identifier; rOp ")"; block }

-- <Finally> ::= 'finally' <Block>
finally = do{ reserved "finally"; block}


-- <Function Declaration> ::= 'function' Identifier '(' <Formal Parameter List> ')' '{' <Function Body> '}'
--                          | 'function' Identifier '(' ')' '{' <Function Body> '}'
functionDeclaration = try (do {reserved "function"; identifier; rOp "("; formalParameterList; rOp ")"; rOp "{"; functionBody; rOp "}" })
                    <|>    do {reserved "function"; identifier; rOp "(";                      rOp ")"; rOp "{"; functionBody; rOp "}" }

-- <Function Expression> ::= 'function' '(' ')' '{' <Function Body> '}'
---                        | 'function' '(' <Formal Parameter List> ')' '{' <Function Body> '}'
functionExpression = try (do{ reserved "function"; rOp "(";                      rOp ")"; rOp "{"; functionBody; rOp "}"})
                   <|>    do{ reserved "function"; rOp "("; formalParameterList; rOp ")"; rOp "{"; functionBody; rOp "}"}

-- <Formal Parameter List> ::= Identifier
--                           | <Formal Parameter List> ',' Identifier
formalParameterList = sepBy1 identifier (rOp ",")

-- <Function Body> ::= <Source Elements>
--                   | 
functionBody = optional sourceElements

-- <Program> ::= <Source Elements>
program = do {many sourceElements; eof; return()}

-- <Source Elements> ::= <Source Element>
--                     | <Source Elements>  <Source Element>
sourceElements = many1 sourceElement

-- <Source Element> ::= <Statement>
--                    | <Function Declaration>
sourceElement = statement
             <|> functionDeclaration

-- ---------------------------------------------------------------
-- Testing

main :: IO ()
main = do args <- getArgs
          putStrLn (readJs (args !! 0))

-- ---------------------------------------------------------------------
          
readJs :: String -> String
readJs input = case parse functionDeclaration "js" input of
    Left err -> "No match: " ++ show err
    Right val -> "Parsed"

symbolp :: Parser Char
symbolp = oneOf "!#$%&|*+-/:<=>?@^_~"

fff = reserved "function"

-- EOF
