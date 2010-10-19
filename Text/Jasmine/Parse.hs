{-# LANGUAGE DeriveDataTypeable #-}
module Text.Jasmine.Parse
    (       
    --  parseScript
      readJs
    , JasmineSettings (..)
    , defaultJasmineSettings
    , JSNode(..)  
    -- For testing      
    , doParse
    , program  
    , functionDeclaration
    , identifier
    , statementList  
    , iterationStatement  
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

data JSNode = JSArguments [[JSNode]]  
              | JSArrayLiteral [JSNode]
              | JSBlock [JSNode]
              | JSBreak [JSNode]
              | JSCallExpression String [JSNode] -- type : ., (), []; rest  
              | JSCase JSNode [JSNode]
              | JSCatch JSNode JSNode
              | JSContinue [JSNode]
              | JSDecimal Integer   
              | JSDefault [JSNode]  
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
              | JSFor JSNode [JSNode] [JSNode] JSNode                
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
              | JSMemberSquare [JSNode]  
              | JSObjectLiteral [JSNode]  
              | JSOperator String  
              | JSPropertyNameandValue JSNode [JSNode]
              | JSRegEx String  
              | JSReturn [JSNode]
              | JSSourceElements [JSNode]
              | JSStringLiteral Char String  
              | JSSwitch JSNode [JSNode]
              | JSThrow JSNode  
              | JSTry JSNode [JSNode]  
              | JSUnary String  
              | JSVarDecl JSNode [JSNode]
              | JSVariables String [JSNode]  
              | JSWhile JSNode JSNode
              | JSWith JSNode [JSNode]
    deriving (Show, Eq, Read, Data, Typeable)

data JSType = JS_SCRIPT | JS_BLOCK | JS_LABEL | JS_FOR_IN | JS_CALL | JS_NEW_WITH_ARGS
            | JS_INDEX | JS_ARRAY_INIT | JS_OBJECT_INIT | JS_PROPERTY_INIT | JS_GETTER
            | JS_SETTER | JS_GROUP | JS_LIST
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


-- Do not use the lexer, it is greedy and consumes subsequent symbols, 
--   e.g. "!" in a==!b
rOp :: [Char] -> GenParser Char st ()
rOp x = try(rOp'' x)

rOp'' :: [Char] -> CharParser st ()
rOp'' []     = fail "trying to parse empty token"
rOp'' [x]    = do{ _ <- char x; optional whiteSpace; return () }
rOp'' (x:xs) = do{ _ <- char x; rOp xs;}
                 
-- ---------------------------------------------------------------------

-- Need to deal with the following cases
-- 1. Missing semi, because following } => empty
-- 2. Additional semi, with following } => empty
-- 3. semi with no following }          => semi

-- TODO: change the return to [JSNode], and get rid of the empty JSLiteral
autoSemi :: GenParser Char st JSNode
autoSemi = try (do { rOp ";"; lookAhead (rOp "}");
                     return (JSLiteral "");})
           <|> try (do{ rOp ";"; 
                        return (JSLiteral ";");})
           <|> try (do {lookAhead (rOp "}");
                        return (JSLiteral "");})

autoSemi' :: GenParser Char st JSNode
autoSemi' = try (do { rOp ";"; lookAhead (rOp "}");
                     return (JSLiteral "");})
           <|> try (do{ rOp ";"; 
                        return (JSLiteral ";");})

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

-- ---------------------------------------------------------------------
-- From HJS

regex :: GenParser Char st [Char]
regex = do { _ <- char '/'; body <- do { c <- firstchar; cs <- many otherchar; return $ concat (c:cs) }; _ <- char '/'; 
             flg <- identPart; return $ ("/"++body++"/"++flg) }

firstchar :: GenParser Char st [Char]
firstchar = do { c <- satisfy (\c -> isPrint c && c /= '*' && c /= '\\' && c /= '/'); return [c]} <|> escapeseq

escapeseq :: GenParser Char st [Char]
escapeseq = do { _ <- char '\\'; c <- satisfy isPrint; return ['\\',c]}

otherchar :: GenParser Char st [Char]
otherchar = do { c <- satisfy (\c -> isPrint c && c /= '\\' && c /= '/'); return [c]} <|> escapeseq

identPart :: GenParser Char st [Char]
identPart = many letter

-- ---------------------------------------------------------------------


-- TODO: do this properly, it is late now, want to move on
regExp :: GenParser Char st JSNode
{-
regExp = do { rOp "/"; v1 <- many (noneOf "/"); rOp "/"; v2 <- optional (oneOf "gim"); -- TODO: remove optional
              return (JSRegEx ("/" ++ v1 ++ "/" ++ (show(v2))))  }
-}
regExp = do { v1 <- regex;       
              return (JSRegEx v1)}

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
                   return (JSLiteral "null")}

-- <Boolean Literal> ::= 'true'
--                     | 'false'
booleanLiteral :: GenParser Char st JSNode
booleanLiteral = do{ reserved "true" ; 
                     return (JSLiteral "true")}
             <|> do{ reserved "false"; 
                     return (JSLiteral "false")}

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
numericLiteral :: GenParser Char st JSNode
numericLiteral = do {val <- decimalLiteral; 
                     return (JSDecimal val)}
             <|> do {val <- hexIntegerLiteral; 
                     return (JSHexInteger val)}


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
arrayLiteral :: GenParser Char st JSNode
arrayLiteral = do {rOp "["; v1 <- many (do { rOp ","; return [(JSElision [])]} <|> assignmentExpression); rOp "]";
                   return (JSArrayLiteral (flatten v1)) }

{-
arrayLiteral :: GenParser Char st JSNode
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
elision :: GenParser Char st JSNode
elision = do{ rOp ",";
              return (JSElision [])}
      <|> do{ v1 <- elision; rOp ",";
              return (JSElision [v1])}
    

-- <Element List> ::= <Elision> <Assignment Expression>
--                  | <Element List> ',' <Elision>  <Assignment Expression>
--                  | <Element List> ',' <Assignment Expression>
--                  | <Assignment Expression>
elementList :: GenParser Char st [JSNode]
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
objectLiteral :: GenParser Char st JSNode
objectLiteral = do{ rOp "{"; val <- propertyNameandValueList; rOp "}"; 
                   return (JSObjectLiteral val)}

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>
propertyNameandValueList :: GenParser Char st [JSNode]
propertyNameandValueList = do{ val <- sepBy1 propertyNameandValue (rOp ",");
                               return val}
                           
propertyNameandValue :: GenParser Char st JSNode
propertyNameandValue = do{ v1 <- propertyName; rOp ":"; v2 <- assignmentExpression;
                           return (JSPropertyNameandValue v1 v2)}

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
                        return (((JSLiteral "new "):v1)++[v2])}) -- xxxx
                <|> memberExpression'

memberExpression' :: GenParser Char st [JSNode]
memberExpression' = try(do{v1 <- primaryExpression; v2 <- rest;
                        return (v1:v2)})
                <|> try(do{v1 <- functionExpression; v2 <- rest;
                        return (v1:v2)})

                where
                  rest = do{ rOp "["; v1 <- expression; rOp "]"; v2 <- rest;
                             return [JSMemberSquare (v1:v2)]}
                     <|> do{ rOp "."; v1 <- identifier ; v2 <- rest;
                             return [JSMemberDot (v1:v2)]}
                     <|> return []
                         


-- <New Expression> ::= <Member Expression>
--                    | new <New Expression>
newExpression :: GenParser Char st [JSNode]
newExpression = memberExpression
           <|> do{ reserved "new"; val <- newExpression;
                   return ((JSLiteral "new "):val)}

-- <Call Expression> ::= <Member Expression> <Arguments>
--                     | <Call Expression> <Arguments> 
--                     | <Call Expression> '[' <Expression> ']'
--                     | <Call Expression> '.' Identifier

callExpression :: GenParser Char st [JSNode]
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


-- <Arguments> ::= '(' ')'
--               | '(' <Argument List> ')'
arguments :: GenParser Char st JSNode
arguments = try(do{ rOp "(";  rOp ")";
                return (JSArguments [[]])})
        <|> do{ rOp "("; v1 <- argumentList; rOp ")";
                return (JSArguments v1)}

-- <Argument List> ::= <Assignment Expression>
--                   | <Argument List> ',' <Assignment Expression>
argumentList :: GenParser Char st [[JSNode]]
argumentList = do{ vals <- sepBy1 assignmentExpression (rOp ",");
                   --return (flatten vals)}
                   return vals}


-- <Left Hand Side Expression> ::= <New Expression> 
--                               | <Call Expression>
leftHandSideExpression :: GenParser Char st [JSNode]
leftHandSideExpression = try (callExpression)
                     <|> newExpression
                     <?> "leftHandSideExpression"


-- <Postfix Expression> ::= <Left Hand Side Expression>
--                        | <Postfix Expression> '++'
--                        | <Postfix Expression> '--'
--TODO: put in the recursive part here, with a 'rest' clause
postfixExpression :: GenParser Char st [JSNode]
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
--TODO: put in the recursive part here, with a 'rest' clause
unaryExpression :: GenParser Char st [JSNode]
unaryExpression = do{ v1 <- postfixExpression; 
                      return v1}
              <|> do{ reserved "delete"; v1 <- unaryExpression;
                      return ((JSUnary "delete"):v1)}
              <|> do{ reserved "void";   v1 <- unaryExpression;
                      return ((JSUnary "void"):v1)}
              <|> do{ reserved "typeof"; v1 <- unaryExpression; 
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
multiplicativeExpression :: GenParser Char st [JSNode]
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
additiveExpression :: GenParser Char st [JSNode]
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
shiftExpression :: GenParser Char st [JSNode]
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
relationalExpression :: GenParser Char st [JSNode]
relationalExpression = do{ v1 <- shiftExpression; v2 <- rest;
                           return (v1++v2)}
                       where
                         rest =
                                 do{ rOp "<"; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary "<" v2 v3)]}
                             <|> do{ rOp ">"; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary ">" v2 v3)]}
                             <|> do{ rOp "<="; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary "<=" v2 v3)]}
                             <|> do{ rOp ">="; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary ">=" v2 v3)]}
                             <|> do{ reserved "instanceof"; v2 <- shiftExpression; v3 <- rest;
                                     return [(JSExpressionBinary "instanceof" v2 v3)]}
                             <|> return []



-- <Equality Expression> ::= <Relational Expression>
--                         | <Equality Expression> '==' <Relational Expression>
--                         | <Equality Expression> '!=' <Relational Expression>
--                         | <Equality Expression> '===' <Relational Expression>
--                         | <Equality Expression> '!==' <Relational Expression>
equalityExpression :: GenParser Char st [JSNode]
equalityExpression = do{ v1 <- relationalExpression; v2 <- rest;
                         return (v1++v2)}
                         -- TODO: more efficient parsing here, without all the backtracking
                     where
                       rest =
                              try(do{ rOp "=="; v2 <- relationalExpression; v3 <- rest;
                                      return [(JSExpressionBinary "==" v2 v3)]})
                          <|> try(do{ rOp "!="; v2 <- relationalExpression; v3 <- rest;
                                      return [(JSExpressionBinary "!=" v2 v3)]})
                          <|> try(do{ rOp "==="; v2 <- relationalExpression; v3 <- rest;
                                      return [(JSExpressionBinary "===" v2 v3)]})
                          <|> try(do{ rOp "!=="; v2 <- relationalExpression; v3 <- rest;
                                      return [(JSExpressionBinary "!==" v2 v3)]})
                          <|> return []
                        

-- <Bitwise And Expression> ::= <Equality Expression>
--                            | <Bitwise And Expression> '&' <Equality Expression>
bitwiseAndExpression :: GenParser Char st [JSNode]
bitwiseAndExpression = do{ v1 <- equalityExpression; v2 <- rest;
                           return (v1++v2)}
                       where
                         rest =
                                do{ rOp "&"; v2 <- equalityExpression; v3 <- rest;
                                    return [(JSExpressionBinary "&" v2 v3)]}
                             <|> return []


-- <Bitwise XOr Expression> ::= <Bitwise And Expression>
--                            | <Bitwise XOr Expression> '^' <Bitwise And Expression>
bitwiseXOrExpression :: GenParser Char st [JSNode]
bitwiseXOrExpression = do{ v1 <- bitwiseAndExpression; v2 <- rest;
                           return (v1++v2)}
                       where
                         rest =
                                do{ rOp "^"; v2 <- bitwiseAndExpression; v3 <- rest;
                                    return [(JSExpressionBinary "^" v2 v3)]}
                            <|> return []


-- <Bitwise Or Expression> ::= <Bitwise XOr Expression>
--                           | <Bitwise Or Expression> '|' <Bitwise XOr Expression>
bitwiseOrExpression :: GenParser Char st [JSNode]
bitwiseOrExpression = do{ v1 <- bitwiseXOrExpression; v2 <- rest;
                          return (v1++v2)}
                      where
                        rest =
                               do{ rOp "|"; v2 <- bitwiseXOrExpression; v3 <- rest;
                                   return [(JSExpressionBinary "|" v2 v3)]}
                           <|> return []



-- <Logical And Expression> ::= <Bitwise Or Expression>
--                            | <Logical And Expression> '&&' <Bitwise Or Expression>
logicalAndExpression :: GenParser Char st [JSNode]
logicalAndExpression = do{ v1 <- bitwiseOrExpression; v2 <- rest;
                           return (v1++v2)}
                       where
                         rest =
                               do{ rOp "&&"; v2 <- bitwiseOrExpression; v3 <- rest;
                                   return [(JSExpressionBinary "&&" v2 v3)]}
                            <|> return []




-- <Logical Or Expression> ::= <Logical And Expression>
--                           | <Logical Or Expression> '||' <Logical And Expression>
logicalOrExpression :: GenParser Char st [JSNode]
logicalOrExpression =  do{ v1 <- logicalAndExpression; v2 <- rest;
                           return (v1++v2)}
                       where
                         rest =
                               do{ rOp "||"; v2 <- logicalAndExpression; v3 <- rest;
                                   return [(JSExpressionBinary "||" v2 v3)]}
                            <|> return []


-- <Conditional Expression> ::= <Logical Or Expression> 
--                            | <Logical Or Expression> '?' <Assignment Expression> ':' <Assignment Expression>
conditionalExpression :: GenParser Char st [JSNode]
conditionalExpression = do{ v1 <- logicalOrExpression;
                            do {
                                 do{ rOp "?"; v2 <- assignmentExpression; rOp ":"; v3 <- assignmentExpression;
                                     return [(JSExpressionTernary v1 v2 v3)]}
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
statement = statementBlock
        <|> try(labelledStatement)
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
        <|> switchStatement
        <|> throwStatement 
        <|> tryStatement
        <?> "statement"


statementBlock :: GenParser Char st29 JSNode
statementBlock = do {v1 <- statementBlock'; return (if (v1 == []) then (JSLiteral ";") else (head v1))}

-- <Block > ::= '{' '}'
--            | '{' <Statement List> '}'
statementBlock' :: GenParser Char st [JSNode]
statementBlock' = try (do {rOp "{"; rOp "}"; 
                          return []})
                  <|> do {rOp "{"; val <- statementList; rOp "}"; 
                          return (if (val == ([JSLiteral ";"])) then ([]) else [(JSBlock val)])}
                  <?> "statementBlock"

-- <Block > ::= '{' '}'
--            | '{' <Statement List> '}'
block :: GenParser Char st JSNode
block = try (do {rOp "{"; rOp "}"; 
            return (JSBlock [])})
    <|> do {rOp "{"; val <- statementList; rOp "}"; 
            return (JSBlock val)}
    <?> "block"


-- <Statement List> ::= <Statement>
--                    | <Statement List> <Statement>
statementList :: GenParser Char st [JSNode]
statementList = many1 statement

-- <Variable Statement> ::= var <Variable Declaration List> ';'
-- Note: Mozilla introduced const declarations, not part of official spec
variableStatement :: GenParser Char st JSNode
variableStatement = do {reserved "var"; val <- variableDeclarationList;
                        return (JSVariables "var" val)}
                <|> do {reserved "const"; val <- variableDeclarationList;
                        return (JSVariables "const" val)}

                    
-- <Variable Declaration List> ::= <Variable Declaration>
--                               | <Variable Declaration List> ',' <Variable Declaration>
variableDeclarationList :: GenParser Char st [JSNode]
variableDeclarationList = do{ val <- sepBy1 variableDeclaration (rOp ","); 
                              return val }


-- <Variable Declaration> ::= Identifier
--                          | Identifier <Initializer>
variableDeclaration :: GenParser Char st JSNode
variableDeclaration = do{ v1 <- identifier; 
                          do {
                            do {v2 <- initializer; 
                                return (JSVarDecl v1 v2)}
                            <|> return (JSVarDecl v1 [])
                            }
                          }

-- <Initializer> ::= '=' <Assignment Expression>
initializer :: GenParser Char st [JSNode]
initializer = do {rOp "="; val <- assignmentExpression; 
                  return val}

-- <Empty Statement> ::= ';'
emptyStatement :: GenParser Char st JSNode
--emptyStatement = do { v1 <- autoSemi'; return (JSEmpty v1)}
emptyStatement = do { v1 <- autoSemi'; return v1}


-- <If Statement> ::= 'if' '(' <Expression> ')' <Statement> 
ifStatement :: GenParser Char st JSNode
ifStatement = do{ reserved "if"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement;
                  return (if (v2 == (JSLiteral ";")) then (JSIf v1 (JSLiteral "")) else (JSIf v1 v2)) }

-- <If Else Statement> ::= 'if' '(' <Expression> ')' <Statement> 'else' <Statement>
ifElseStatement :: GenParser Char st JSNode
ifElseStatement = do{ reserved "if"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement; reserved "else"; v3 <- statement ;
                      return (JSIfElse v1 v2 v3) }


-- <Iteration Statement> ::= 'do' <Statement> 'while' '(' <Expression> ')' ';'
--                         | 'while' '(' <Expression> ')' <Statement> 
--                         | 'for' '(' <Expression> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration List> ';' <Expression> ';' <Expression> ')' <Statement> 
--                         | 'for' '(' <Left Hand Side Expression> in <Expression> ')' <Statement> 
--                         | 'for' '(' 'var' <Variable Declaration> in <Expression> ')' <Statement> 
iterationStatement :: GenParser Char st JSNode
iterationStatement = do{ reserved "do"; v1 <- statement; reserved "while"; rOp "("; v2 <- expression; rOp ")"; v3 <- autoSemi ; 
                         return (JSDoWhile v1 v2 v3)}
                 <|> do{ reserved "while"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement; 
                         return (JSWhile v1 v2)}
                 <|> try(do{ reserved "for"; rOp "("; reserved "var"; v1 <- variableDeclarationList; rOp ";"; v2 <- optionalExpression ";"; 
                         v3 <- optionalExpression ")"; v4 <- statement; 
                         return (JSForVar v1 v2 v3 v4)})
                 <|> try(do{ reserved "for"; rOp "("; v1 <- expression; rOp ";"; v2 <- optionalExpression ";"; 
                         v3 <- optionalExpression ")"; v4 <- statement; 
                         return (JSFor v1 v2 v3 v4)})
                 <|> try(do{ reserved "for"; rOp "("; v1 <- leftHandSideExpression; reserved "in"; v2 <- expression; rOp ")"; 
                         v3 <- statement; 
                         return (JSForIn v1 v2 v3)})
                 <|> do{ reserved "for"; rOp "("; reserved "var"; v1 <- variableDeclaration; reserved "in"; 
                         v2 <- expression; rOp ")"; v3 <- statement; 
                         return (JSForVarIn v1 v2 v3)}
                     
optionalExpression :: [Char] -> GenParser Char st38 [JSNode]
optionalExpression s = do { rOp s; 
                            return []}
                   <|> do { v1 <- expression; rOp s ;
                            return [v1]}

-- <Continue Statement> ::= 'continue' ';'
--                        | 'continue' Identifier ';'
continueStatement :: GenParser Char st JSNode
continueStatement = do {reserved "continue"; v1 <- autoSemi; 
                        return (JSContinue [v1])}
                <|> do {reserved "continue"; v1 <- identifier; v2 <- autoSemi; 
                        return (JSContinue [v1,v2])}


-- <Break Statement> ::= 'break' ';'
--                        | 'break' Identifier ';'
breakStatement :: GenParser Char st JSNode
breakStatement = do {reserved "break"; 
                     do {
                          do {v1 <- autoSemi; 
                              return (if (v1 == JSLiteral "") then (JSBreak []) else (JSBreak [v1]))}
                     <|>  do {v1 <- identifier; v2 <- autoSemi; 
                              return (JSBreak [v1,v2])}
                        }
                     }


-- <Return Statement> ::= 'return' ';'
--                        | 'return' <Expression> ';'
returnStatement :: GenParser Char st JSNode
returnStatement = do {reserved "return"; 
                      do{
                            do {v1 <- autoSemi; return (JSReturn [v1])}
                        <|> do {v1 <- expression; v2 <- autoSemi; 
                                return (JSReturn [v1,v2])}
                        }
                      }


-- <With Statement> ::= 'with' '(' <Expression> ')' <Statement> ';'
withStatement :: GenParser Char st JSNode
withStatement = do{ reserved "with"; rOp "("; v1 <- expression; rOp ")"; v2 <- statement; v3 <- autoSemi; 
                    return (JSWith v1 [v2,v3])}


-- <Switch Statement> ::= 'switch' '(' <Expression> ')' <Case Block>  
switchStatement :: GenParser Char st JSNode
switchStatement = do{ reserved "switch"; rOp "("; v1 <- expression; rOp ")"; v2 <- caseBlock;
                      return (JSSwitch v1 v2)}

-- <Case Block> ::= '{' '}'
--                | '{' <Case Clauses> '}'
--                | '{' <Case Clauses> <Default Clause> '}'
--                | '{' <Case Clauses> <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> <Case Clauses> '}'
--                | '{' <Default Clause> '}'
caseBlock :: GenParser Char st [JSNode]
-- TODO: get rid of the try clauses by unwinding this
caseBlock = try(do{ rOp "{"; rOp "}"; 
                return []})
        <|> try(do{ rOp "{"; v1 <- caseClauses; rOp "}"; 
                return v1})
        <|> try(do{ rOp "{"; v1 <- caseClauses; v2 <- defaultClause; rOp "}"; 
                return (v1++[v2])})
        <|> try(do{ rOp "{"; v1 <- caseClauses; v2 <- defaultClause; v3 <- caseClauses; rOp "}"; 
                return (v1++[v2]++v3)})
        <|> try(do{ rOp "{"; v1 <- defaultClause; v2 <- caseClauses; rOp "}"; 
                return (v1:v2)})
        <|> do{ rOp "{"; v1 <- defaultClause; rOp "}"; 
                return [v1]}


-- <Case Clauses> ::= <Case Clause>
--                  | <Case Clauses> <Case Clause>
caseClauses :: GenParser Char st [JSNode]
caseClauses = do{ val <- many1 caseClause;
                  return val}

-- <Case Clause> ::= 'case' <Expression> ':' <Statement List>
--                 | 'case' <Expression> ':'
caseClause :: GenParser Char st JSNode
caseClause = do { reserved "case"; v1 <- expression; rOp ":"; 
                  do {
                       do { v2 <- statementList;
                            return (JSCase v1 v2)}
                   <|> return (JSCase v1 [])
                     }
                  }

-- <Default Clause> ::= 'default' ':' 
--                    | 'default' ':' <Statement List>
defaultClause :: GenParser Char st JSNode
defaultClause = do{ reserved "default"; rOp ":"; 
                    return (JSDefault [])}
            <|> do{ reserved "default"; rOp ":"; v1 <- statementList;
                    return (JSDefault v1)}

-- <Labelled Statement> ::= Identifier ':' <Statement> 
labelledStatement :: GenParser Char st JSNode
labelledStatement = do { v1 <- identifier; rOp ":"; v2 <- statement;
                         return (JSLabelled v1 v2)}

-- <Throw Statement> ::= 'throw' <Expression>
throwStatement :: GenParser Char st JSNode
throwStatement = do{ reserved "throw"; val <- expression;
                     return (JSThrow val)}

-- <Try Statement> ::= 'try' <Block> <Catch>
--                   | 'try' <Block> <Finally>
--                   | 'try' <Block> <Catch> <Finally>
tryStatement :: GenParser Char st JSNode
tryStatement = do{ reserved "try"; v1 <- block; v2 <- catch;
                   return (JSTry v1 [v2])}
           <|> do{ reserved "try"; v1 <- block; v2 <- finally;
                   return (JSTry v1 [v2])}
           <|> do{ reserved "try"; v1 <- block; v2 <- catch; v3 <- finally;
                   return (JSTry v1 [v2,v3])}

-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
catch :: GenParser Char st JSNode
catch = do{ reserved "catch"; rOp "("; v1 <- identifier; rOp ")"; v2 <- block;
            return (JSCatch v1 v2)}

-- <Finally> ::= 'finally' <Block>
finally :: GenParser Char st JSNode
finally = do{ reserved "finally"; v1 <- block;
            return (JSFinally v1)}

-- <Function Declaration> ::= 'function' Identifier '(' <Formal Parameter List> ')' '{' <Function Body> '}'
--                          | 'function' Identifier '(' ')' '{' <Function Body> '}'
functionDeclaration :: GenParser Char st JSNode
functionDeclaration = do {reserved "function"; v1 <- identifier; rOp "("; v2 <- formalParameterList; rOp ")"; 
                          v3 <- functionBody; 
                          return (JSFunction v1 v2 v3) } 
                  <?> "functionDeclaration"


-- <Function Expression> ::= 'function' '(' ')' '{' <Function Body> '}'
---                        | 'function' '(' <Formal Parameter List> ')' '{' <Function Body> '}'
functionExpression :: GenParser Char st JSNode
functionExpression = do{ reserved "function"; rOp "("; 
                         do {
                           do { rOp ")"; v2 <- functionBody; 
                                return (JSFunctionExpression [] v2)}
                           <|> do {v1 <- formalParameterList; rOp ")"; v2 <- functionBody; 
                                   return (JSFunctionExpression v1 v2)}
                           }
                         }

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
program = do {whiteSpace; val <- sourceElements; eof; return val}


-- <Source Elements> ::= <Source Element>
--                     | <Source Elements>  <Source Element>
sourceElements :: GenParser Char st JSNode
sourceElements = do{ val <- many1 sourceElement;
                     return (JSSourceElements (fixSourceElements val))}
                 

-- <Source Element> ::= <Statement>
--                    | <Function Declaration>
sourceElement :: GenParser Char st JSNode
sourceElement = functionDeclaration
            <|> statement
            <?> "sourceElement"

-- ---------------------------------------------------------------
-- Utility stuff

-- TODO: move this into the Pretty printer, to keep the Parse true to the source, so it can be reused
-- Make sure every alternate part is a JSLiteral ";", but not the last one
fixSourceElements :: [JSNode] -> [JSNode]
fixSourceElements xs = myIntersperse (JSLiteral ";") $ filter (\x -> JSLiteral "" /= x) $ filter (\x -> JSLiteral ";" /= x) xs
  
myIntersperse :: JSNode -> [JSNode] -> [JSNode]
myIntersperse _   []      = []
myIntersperse _   [x]     = [x]
myIntersperse sep (x:(JSFunction v1 v2 v3):xs)  = x : (JSLiteral "\n") : (JSFunction v1 v2 v3) : sep : myIntersperse sep xs
myIntersperse sep (x:xs)  = x : sep : myIntersperse sep xs
                       
                       
-- ---------------------------------------------------------------
-- Testing

m :: IO ()
m = do args <- getArgs
       putStrLn (show $ readJs (args !! 0))


-- ---------------------------------------------------------------------

flatten :: [[a]] -> [a]
flatten xs = foldl' (++) [] xs

-- ---------------------------------------------------------------------

main :: IO ()
main =
  do args <- getArgs
     x <- readFile (args !! 0)
     putStrLn (show $ doParse program x)            
    
-- ---------------------------------------------------------------------
          
--readJs :: String -> String
readJs :: [Char] -> JSNode
readJs input = case parse (p' program) "js" input of
    Left err -> error ("Parse failed:" ++ show err)
    Right val -> val

-- ---------------------------------------------------------------------
    
doParse :: (Show tok) => GenParser tok () a -> [tok] -> a
doParse p input = case parse (p' p) "js" input of
    Left err -> error ("Parse failed:" ++ show err)
    Right val -> val

-- ---------------------------------------------------------------------
    
p' :: (Show tok) => GenParser tok st b -> GenParser tok st b
p' p = do {val <- p; eof; return val}

-- ---------------------------------------------------------------------

showFile :: FilePath -> IO String
showFile filename =
  do 
     x <- readFile (filename)
     return $ (show x)

-- ---------------------------------------------------------------------

parseFile :: FilePath -> IO JSNode
parseFile filename =
  do 
     x <- readFile (filename)
     return $ (readJs x)

-- EOF
