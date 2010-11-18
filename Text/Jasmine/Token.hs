module Text.Jasmine.Token
    (
      identifier  
    , reserved  
    , whiteSpace  
    , dec  
    , hex  
    , autoSemi  
    , autoSemi'  
    , rOp  
    , lChar  
    , lexeme  
    -- Testing  
    , isAlpha  
    ) where

-- ---------------------------------------------------------------------

import Control.Applicative ( (<|>) )
import Data.Attoparsec.Char8 (hexadecimal, decimal, char, Parser, satisfy, try, many, (<?>), skipMany, skipMany1, isDigit, string, skipWhile, anyChar)
import Data.Char ( isAlpha, isSpace )
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as S8

-- ---------------------------------------------------------------------

-- Do not use the lexer, it is greedy and consumes subsequent symbols, 
--   e.g. "!" in a==!b
rOp :: String -> Parser ()
rOp x = lexeme $ do { _ <- string $ S8.pack x; return () }

lChar :: Char -> Parser ()
lChar x = lexeme $ do { _ <- char x; return () }
               
-- ---------------------------------------------------------------------

-- Need to deal with the following cases
-- 1. Missing semi, because following } => empty
-- 2. Additional semi, with following } => empty
-- 3. semi with no following }          => semi
-- 4. no semi, but prior NL             => semi

autoSemi :: Parser [Char]
autoSemi = do{ _ <- rOp ";"; return (";");}
          <|> return ("")


autoSemi' :: Parser [Char]
autoSemi' = do{ _ <- rOp ";"; return (";");}

-- ---------------------------------------------------------------------

--identifier = lexeme $ many1 (letter <|> oneOf "_")
identifier :: Parser String
identifier =
        lexeme $ try $
        do{ name <- ident
          ; if (isReservedName name)
             then fail ("reserved word " ++ show name)
             else return name
          }

ident :: Parser [Char]
ident
        = do{ c <- identStart
            ; cs <- many identLetter
            ; return (c:cs)
            }
        <?> "identifier"

isReservedName :: String -> Bool
isReservedName = flip Set.member reservedNames

-- TODO: fix trailing characters test          
reserved :: String -> Parser ()
reserved name = lexeme $ do { _ <- string $ S8.pack name; return () }
{-
reserved name =
        lexeme $ try $
        do{ _ <- string name
          ; notFollowedBy identLetter <?> ("end of " ++ show name)
          }
-}

--whiteSpace = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
--whiteSpace = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <|> do { _ <- char '\n'; setNLFlag} <?> "")
whiteSpace :: Parser ()
whiteSpace = skipMany (do { _ <- string $ S8.pack "\n"; return ()} <|> simpleSpace <|> comment <?> "")
-- whiteSpace = try $ many $ (do { equal TokenWhite } <|> do { (equal TokenNL); setNLFlag})


simpleSpace :: Parser ()
-- Note : Data.Char.isSpace includes a variety of non-breaking space chars, and unicode variants
simpleSpace  = skipMany1 $ (satisfy isSpace) 

comment :: Parser ()
comment = do
    isMulti <- try $ do
        _ <- char fslash
        (char asterisk >> return True) <|> (char fslash >> return False)
    if isMulti then inComment else skipWhile (/= linefeed)

inComment :: Parser ()
inComment =
    theEnd <|> (anyChar >> inComment) <?> "end of comment"
  where
    theEnd = char asterisk >> (char fslash >> return ()) <|> inComment

fslash, asterisk, linefeed :: Char
fslash = '/'
asterisk = '*'
linefeed = '\n'

identLetter :: Parser Char
identLetter = alphaNum <|> oneOf "_"

identStart :: Parser Char
identStart  = letter <|> oneOf "_$"

reservedNames :: Set.Set String
reservedNames = Set.fromAscList [ 
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


hex :: Parser Integer
hex = lexeme $ do{ _ <- oneOf "xX"; hexadecimal }

dec :: Parser Integer
dec = lexeme $ decimal

-- | @lexeme p@ first applies parser @p@ and than the 'whiteSpace'
-- parser, returning the value of @p@. Every lexical
-- token (lexeme) is defined using @lexeme@, this way every parse
-- starts at a point without white space. Parsers that use @lexeme@ are
-- called /lexeme/ parsers in this document.
-- 
-- The only point where the 'whiteSpace' parser should be
-- called explicitly is the start of the main parser in order to skip
-- any leading white space.
--
-- >    mainParser  = do{ whiteSpace
-- >                     ; ds <- many (lexeme digit)
-- >                     ; eof
-- >                     ; return (sum ds)
-- >                     }

lexeme :: Parser b -> Parser b
lexeme p = do{ x <- p; whiteSpace; return x  }

-- ---------------------------------------------------------------------
-- Stuff from Parsec needed to make Attoparsec work


-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
-- 
-- >   vowel  = oneOf "aeiou"

oneOf :: String -> Parser Char
oneOf = satisfy . flip elem


-- | Parses a letter or digit (a character between \'0\' and \'9\').
-- Returns the parsed character. 

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum    <?> "letter or digit"

-- | Parses a letter (an upper case or lower case character). Returns the
-- parsed character. 

letter :: Parser Char
letter = satisfy isAlpha       <?> "letter"

-- | Parses a digit. Returns the parsed character. 


-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character. 

isAlphaNum :: Char -> Bool
isAlphaNum c =  isAlpha c || isDigit c

-- ---------------------------------------------------------------------
{-
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
                , identStart = letter <|> oneOf "_"
                , identLetter = alphaNum <|> oneOf "_"
                               
                , caseSensitive  = True
		}
                
        
-}

-- EOF
