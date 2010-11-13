module Text.Jasmine.Token
    (
    --  TokenParser
      identifier  
    , reserved  
    , whiteSpace  
    , dec  
    , hex  
    , autoSemi  
    , autoSemi'  
    , rOp  
    , lexeme  
    -- Testing  
    --, simpleSpace  
    , oneLineComment  
    , isAlpha  
    ) where

-- ---------------------------------------------------------------------

import Control.Applicative ( (<|>) )
--import Data.Attoparsec ()
import Data.Attoparsec.Char8 (isSpace, hexadecimal, decimal, char, Parser, satisfy, try, many, (<?>), skipMany, skipMany1, isDigit)
import Data.Char ( isAlpha )
import Data.List ( nub, sort)
import qualified Data.Text as T

-- ---------------------------------------------------------------------

-- Do not use the lexer, it is greedy and consumes subsequent symbols, 
--   e.g. "!" in a==!b
rOp :: String -> Parser ()
rOp x = lexeme $ do { _ <- myString x; return () }

               
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
identifier :: Parser T.Text
identifier =
        lexeme $ try $
        do{ name <- ident
          ; if (isReservedName $ T.pack name)
             then fail ("reserved word " ++ show name)
             else return $ T.pack name
          }

ident :: Parser [Char]
ident
        = do{ c <- identStart
            ; cs <- many identLetter
            ; return (c:cs)
            }
        <?> "identifier"

isReservedName :: T.Text -> Bool
isReservedName name = isReserved theReservedNames name


isReserved :: [T.Text] -> T.Text -> Bool          
isReserved names name
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case (compare r name) of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

theReservedNames :: [T.Text]
theReservedNames = map T.pack $ sort reservedNames

-- TODO: fix trailing characters test          
reserved :: String -> Parser ()
reserved name = lexeme $ do { _ <- myString name; return () }
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
whiteSpace = skipMany (do { _ <- myString "\n"; return ()} <|> simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
-- whiteSpace = try $ many $ (do { equal TokenWhite } <|> do { (equal TokenNL); setNLFlag})


--simpleSpace = skipMany1 (satisfy isSpace)
simpleSpace :: Parser ()
simpleSpace  = skipMany1 (satisfy (\c -> isSpace c && c /= '\n')) -- From HJS


oneLineComment :: Parser ()
oneLineComment =
  do{ _ <- myString commentLine
    ; skipMany (satisfy (/= '\n'))
    -- ; word8 10 
    ; return ()
    }

multiLineComment :: Parser ()
multiLineComment =
  do { _ <- try (myString commentStart)
     ; inComment
     }

inComment :: Parser ()
inComment
  | nestedComments = inCommentMulti
  | otherwise      = inCommentSingle

inCommentMulti :: Parser ()
inCommentMulti
        =   do{ _ <- try (myString commentEnd)         ; return () }
        <|> do{ multiLineComment                     ; inCommentMulti }
        <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
        <|> do{ _ <- oneOf startEnd                  ; inCommentMulti }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd ++ commentStart)

inCommentSingle :: Parser ()
inCommentSingle
        =   do{ _ <- try (myString commentEnd)        ; return () }
        <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
        <|> do{ _ <- oneOf startEnd                 ; inCommentSingle }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd ++ commentStart)

commentStart :: [Char]
commentStart   = "/*"

commentEnd :: [Char]
commentEnd     = "*/"

commentLine :: [Char]
commentLine    = "//"

nestedComments :: Bool
nestedComments = True

identLetter :: Parser Char
identLetter = alphaNum <|> oneOf "_"

identStart :: Parser Char
identStart  = letter <|> oneOf "_$"

reservedNames :: [String]
reservedNames = [ 
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

--lexeme p = do{ x <- p;              whiteSpace; return x  }
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
oneOf cs = satisfy (\c -> elem c cs)


-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"

--noneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
noneOf :: String -> Parser Char
noneOf cs = satisfy (\c -> not (elem c cs))

-- | Parses a letter or digit (a character between \'0\' and \'9\').
-- Returns the parsed character. 

--alphaNum :: (Stream s m Char => ParsecT s u m Char)
alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum    <?> "letter or digit"

-- | Parses a letter (an upper case or lower case character). Returns the
-- parsed character. 

letter :: Parser Char
letter = satisfy isAlpha       <?> "letter"

-- | Parses a digit. Returns the parsed character. 


-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character. 

--hexDigit :: (Stream s m Char) => ParsecT s u m Char
--hexDigit :: Parser Word8
--hexDigit = satisfy isHexDigit    <?> "hexadecimal digit"

myString :: String -> Parser String
myString s = mapM (\c -> char c) s

isAlphaNum :: Char -> Bool
isAlphaNum c            =  isAlpha c || isDigit c

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
