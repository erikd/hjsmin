module Text.Jasmine.Token
    (
    --  TokenParser
      identifier  
    , reserved  
    , whiteSpace  
    , decimal  
    , hexadecimal  
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

--import Text.ParserCombinators.Parsec hiding (Line)
import Data.Attoparsec
import Control.Applicative ( (<|>) )

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U

import Data.Char ( toLower, digitToInt, chr, ord )
import Data.List ( nub, sort )
import Data.Word

-- ---------------------------------------------------------------------

-- Do not use the lexer, it is greedy and consumes subsequent symbols, 
--   e.g. "!" in a==!b
rOp :: String -> Parser ()
rOp x = lexeme $ do { _ <- string (U.fromString x); return () }

               
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
--identifier :: Parser [Word8]
identifier :: Parser B.ByteString
identifier =
        lexeme $ try $
        do{ name <- ident
          ; if (isReservedName $ B.pack name)
             then fail ("reserved word " ++ show name)
             else return $ B.pack name
          }

ident :: Parser [Word8]
ident
        = do{ c <- identStart
            ; cs <- many identLetter
            ; return (c:cs)
            }
        <?> "identifier"

--isReservedName :: [Char] -> Bool
isReservedName :: B.ByteString -> Bool
isReservedName name
        = isReserved theReservedNames caseName
        where
          caseName      | caseSensitive  = name
                        | otherwise      = B.pack $ map myToLower $ B.unpack name


--isReserved :: (Ord t) => [t] -> t -> Bool
isReserved :: [B.ByteString] -> B.ByteString -> Bool          
isReserved names name
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case (compare r name) of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

theReservedNames :: [B.ByteString]
theReservedNames
        | caseSensitive = map U.fromString sortedNames
        | otherwise     = map (\s -> U.fromString $ map toLower s) sortedNames
        where
          sortedNames   = sort reservedNames

--reserved :: String -> Parser U.ByteString
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
--simpleSpace  = skipMany1 (satisfy (\c -> isSpace c && c /= 13)) -- From HJS
simpleSpace  = skipMany1 (satisfy (\c -> isSpace c && c /= 10)) -- From HJS


oneLineComment :: Parser ()
oneLineComment =
  do{ _ <- myString commentLine
    ; skipMany (satisfy (/= 10))
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

caseSensitive :: Bool
caseSensitive = True

commentStart :: [Char]
commentStart   = "/*"

commentEnd :: [Char]
commentEnd     = "*/"

commentLine :: [Char]
commentLine    = "//"

nestedComments :: Bool
nestedComments = True

identLetter :: Parser Word8
identLetter = alphaNum <|> oneOf "_"

identStart :: Parser Word8
identStart  = letter <|> oneOf "_$"

reservedNames :: [[Char]]
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
                

decimal :: Parser Integer
decimal = lexeme $ number 10 digit

hexadecimal :: Parser Integer
hexadecimal = lexeme $ do{ _ <- oneOf "xX"; number 16 hexDigit }

{-
octal :: GenParser Char JSPState Integer
octal           = do{ _ <- oneOf "oO"; number 8 octDigit  }
-}

number :: Integer -> Parser Word8 -> Parser Integer
number base baseDigit
        = do{ digits <- many1 baseDigit
            -- ; let n = foldl (\x d -> base*x + toInteger (digitToInt (chr d))) 0 digits
            ; let n = foldl accumNum 0 digits
            ; seq n (return n)
            }
  where
    accumNum :: Integer -> Word8 -> Integer
    accumNum acc d = base*acc + toInteger (digitToInt (chr (fromIntegral d)))

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

--oneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
oneOf :: String -> Parser Word8
oneOf cs            = satisfy (\c -> B.elem c cs') where cs' = U.fromString cs


-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"

--noneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
noneOf :: String -> Parser Word8
noneOf cs           = satisfy (\c -> not (B.elem c cs')) where cs' = U.fromString cs

-- | Parses a letter or digit (a character between \'0\' and \'9\').
-- Returns the parsed character. 

--alphaNum :: (Stream s m Char => ParsecT s u m Char)
alphaNum :: Parser Word8
alphaNum            = satisfy isAlphaNum    <?> "letter or digit"

-- | Parses a letter (an upper case or lower case character). Returns the
-- parsed character. 

--letter :: (Stream s m Char) => ParsecT s u m Char
letter :: Parser Word8
letter              = satisfy isAlpha       <?> "letter"

-- | Parses a digit. Returns the parsed character. 

--digit :: (Stream s m Char) => ParsecT s u m Char
digit :: Parser Word8
digit = satisfy isDigit       <?> "digit"

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character. 

--hexDigit :: (Stream s m Char) => ParsecT s u m Char
hexDigit :: Parser Word8
hexDigit            = satisfy isHexDigit    <?> "hexadecimal digit"

myString :: String -> Parser U.ByteString
myString s = string (U.fromString s)

isSpace :: Word8 -> Bool
--isSpace c = c == 32
isSpace w = (w == 32) || (9 <= w && w <= 13)


-- The upper case ISO characters have the multiplication sign dumped
-- randomly in the middle of the range.  Go figure.
isUpper :: Word8 -> Bool
isUpper c'              =  c >= (ord 'A')    && c <= (ord 'Z') ||
                           c >= (ord '\xC0') && c <= (ord '\xD6') ||
                           c >= (ord '\xD8') && c <= (ord '\xDE')
                where c = fromIntegral c'

-- The lower case ISO characters have the division sign dumped
-- randomly in the middle of the range.  Go figure.
isLower :: Word8 -> Bool                      
isLower c'               =  c >= (ord 'a')    && c <= (ord 'z') ||
                           c >= (ord '\xDF') && c <= (ord '\xF6') ||
                           c >= (ord '\xF8') && c <= (ord '\xFF')
                where c = fromIntegral c'

isAlpha :: Word8 -> Bool
isAlpha c               =  isLower c || isUpper c

isAlphaNum :: Word8 -> Bool
isAlphaNum c            =  isAlpha c || isDigit c

-- | Selects ASCII digits, i.e. @\'0\'@..@\'9\'@.
isDigit :: Word8 -> Bool
isDigit c' =  c >= (ord '0') && c <= (ord '9')
          where c = fromIntegral c'

-- | Selects ASCII octal digits, i.e. @\'0\'@..@\'7\'@.
_isOctDigit :: Int -> Bool
_isOctDigit c            =  c >= (ord '0') && c <= (ord '7')

-- | Selects ASCII hexadecimal digits,
-- i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
--isHexDigit              :: Char -> Bool
--isHexDigit              :: Word8 -> Bool
isHexDigit :: Word8 -> Bool
isHexDigit c'            =  isDigit c' || c >= (ord 'A') && c <= (ord 'F') ||
                                          c >= (ord 'a') && c <= (ord 'f')
                where c = fromIntegral c'

myToLower :: Word8 -> Word8
myToLower c = fromIntegral $ ord $ toLower (chr (fromIntegral c))

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
