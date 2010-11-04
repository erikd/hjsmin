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
    , newJSPState  
    , JSPState  
    , lexeme  
    -- Testing  
    --, simpleSpace  
    , nlPrior  
    ) where

-- ---------------------------------------------------------------------

--import Text.ParserCombinators.Parsec hiding (Line)
import Data.Attoparsec
import Control.Applicative ( (<|>) )

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U

import Data.Char ( toLower, digitToInt, isAlphaNum, isAlpha, isDigit, isHexDigit, chr )
import Data.List ( nub, sort )


-- ---------------------------------------------------------------------
-- This bit from HJS Prim.hs

data JSPState = JSPState {nlFlag::Bool}

newJSPState :: JSPState
newJSPState = JSPState { nlFlag = False }

--clearNLFlag = updateState (\x -> x { nlFlag=False })
clearNLFlag = undefined

--setNLFlag   = updateState (\x -> x { nlFlag=True })
setNLFlag   = undefined

--getNLFlag   = do s <- getState; return $ nlFlag s                   
getNLFlag   = undefined



nlPrior = do { s <- getNLFlag; if s then (return ()) else (fail "no parse") }


-- ---------------------------------------------------------------------

-- Do not use the lexer, it is greedy and consumes subsequent symbols, 
--   e.g. "!" in a==!b
rOp :: String -> Parser B.ByteString
rOp x = string (U.fromString x)

{-
rOp'' []     = fail "trying to parse empty token"
rOp'' [x]    = do{ _ <- char x; 
                   do {
                       do {_ <- whiteSpace; return () }
                       <|> return ()
                       }
                   }

rOp'' (x:xs) = do{ _ <- char x; rOp xs;}
  -}
               
-- ---------------------------------------------------------------------

-- Need to deal with the following cases
-- 1. Missing semi, because following } => empty
-- 2. Additional semi, with following } => empty
-- 3. semi with no following }          => semi
-- 4. no semi, but prior NL             => semi

autoSemi = do{ rOp ";"; return (";");}
{-
autoSemi = try (do { rOp ";"; lookAhead (rOp "}");
                     return ("");})
           <|> try (do{ rOp ";"; 
                        return (";");})
           <|> try (do {lookAhead (rOp "}");
                        return ("");})
           <|> try (do {nlPrior;
                        return ";/*NLPRIOR*/"})
-}


autoSemi' = do{ rOp ";"; return (";");}
{-
autoSemi' = try (do { rOp ";"; lookAhead (rOp "}");
                     return ("");})
           <|> try (do{ rOp ";"; 
                        return (";");})
-}

-- ---------------------------------------------------------------------

{-
lexer :: P.TokenParser st
lexer = P.makeTokenParser javascriptDef
-}      


--identifier = lexeme $ many1 (letter <|> oneOf "_")
identifier =
        lexeme $ try $
        do{ name <- ident
          ; if (isReservedName name)
             then fail ("reserved word " ++ show name)
             else return name
          }

ident
        = do{ c <- identStart
            ; cs <- many identLetter
            ; return (c:cs)
            }
        <?> "identifier"

--isReservedName :: [Char] -> Bool
isReservedName name
        = isReserved theReservedNames caseName
        where
          caseName      | caseSensitive  = name
                        | otherwise      = map toLower name


--isReserved :: (Ord t) => [t] -> t -> Bool
isReserved names name
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case (compare r name) of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

theReservedNames :: [[Char]]
theReservedNames
        | caseSensitive = sortedNames
        | otherwise     = map (map toLower) sortedNames
        where
          sortedNames   = sort reservedNames

reserved name = lexeme $ myString name
{-
reserved name =
        lexeme $ try $
        do{ _ <- string name
          ; notFollowedBy identLetter <?> ("end of " ++ show name)
          }
-}

--whiteSpace = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
--whiteSpace = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <|> do { _ <- char '\n'; setNLFlag} <?> "")
whiteSpace = skipMany (do { _ <- myString "\n"; setNLFlag; return ()} <|> simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
-- whiteSpace = try $ many $ (do { equal TokenWhite } <|> do { (equal TokenNL); setNLFlag})


--simpleSpace = skipMany1 (satisfy isSpace)
simpleSpace  = skipMany1 (satisfy (\c -> isSpace c && c /= 13)) -- From HJS


oneLineComment =
  do{ _ <- try (myString commentLine)
    ; skipMany (satisfy (/= 13))
    ; return ()
    }

multiLineComment =
  do { _ <- try (myString commentStart)
     ; inComment
     }

inComment
  | nestedComments = inCommentMulti
  | otherwise      = inCommentSingle

inCommentMulti
        =   do{ _ <- try (myString commentEnd)         ; return () }
        <|> do{ multiLineComment                     ; inCommentMulti }
        <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
        <|> do{ _ <- oneOf startEnd                  ; inCommentMulti }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd ++ commentStart)

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

identLetter = alphaNum <|> oneOf "_"

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
                

decimal = lexeme $ number 10 digit

hexadecimal = lexeme $ do{ _ <- oneOf "xX"; number 16 hexDigit }

{-
octal :: GenParser Char JSPState Integer
octal           = do{ _ <- oneOf "oO"; number 8 octDigit  }
-}

number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }


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
lexeme p = do{ x <- p; clearNLFlag; whiteSpace; return x  }

-- ---------------------------------------------------------------------
-- Stuff from Parsec needed to make Attoparsec work


-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
-- 
-- >   vowel  = oneOf "aeiou"

--oneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
oneOf cs            = satisfy (\c -> B.elem c cs') where cs' = U.fromString cs


-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"

--noneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
noneOf cs           = satisfy (\c -> not (B.elem c cs')) where cs' = U.fromString cs

-- | Parses a letter or digit (a character between \'0\' and \'9\').
-- Returns the parsed character. 

--alphaNum :: (Stream s m Char => ParsecT s u m Char)
alphaNum            = satisfy isAlphaNum    <?> "letter or digit"

-- | Parses a letter (an upper case or lower case character). Returns the
-- parsed character. 

--letter :: (Stream s m Char) => ParsecT s u m Char
letter              = satisfy isAlpha       <?> "letter"

-- | Parses a digit. Returns the parsed character. 

--digit :: (Stream s m Char) => ParsecT s u m Char
digit               = satisfy isDigit       <?> "digit"

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character. 

--hexDigit :: (Stream s m Char) => ParsecT s u m Char
hexDigit            = satisfy isHexDigit    <?> "hexadecimal digit"

myString s = string (U.fromString s)

isSpace c = c == 20

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
