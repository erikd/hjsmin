module Text.Jasmine.Token
    (
    --  TokenParser
      identifier  
    , reserved  
    , whiteSpace  
    , decimal  
    , hexadecimal  
    ) where

-- ---------------------------------------------------------------------

import Text.ParserCombinators.Parsec hiding (Line)
import Data.Char ( isAlpha, toLower, toUpper, isSpace, digitToInt )
import Data.List ( nub, sort )

-- ---------------------------------------------------------------------

{-
lexer :: P.TokenParser st
lexer = P.makeTokenParser javascriptDef
-}      

identifier :: CharParser st String
--identifier = lexeme $ many1 (letter <|> oneOf "_")
identifier =
        lexeme $ try $
        do{ name <- ident
          ; if (isReservedName name)
             then unexpected ("reserved word " ++ show name)
             else return name
          }

ident
        = do{ c <- identStart
            ; cs <- many identLetter
            ; return (c:cs)
            }
        <?> "identifier"

isReservedName name
        = isReserved theReservedNames caseName
        where
          caseName      | caseSensitive  = name
                        | otherwise      = map toLower name


isReserved names name
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case (compare r name) of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

theReservedNames
        | caseSensitive = sortedNames
        | otherwise     = map (map toLower) sortedNames
        where
          sortedNames   = sort reservedNames


reserved :: String -> CharParser st ()
reserved name =
        lexeme $ try $
        do{ string name
          ; notFollowedBy identLetter <?> ("end of " ++ show name)
          }



whiteSpace :: CharParser st ()
whiteSpace
  | noLine && noMulti  = skipMany (simpleSpace <?> "")
  | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
  | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
  | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
  where
       --noLine  = null (commentLine languageDef)
       noLine = False
       --noMulti = null (commentStart languageDef)
       noMulti = False

simpleSpace :: GenParser Char st ()
simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment :: GenParser Char st ()
oneLineComment =
  do{ try (string commentLine)
    ; skipMany (satisfy (/= '\n'))
    ; return ()
    }

multiLineComment :: GenParser Char st ()
multiLineComment =
  do { try (string commentStart)
     ; inComment
     }

inComment :: GenParser Char st ()
inComment
  | nestedComments = inCommentMulti
  | otherwise      = inCommentSingle

inCommentMulti :: GenParser Char st ()
inCommentMulti
        =   do{ try (string commentEnd)              ; return () }
        <|> do{ multiLineComment                     ; inCommentMulti }
        <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
        <|> do{ oneOf startEnd                       ; inCommentMulti }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd ++ commentStart)

inCommentSingle :: GenParser Char st ()
inCommentSingle
        =   do{ try (string commentEnd)             ; return () }
        <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
        <|> do{ oneOf startEnd                      ; inCommentSingle }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd ++ commentStart)

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
identStart  = letter <|> oneOf "_"

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
                

decimal         = number 10 digit
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
octal           = do{ oneOf "oO"; number 8 octDigit  }

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

--lexeme  :: forall a. ParsecT s u m a -> ParsecT s u m a,
lexeme :: GenParser Char st b -> GenParser Char st b
lexeme p = do{ x <- p; whiteSpace; return x  }

-- ---------------------------------------------------------------------
{-
type TokenParser st = GenTokenParser String st Identity

-- | The type of the record that holds lexical parsers that work on
-- @s@ streams with state @u@ over a monad @m@.

data GenTokenParser s u m
    = TokenParser {
                  }


makeTokenParser :: (Stream s m Char)
                => GenLanguageDef s u m -> GenTokenParser s u m
makeTokenParser languageDef
    = TokenParser{ }

type LanguageDef st = GenLanguageDef String st Identity

-- | The @GenLanguageDef@ type is a record that contains all parameterizable
-- features of the 'Text.Parsec.Token' module. The module 'Text.Parsec.Language'
-- contains some default definitions.

data GenLanguageDef s u m
    = LanguageDef { }
-}      

-- EOF
