{-# LANGUAGE DeriveDataTypeable #-}
module Text.HJsMin.Parse
    (       
      parseScript
    , HJsMinSettings (..)
    , defaultHJsMinSettings
    ) where

-- ---------------------------------------------------------------------

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

parseScript :: HJsMinSettings -> String -> Result [(Int,Line)]
parseScript set s = do
    ls <- parseLines set s
    return ls 

-- ---------------------------------------------------------------------
-- | Settings for parsing of a javascript document.
data HJsMinSettings = HJsMinSettings
    {
      -- | Placeholder in the structure, no actual settings yet
      hjsminPlaceholder :: String
    }

-- ---------------------------------------------------------------------
-- | Defaults settings: settings not currently used
defaultHJsMinSettings :: HJsMinSettings
defaultHJsMinSettings = HJsMinSettings "foo"

-- ---------------------------------------------------------------------

parseLines :: HJsMinSettings -> String -> Result [(Int, Line)]
parseLines set s =
    case parse (many $ parseLine set) s s of
        Left e -> Error $ show e
        Right x -> Ok x

-- ---------------------------------------------------------------------

parseLine :: HJsMinSettings -> Parser (Int, Line)
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
reservedOp  = P.reservedOp lexer
        
-- EOF
