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

data Script = ScriptForall [Statements]
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

-- EOF
