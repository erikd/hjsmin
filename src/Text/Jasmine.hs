module Text.Jasmine
    ( minify
    , minifym
    , minifyBb
    , minifyFile
    ) where

import           Control.Applicative ((<$>))

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text.Lazy (unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)

import           Language.JavaScript.Parser (readJs, parse, JSAST)
import           Language.JavaScript.Pretty.Printer (renderJS)
import           Language.JavaScript.Process.Minify (minifyJS)



minifym :: LBS.ByteString -> Either String LBS.ByteString
minifym s =
  case myParse s of
    Left msg -> Left (show msg)
    Right p  -> Right $ Builder.toLazyByteString $ renderJS $ minifyJS p


minifyBb :: LBS.ByteString -> Either String Builder
minifyBb s =
  case myParse s  of
    Left msg -> Left (show msg)
    Right p  -> Right (renderJS $ minifyJS p)


minify :: LBS.ByteString -> LBS.ByteString
minify =
  Builder.toLazyByteString . renderJS . minifyJS . readJs . lbToStr


minifyFile :: FilePath -> IO LBS.ByteString
minifyFile filename =
  minify <$> LBS.readFile filename


myParse :: LBS.ByteString -> Either String JSAST
myParse input =
  parse (lbToStr input) "src"


lbToStr :: LBS.ByteString -> String
lbToStr =
  unpack . decodeUtf8With lenientDecode
