module Text.Jasmine
    ( minify
    , minifym
    , minifyBb
    , minifyFile
    ) where

import Control.Applicative ((<$>))
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Language.JavaScript.Parser (readJs, parse, JSNode(..))

import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as S8

import Text.Jasmine.Pretty


minifym :: LB.ByteString -> Either String LB.ByteString
minifym s =
    case myParse s of
        Left msg -> Left (show msg)
        Right p  -> Right $ BB.toLazyByteString $ renderJS p


minifyBb :: LB.ByteString -> Either String BB.Builder
minifyBb s =
    case myParse s  of
        Left msg -> Left (show msg)
        Right p  -> Right (renderJS p)


minify :: LB.ByteString -> LB.ByteString
minify s = BB.toLazyByteString . renderJS . readJs $ lbToStr s


minifyFile :: FilePath -> IO LB.ByteString
minifyFile filename = minify <$> LB.readFile filename


myParse :: S8.ByteString -> Either String JSNode
myParse input = parse (lbToStr input) "src"


lbToStr :: S8.ByteString -> String
lbToStr = unpack . decodeUtf8With lenientDecode
