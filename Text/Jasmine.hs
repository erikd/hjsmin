module Text.Jasmine
    (
      minify
    , minifym
    , minifyBb
    , minifyFile
    ) where

--import Text.Jasmine.Parse
import Language.JavaScript.Parser (readJs, parse, JSNode(..))
import Text.Jasmine.Pretty
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as S8
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

minifym :: LB.ByteString -> Either String LB.ByteString
minifym s = case parse' s of
             Left msg -> Left (show msg)
             Right p  -> Right $ BB.toLazyByteString $ renderJS p

minifyBb :: LB.ByteString -> Either String BB.Builder
minifyBb s = case parse' s  of
             Left msg -> Left (show msg)
             Right p  -> Right (renderJS p)

minify :: LB.ByteString -> LB.ByteString
--minify s = BB.toLazyByteString $ renderJS $ readJs s
minify s = BB.toLazyByteString $ renderJS $ readJs (lbToStr s)

_minify' :: LB.ByteString -> BB.Builder
_minify' s = renderJS $ readJs (lbToStr s)

minifyFile :: FilePath -> IO LB.ByteString
minifyFile filename =
  do
     x <- LB.readFile (filename)
     return $ minify x

--parse' :: S8.ByteString -> Either ParseError JSNode
parse'
  :: S8.ByteString -> Either String JSNode
parse' input = parse (lbToStr input) "src"

lbToStr :: S8.ByteString -> [Char]
lbToStr = unpack . decodeUtf8With lenientDecode

_strToLb :: String -> S8.ByteString
_strToLb str = S8.pack str


-- EOF
