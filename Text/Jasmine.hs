module Text.Jasmine
    (       
      minify
    , minifym  
    , minifyBb  
    , minifyFile  
    ) where      
    
--import Text.Jasmine.Parse
import Language.JavaScript.Parser
import Text.Jasmine.Pretty
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy.Char8 as S8

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
    
parse' input = parse (lbToStr input) "src"

lbToStr str = S8.unpack str

strToLb str = (LB.fromChunks [(E.encodeUtf8 $ T.pack str)])
     
-- EOF    
