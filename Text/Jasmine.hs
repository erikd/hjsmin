module Text.Jasmine
    (       
      minify
    , minifyFile  
    ) where      
    
import Text.Jasmine.Parse
import Text.Jasmine.Pretty
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B


minify :: B.ByteString -> LB.ByteString
minify s = BB.toLazyByteString $ renderJS $ readJs s

_minify' :: B.ByteString -> BB.Builder
_minify' s = renderJS $ readJs s

-- TODO: read file as a ByteString
minifyFile :: FilePath -> IO LB.ByteString
minifyFile filename =
  do 
     x <- B.readFile (filename)
     return $ minify x
    
-- EOF    
