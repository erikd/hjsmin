module Text.Jasmine
    (       
      minify
    , minifym  
    , minifyFile  
    ) where      
    
import Text.Jasmine.Parse
import Text.Jasmine.Pretty
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB

minifym :: LB.ByteString -> Either String LB.ByteString
minifym s = case readJsm s of
             Left msg -> Left msg
             Right p  -> Right $ BB.toLazyByteString $ renderJS p                   

minify :: LB.ByteString -> LB.ByteString
minify s = BB.toLazyByteString $ renderJS $ readJs s

_minify' :: LB.ByteString -> BB.Builder
_minify' s = renderJS $ readJs s

minifyFile :: FilePath -> IO LB.ByteString
minifyFile filename =
  do 
     x <- LB.readFile (filename)
     return $ minify x
    
-- EOF    
