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
import qualified Data.ByteString as B

-- TODO: consider using option 4 of http://www.randomhacks.net/articles/2007/03/10/haskell-8-ways-to-report-errors
minifym :: B.ByteString -> Either String LB.ByteString
minifym s = case readJsm s of
             Left msg -> Left msg
             Right p  -> Right $ BB.toLazyByteString $ renderJS p                   

minify :: B.ByteString -> LB.ByteString
minify s = BB.toLazyByteString $ renderJS $ readJs s

_minify' :: B.ByteString -> BB.Builder
_minify' s = renderJS $ readJs s

minifyFile :: FilePath -> IO LB.ByteString
minifyFile filename =
  do 
     x <- B.readFile (filename)
     return $ minify x
    
-- EOF    
