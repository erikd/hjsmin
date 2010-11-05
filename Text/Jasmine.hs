module Text.Jasmine
    (       
      minify
    , minifyFile  
    ) where      
    
import Text.Jasmine.Parse
import Text.Jasmine.Pretty
import Text.PrettyPrint
import qualified Data.ByteString.UTF8 as U

minify :: U.ByteString -> String
minify s = show $ renderJS $ readJs s

_minify' :: U.ByteString -> Doc
_minify' s = renderJS $ readJs s

-- TODO: read file as a ByteString
minifyFile :: FilePath -> IO String
minifyFile filename =
  do 
     x <- readFile (filename)
     return $ minify (U.fromString x)
    
-- EOF    
