module Text.Jasmine
    (       
      minify
    , minifyFile  
    ) where      
    
import Text.Jasmine.Parse
import Text.Jasmine.Pretty
import Text.PrettyPrint

minify :: String -> String
minify s = show $ renderJS $ readJs s

_minify' :: [Char] -> Text.PrettyPrint.Doc
_minify' s = renderJS $ readJs s

minifyFile :: FilePath -> IO String
minifyFile filename =
  do 
     x <- readFile (filename)
     return $ minify x
    
-- EOF    
