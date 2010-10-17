
module Text.Jasmine
    (       
      minify
    , minifyFile  
    ) where      
    
import Text.Jasmine.Parse
import Text.Jasmine.Pretty
import Text.PrettyPrint

minify :: [Char] -> Text.PrettyPrint.Doc
minify s = renderJS $ readJs s

minifyFile :: FilePath -> IO Doc
minifyFile filename =
  do 
     x <- readFile (filename)
     return $ minify x
    
-- EOF    
