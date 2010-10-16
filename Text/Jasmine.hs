
module Text.Jasmine
    (       
    blah      
    , minify
    ) where      
    
import Text.Jasmine.Parse
import Text.Jasmine.Pretty
import Text.PrettyPrint


blah :: Integer
blah = 1    

minify :: [Char] -> Text.PrettyPrint.Doc
minify s = renderJS $ readJs s

-- EOF    
