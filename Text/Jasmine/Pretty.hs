module Text.Jasmine.Pretty
    (       
 
    ) where

import Text.Jasmine.Parse
import Text.PrettyPrint

-- ---------------------------------------------------------------------

renderJS (JSEmpty) = text ";"

-- ---------------------------------------------------------------------
-- Test stuff

-- doParse statementList "a=1;"  

case1 = [JSExpression 
         [JSNode JS_value (JSValue "assignmentExpression") 
          [JSIdentifier "a",
           JSNode JS_value (JSValue "=") [] [] [],
           JSNode JS_value (JSDecimal 1) [] [] []
          ] [] []
         ],
         JSEmpty]
  



-- EOF

