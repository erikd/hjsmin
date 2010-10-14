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
         [JSElement "assignmentExpression" 
          [JSIdentifier "a",JSOperator "=",JSDecimal 1]
         ]
        ,JSEmpty
        ]
  
  
  



-- EOF

