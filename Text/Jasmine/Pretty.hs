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
case1 = [JSNode JS_value (JSValue "expression") 
          [JSNode JS_value (JSValue "assignmentExpression") 
           [JSNode JS_value (JSValue "memberExpression'") 
            [JSNode JS_value (JSIdentifier "a") [] [] [],
             JSNode JS_value (JSValue "memberExpression") [] [] []] [] [],
            JSNode JS_value (JSValue "=") [] [] [],
            JSNode JS_value (JSValue "memberExpression'") 
            [JSNode JS_value (JSDecimal 1) [] [] [],
             JSNode JS_value (JSValue "memberExpression") [] [] []] [] []] [] []] [] [],
         JSEmpty]
                                   



-- EOF

