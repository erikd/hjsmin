{-# LANGUAGE OverloadedStrings #-}
import Text.Jasmine
import qualified Data.ByteString.Lazy as L

main = do
    lbs <- L.readFile "tmp.js"
    print $ minifym lbs
