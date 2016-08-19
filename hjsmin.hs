{-# LANGUAGE CPP #-}
module Main where

#include "cabal_macros.h"

import Data.Monoid ((<>))
import Options.Applicative
import Text.Jasmine

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8


data Options = Options
    { inputFile :: String
    , outputFile :: Maybe String
    }

main :: IO ()
main =
    execParser opts >>= minify'
  where
    opts = info (helper <*> options)
        ( fullDesc
            <> progDesc
                ( "Minify JavaScript files (using language-javascript version "
                ++ languageJavascriptVersion ++ ")."
                )
            <> header "hjsmin - a simple command-line interface to the 'hjsmin' library"
            )

options :: Parser Options
options = Options
      <$> argument str (metavar "INPUT_FILE"
                     <> help "The unminified, original JavaScript file")
      <*> optional
            ( strOption (long "output-file"
                    <> short 'o'
                    <> metavar "OUTPUT_FILE"
                    <> help "The minified output file. Default: stdout")
                    )

minify' :: Options -> IO ()
minify' o = do
    minified <- minifyFile (inputFile o)
    case outputFile o of
        Nothing -> C8.putStrLn minified
        Just f  -> B.writeFile f minified


languageJavascriptVersion :: String
languageJavascriptVersion = VERSION_language_javascript
