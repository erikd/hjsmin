{-# LANGUAGE CPP #-}

#include "cabal_macros.h"

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Monoid ((<>))
import           Options.Applicative
import           Text.Jasmine

import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitFailure)

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
  lbs <- LBS.readFile $ inputFile o
  if LBS.null lbs
    then emptyFileError
    else do
      let minified = minify lbs
      case outputFile o of
        Nothing -> LBS.putStrLn minified
        Just f  -> LBS.writeFile f minified
  where
    emptyFileError = do
      hPutStrLn stderr $ "Error: input file '" ++ inputFile o ++ "' is empty."
      exitFailure


languageJavascriptVersion :: String
languageJavascriptVersion = VERSION_language_javascript
