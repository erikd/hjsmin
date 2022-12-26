{-# LANGUAGE CPP #-}

#include "cabal_macros.h"

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Options.Applicative (Parser, ParserInfo, ParserPrefs)
import qualified Options.Applicative as Opt
import           Text.Jasmine (minify)

import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitFailure)

data Command
  = Process FilePath (Maybe FilePath)

main :: IO ()
main =
  Opt.customExecParser p opts >>= processFile
  where
    opts :: ParserInfo Command
    opts = Opt.info (Opt.helper <*> pVersion <*> pProcess)
      ( Opt.fullDesc
      <> Opt.header "hjsmin - Haskell implementation of a Javascript and JSON minifier"
      )

    p :: ParserPrefs
    p = Opt.prefs Opt.showHelpOnEmpty

pVersion :: Parser (a -> a)
pVersion =
  Opt.infoOption versionString
    (  Opt.long "version"
    <> Opt.short 'v'
    <> Opt.help "Print the version and exit"
    )

pProcess :: Parser Command
pProcess =
  Process <$> pInputFile <*> pMaybeOutputFile
  where
    pInputFile =
      Opt.strOption
        (  Opt.long "input"
        <> Opt.short 'i'
        <> Opt.metavar "INPUT_FILE"
        <> Opt.help "The original JavaScript file"
        )

    pMaybeOutputFile =
      Opt.optional $ Opt.strOption
        (  Opt.long "output"
        <> Opt.short 'o'
        <> Opt.metavar "OUTPUT_FILE"
        <> Opt.help "The minified output file. Default: stdout"
        )


processFile :: Command -> IO ()
processFile (Process inputFile outputFile) = do
  lbs <- LBS.readFile inputFile
  if LBS.null lbs
    then emptyFileError
    else do
      let minified = minify lbs
      case outputFile of
        Nothing -> LBS.putStrLn minified
        Just f  -> LBS.writeFile f minified
  where
    emptyFileError = do
      hPutStrLn stderr $ "Error: input file '" ++ inputFile ++ "' is empty."
      exitFailure


versionString :: String
versionString =
  concat
    [ "hjsmin version ", VERSION_hjsmin
    , " (using language-javascript version ", VERSION_language_javascript, ")"
    ]
