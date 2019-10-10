{-# LANGUAGE MultiWayIf #-}

import           Control.Monad (foldM, forM)
import           Control.Monad.Extra (concatMapM)

import           System.Directory (canonicalizePath, getCurrentDirectory, listDirectory)
import           System.Environment (setEnv)
import           System.FilePath (takeFileName, (</>))
import           System.Exit (ExitCode (..), exitFailure, exitSuccess)
import           System.Posix.Files (getFileStatus, isDirectory, isRegularFile)
import           System.Process (rawSystem)
import           System.IO (BufferMode (..))
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout LineBuffering
  IO.hSetBuffering IO.stderr LineBuffering

  cwd <- getCurrentDirectory
  topdir <- canonicalizePath $ cwd </> "dist-newstyle"

  -- Set an environment variable for all the exectuables we want to test.
  setExecutableEnvVar "HJSMIN" topdir "hjsmin"

  tests <- filter (`notElem` ["core", "data"]) <$> listDirectory "test/cli/"
  res <- forM tests $ \ t -> rawSystem ("test/cli/" ++ t ++ "/run") []
  if all (== ExitSuccess) res
    then exitSuccess
    else exitFailure


setExecutableEnvVar :: String -> FilePath -> FilePath -> IO ()
setExecutableEnvVar envName startDir target = do
  xs <- listDirectoryRecursive startDir
  case filter match xs of
    [] -> error "Unable to find hjsmin binary"
    [x] -> setEnv envName x
    _ -> error $ "Multiple binaries: " ++ show xs
 where
  match :: FilePath -> Bool
  match fp = takeFileName fp == target

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fpath = do
  xs <- fmap (fpath </>) <$> listDirectory fpath
  (files, dirs) <- foldM partitioner ([], []) xs
  rest <- concatMapM listDirectoryRecursive (dirs :: [FilePath])
  pure $ files ++ rest
 where
  partitioner :: ([FilePath], [FilePath]) -> FilePath -> IO ([FilePath], [FilePath])
  partitioner (files, dirs) fp = do
    st <- getFileStatus fp
    if
      | isRegularFile st -> pure (fp : files, dirs)
      | isDirectory st -> pure (files, fp : dirs)
      | otherwise -> pure (files, dirs)
