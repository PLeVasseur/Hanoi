module Paths_hanoiTower (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/peter/.cabal/bin"
libdir     = "/home/peter/.cabal/lib/hanoiTower-0.1.0.0/ghc-7.10.3"
datadir    = "/home/peter/.cabal/share/hanoiTower-0.1.0.0"
libexecdir = "/home/peter/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hanoiTower_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hanoiTower_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hanoiTower_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hanoiTower_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
