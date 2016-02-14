module Paths_hanoiTower (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/peter/haskell/hanoiTower/.stack-work/install/x86_64-linux/lts-5.3/7.10.3/bin"
libdir     = "/home/peter/haskell/hanoiTower/.stack-work/install/x86_64-linux/lts-5.3/7.10.3/lib/x86_64-linux-ghc-7.10.3/hanoiTower-0.1.0.0-D5Xa2cmSkj87dEQa8rRMWh"
datadir    = "/home/peter/haskell/hanoiTower/.stack-work/install/x86_64-linux/lts-5.3/7.10.3/share/x86_64-linux-ghc-7.10.3/hanoiTower-0.1.0.0"
libexecdir = "/home/peter/haskell/hanoiTower/.stack-work/install/x86_64-linux/lts-5.3/7.10.3/libexec"
sysconfdir = "/home/peter/haskell/hanoiTower/.stack-work/install/x86_64-linux/lts-5.3/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hanoiTower_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hanoiTower_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hanoiTower_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hanoiTower_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hanoiTower_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)