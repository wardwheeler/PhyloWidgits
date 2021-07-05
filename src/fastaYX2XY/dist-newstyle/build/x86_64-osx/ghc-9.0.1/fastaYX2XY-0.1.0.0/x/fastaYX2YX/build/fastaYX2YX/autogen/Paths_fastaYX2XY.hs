{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_fastaYX2XY (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/ward/.cabal/bin"
libdir     = "/Users/ward/.cabal/lib/x86_64-osx-ghc-9.0.1/fastaYX2XY-0.1.0.0-inplace-fastaYX2YX"
dynlibdir  = "/Users/ward/.cabal/lib/x86_64-osx-ghc-9.0.1"
datadir    = "/Users/ward/.cabal/share/x86_64-osx-ghc-9.0.1/fastaYX2XY-0.1.0.0"
libexecdir = "/Users/ward/.cabal/libexec/x86_64-osx-ghc-9.0.1/fastaYX2XY-0.1.0.0"
sysconfdir = "/Users/ward/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fastaYX2XY_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fastaYX2XY_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fastaYX2XY_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fastaYX2XY_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fastaYX2XY_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fastaYX2XY_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
