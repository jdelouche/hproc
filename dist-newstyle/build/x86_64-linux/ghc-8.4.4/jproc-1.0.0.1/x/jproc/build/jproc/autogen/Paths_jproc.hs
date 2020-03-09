{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_jproc (
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
version = Version [1,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jdelouch/.cabal/bin"
libdir     = "/home/jdelouch/.cabal/lib/x86_64-linux-ghc-8.4.4/jproc-1.0.0.1-inplace-jproc"
dynlibdir  = "/home/jdelouch/.cabal/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/jdelouch/.cabal/share/x86_64-linux-ghc-8.4.4/jproc-1.0.0.1"
libexecdir = "/home/jdelouch/.cabal/libexec/x86_64-linux-ghc-8.4.4/jproc-1.0.0.1"
sysconfdir = "/home/jdelouch/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "jproc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "jproc_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "jproc_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "jproc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "jproc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "jproc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
