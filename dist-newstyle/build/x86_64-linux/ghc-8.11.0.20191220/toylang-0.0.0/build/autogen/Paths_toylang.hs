{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_toylang (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/blackcap/.cabal/bin"
libdir     = "/home/blackcap/.cabal/lib/x86_64-linux-ghc-8.11.0.20191220/toylang-0.0.0-inplace"
dynlibdir  = "/home/blackcap/.cabal/lib/x86_64-linux-ghc-8.11.0.20191220"
datadir    = "/home/blackcap/.cabal/share/x86_64-linux-ghc-8.11.0.20191220/toylang-0.0.0"
libexecdir = "/home/blackcap/.cabal/libexec/x86_64-linux-ghc-8.11.0.20191220/toylang-0.0.0"
sysconfdir = "/home/blackcap/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "toylang_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "toylang_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "toylang_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "toylang_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "toylang_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "toylang_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
