{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ccs (
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

bindir     = "/home/maria/Desktop/B5-proj/inversion-tool/.stack-work/install/x86_64-linux/9e88d7ad74d9f23cb0a15edbb207af22e7c2558f58a29c50885aad1adf380ea2/8.4.3/bin"
libdir     = "/home/maria/Desktop/B5-proj/inversion-tool/.stack-work/install/x86_64-linux/9e88d7ad74d9f23cb0a15edbb207af22e7c2558f58a29c50885aad1adf380ea2/8.4.3/lib/x86_64-linux-ghc-8.4.3/ccs-0.0.0-CE9Q9aZwmeE7laM5D5akzF"
dynlibdir  = "/home/maria/Desktop/B5-proj/inversion-tool/.stack-work/install/x86_64-linux/9e88d7ad74d9f23cb0a15edbb207af22e7c2558f58a29c50885aad1adf380ea2/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/maria/Desktop/B5-proj/inversion-tool/.stack-work/install/x86_64-linux/9e88d7ad74d9f23cb0a15edbb207af22e7c2558f58a29c50885aad1adf380ea2/8.4.3/share/x86_64-linux-ghc-8.4.3/ccs-0.0.0"
libexecdir = "/home/maria/Desktop/B5-proj/inversion-tool/.stack-work/install/x86_64-linux/9e88d7ad74d9f23cb0a15edbb207af22e7c2558f58a29c50885aad1adf380ea2/8.4.3/libexec/x86_64-linux-ghc-8.4.3/ccs-0.0.0"
sysconfdir = "/home/maria/Desktop/B5-proj/inversion-tool/.stack-work/install/x86_64-linux/9e88d7ad74d9f23cb0a15edbb207af22e7c2558f58a29c50885aad1adf380ea2/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ccs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ccs_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ccs_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ccs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ccs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ccs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
