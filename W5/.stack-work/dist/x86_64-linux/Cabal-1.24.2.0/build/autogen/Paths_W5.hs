{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_W5 (
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

bindir     = "/mnt/128GB/Users/Torstein/Cloud/Projects/learnhaskell/cis194/W5/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/bin"
libdir     = "/mnt/128GB/Users/Torstein/Cloud/Projects/learnhaskell/cis194/W5/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/lib/x86_64-linux-ghc-8.0.2/W5-0.1.0.0-XZdPgBj8ab8AI4eBIwYf2"
dynlibdir  = "/mnt/128GB/Users/Torstein/Cloud/Projects/learnhaskell/cis194/W5/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/mnt/128GB/Users/Torstein/Cloud/Projects/learnhaskell/cis194/W5/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/share/x86_64-linux-ghc-8.0.2/W5-0.1.0.0"
libexecdir = "/mnt/128GB/Users/Torstein/Cloud/Projects/learnhaskell/cis194/W5/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/libexec"
sysconfdir = "/mnt/128GB/Users/Torstein/Cloud/Projects/learnhaskell/cis194/W5/.stack-work/install/x86_64-linux/lts-8.0/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "W5_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "W5_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "W5_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "W5_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "W5_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "W5_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
