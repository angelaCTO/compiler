module Paths_fdl (
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

bindir     = "/home/linux/ieng6/oce/26/cshmerli/131/CSE131_Programs/06-fer-de-lance/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/bin"
libdir     = "/home/linux/ieng6/oce/26/cshmerli/131/CSE131_Programs/06-fer-de-lance/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/lib/x86_64-linux-ghc-7.10.3/fdl-0.1.0.0-FOF0zxZGtS19QRevSEc2DZ"
datadir    = "/home/linux/ieng6/oce/26/cshmerli/131/CSE131_Programs/06-fer-de-lance/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/share/x86_64-linux-ghc-7.10.3/fdl-0.1.0.0"
libexecdir = "/home/linux/ieng6/oce/26/cshmerli/131/CSE131_Programs/06-fer-de-lance/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/libexec"
sysconfdir = "/home/linux/ieng6/oce/26/cshmerli/131/CSE131_Programs/06-fer-de-lance/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fdl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fdl_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "fdl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fdl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fdl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
