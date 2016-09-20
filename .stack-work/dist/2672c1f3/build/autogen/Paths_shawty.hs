module Paths_shawty (
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

bindir     = "E:\\github\\shawty\\.stack-work\\install\\b70b48a6\\bin"
libdir     = "E:\\github\\shawty\\.stack-work\\install\\b70b48a6\\lib\\x86_64-windows-ghc-7.10.3\\shawty-0.1.0.0-9VvhcCZugZd3TERaXSxfZx"
datadir    = "E:\\github\\shawty\\.stack-work\\install\\b70b48a6\\share\\x86_64-windows-ghc-7.10.3\\shawty-0.1.0.0"
libexecdir = "E:\\github\\shawty\\.stack-work\\install\\b70b48a6\\libexec"
sysconfdir = "E:\\github\\shawty\\.stack-work\\install\\b70b48a6\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "shawty_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "shawty_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "shawty_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shawty_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shawty_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
