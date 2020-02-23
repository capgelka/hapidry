import           Data.Maybe
import           Distribution.PackageDescription    hiding (Flag)
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           System.Directory


main = defaultMainWithHooks simpleUserHooks
  {
      confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
      , postCopy = copyExtLib
  }


updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = (dir ++ "/bin") :
                        extraLibDirs libBuild
                }
            }
        }
    }

copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    let verbosity = fromFlag $ copyVerbosity flags
    rawSystemExit verbosity "cp" ["bin/libhtml_md_tools.a", libPref]
