import Distribution.PackageDescription (HookedBuildInfo, PackageDescription, emptyHookedBuildInfo, extraLibDirs, includeDirs, libBuildInfo, library)
import Distribution.Simple (Args, confHook, defaultMainWithHooks, postClean, preConf, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (localPkgDescr))
import Distribution.Simple.Setup (CleanFlags, ConfigFlags)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeDirectoryRecursive, withCurrentDirectory)
import System.Process (readProcess)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = makeLibNewrelic,
        confHook = \info flags -> do
          localBuildInfo <- confHook simpleUserHooks info flags
          updateExtraLibAndIncludeDirs localBuildInfo,
        postClean = cleanLibNewrelic
      }

newrelicVersion :: String
newrelicVersion = "1.2.0"

newrelicBuildDir :: String
newrelicBuildDir = "_build"

newrelicSdkDir :: String
newrelicSdkDir = "c-sdk-" <> newrelicVersion

-- Here we grab the newrelic c-sdk source code, untar it and run make
-- to produce the static library, while also retaining everything from the
-- c-sdk so we have the include headers needed to compile the hsc file.
makeLibNewrelic :: Args -> ConfigFlags -> IO HookedBuildInfo
makeLibNewrelic _ _ = do
  _ <- createDirectoryIfMissing False newrelicBuildDir
  withCurrentDirectory newrelicBuildDir $ do
    let filename = "v" <> newrelicVersion <> ".tar.gz"
    _ <-
      readProcess
        "curl"
        ["-OL", "https://github.com/newrelic/c-sdk/archive/" <> filename]
        ""
    _ <-
      readProcess
        "tar"
        ["xzf", filename]
        ""
    _ <-
      withCurrentDirectory newrelicSdkDir
        $ readProcess
          "make"
          ["static", "CFLAGS='-Wno-missing-field-initializers'"]
          ""
    pure emptyHookedBuildInfo

-- All this does is programmatically edit "package.yaml" (or, well, the .cabal file)
-- to add `extra-lib-dirs` and `include-dirs` pointing to the newrelic C-SDK paths
-- we just got in `makeLibNewrelic`.
updateExtraLibAndIncludeDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibAndIncludeDirs localBuildInfo = do
  let packageDescription = localPkgDescr localBuildInfo
      (Just lib) = library packageDescription
      libBuild = libBuildInfo lib
  dir <- getCurrentDirectory
  let sdkDir = dir <> "/" <> newrelicBuildDir <> "/" <> newrelicSdkDir
  let sdkIncludeDir = sdkDir <> "/include"
  pure
    localBuildInfo
      { localPkgDescr =
          packageDescription
            { library =
                Just
                  $ lib
                    { libBuildInfo =
                        libBuild
                          { extraLibDirs = sdkDir : extraLibDirs libBuild,
                            includeDirs = sdkIncludeDir : includeDirs libBuild
                          }
                    }
            }
      }

cleanLibNewrelic :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanLibNewrelic _ _ _ _ =
  removeDirectoryRecursive newrelicBuildDir
