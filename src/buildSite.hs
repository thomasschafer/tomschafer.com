import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.FilePath ((</>))

copyContents :: FilePath -> FilePath -> IO ()
copyContents srcDir destDir = do
  createDirectoryIfMissing True destDir
  contents <- listDirectory srcDir
  mapM_ (copyItem srcDir destDir) contents

copyItem :: FilePath -> FilePath -> FilePath -> IO ()
copyItem srcDir destDir itemName = do
  let srcPath = srcDir </> itemName
  let destPath = destDir </> itemName
  isDir <- doesDirectoryExist srcPath
  isFile <- doesFileExist srcPath
  case (isDir, isFile) of
    (True, _) -> copyContents srcPath destPath
    (_, True) -> copyFile srcPath destPath
    _ -> return ()

copyAndRenderTemplates :: FilePath -> FilePath -> IO ()
copyAndRenderTemplates srcDir destDir = do
  copyContents srcDir destDir

main :: IO ()
main = do
  let destDir = "out"
  removeDirectoryRecursive destDir
  copyContents "static" destDir
  copyAndRenderTemplates "src/templates" destDir
