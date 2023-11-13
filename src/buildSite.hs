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

renderPages :: FilePath -> FilePath -> FilePath -> IO ()
renderPages srcDir postPreviewDir destDir = do
  templates <- listDirectory srcDir
  mapM_ renderAndCopy templates
  where
    renderAndCopy :: FilePath -> IO ()
    renderAndCopy path = do
      contents <- readFile (srcDir </> path)
      -- TODO: render template strings here, using postPreviewDir
      writeFile (destDir </> path) contents

createPostPages :: FilePath -> FilePath -> IO ()
createPostPages postDir destDir = do
  putStrLn "Todo"

main :: IO ()
main = do
  let destDir = "out"
  removeDirectoryRecursive destDir
  copyContents "static" destDir
  -- TODO: Load blog posts from "blog_posts" dir
  renderPages "src/pages" "src/templates/post-preview.html" destDir
  createPostPages "src/templates/post.html" destDir
