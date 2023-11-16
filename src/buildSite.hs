import Control.Monad (when)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.FilePath (takeExtension, (</>))
import Text.Pandoc
  ( Extension (Ext_yaml_metadata_block),
    Meta (Meta),
    Pandoc (Pandoc),
    ReaderOptions (readerExtensions, readerStandalone),
    def,
    enableExtension,
    readMarkdown,
    runIOorExplode,
    writeHtml5String,
  )
import Text.Pandoc.Definition (MetaValue (MetaMap))
import Text.Pandoc.Shared (stringify)

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

isMarkdown :: FilePath -> Bool
isMarkdown file = takeExtension file `elem` [".md", ".markdown"]

loadMarkdownFiles :: String -> IO [(FilePath, Frontmatter, Text)]
loadMarkdownFiles dir = do
  allFiles <- listDirectory dir
  let mdFiles = filter isMarkdown allFiles
  mapM processFile mdFiles
  where
    processFile file = do
      let filePath = dir </> file
      fileContent <- TIO.readFile filePath
      (metaData, content) <- processMarkdown fileContent
      return (file, metaData, content)

data ImageData = ImageData {url :: Text, alt :: Text}
  deriving (Show)

data Frontmatter = Frontmatter
  { title :: Text,
    pubDate :: Text,
    image :: ImageData
  }
  deriving (Show)

processMarkdown :: Text -> IO (Frontmatter, Text)
processMarkdown filePath = runIOorExplode $ do
  pandoc@(Pandoc (Meta meta) _) <-
    readMarkdown
      ( def
          { readerStandalone = True,
            readerExtensions = enableExtension Ext_yaml_metadata_block (readerExtensions def)
          }
      )
      filePath
  let title = maybe "" stringify $ M.lookup "title" meta
  let pubDate = maybe "" stringify $ M.lookup "pubDate" meta
  let imageMeta = case M.lookup "image" meta of
        Just (MetaMap m) -> m
        _ -> M.empty
  let imageUrl = maybe "" stringify $ M.lookup "url" imageMeta
  let imageAlt = maybe "" stringify $ M.lookup "alt" imageMeta
  let metaData =
        Frontmatter
          { title,
            pubDate,
            image = ImageData {url = imageUrl, alt = imageAlt}
          }
  res <- writeHtml5String def pandoc
  return (metaData, res)

main :: IO ()
main = do
  let destDir = "out"
  destDirExists <- doesDirectoryExist destDir
  when destDirExists $ removeDirectoryRecursive destDir
  copyContents "static" destDir
  blogPosts <- loadMarkdownFiles "./blog_posts"
  mapM_ showStuff blogPosts
  -- TODO: Load blog posts from "blog_posts" dir
  renderPages "src/pages" "src/templates/post-preview.html" destDir
  createPostPages "src/templates/post.html" destDir
  where
    showStuff :: (FilePath, Frontmatter, Text) -> IO ()
    showStuff (fp, fm, t) = do
      print fp
      print fm
      print t
