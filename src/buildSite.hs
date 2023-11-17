import Control.Monad (when)
import Data.Char (toLower)
import Data.Map qualified as M
import Data.Text (Text, unpack)
import Data.Text.IO qualified as TIO
import Debug.Trace (trace)
import GHC.Base (Opaque (O))
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    createFileLink,
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
    runPure,
    writeHtml5String,
  )
import Text.Pandoc.Definition (MetaValue (MetaMap))
import Text.Pandoc.Highlighting (Style, pygments)
import Text.Pandoc.Options (ReaderOptions, WriterOptions, writerHighlightStyle)
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

renderPages :: [BlogPost] -> FilePath -> FilePath -> FilePath -> IO ()
renderPages blogPosts postPreviewPath srcDir destDir = do
  templates <- listDirectory srcDir
  mapM_ renderAndCopy templates
  where
    renderAndCopy :: FilePath -> IO ()
    renderAndCopy path = do
      contents <- readFile (srcDir </> path)
      -- TODO: render template strings here, using postPreviewDir
      writeFile (destDir </> path) contents

createPostPages :: [BlogPost] -> FilePath -> FilePath -> IO ()
createPostPages blogPosts postTemplatePath destDir = do
  postTemplate <- readFile postTemplatePath
  mapM_ createPostPage blogPosts
  where
    createPostPage :: BlogPost -> IO ()
    createPostPage (_, frontmatter, content) = do
      let postFilePath =
            map (\c -> if c == ' ' then '-' else toLower c) $
              destDir </> ((title frontmatter :: FilePath) ++ ".html")
      -- TODO: templating
      TIO.writeFile (trace ("postFilePath = " ++ postFilePath) postFilePath) content

isMarkdown :: FilePath -> Bool
isMarkdown file = takeExtension file `elem` [".md", ".markdown"]

type BlogPost = (FilePath, Frontmatter, Text)

loadMarkdownFiles :: String -> IO [BlogPost]
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

data ImageData = ImageData {url :: String, alt :: String}
  deriving (Show)

data Frontmatter = Frontmatter
  { title :: String,
    pubDate :: String,
    image :: ImageData
  }
  deriving (Show)

processMarkdown :: Text -> IO (Frontmatter, Text)
processMarkdown filePath = runIOorExplode $ do
  let readerOpts =
        def
          { readerStandalone = True,
            readerExtensions = enableExtension Ext_yaml_metadata_block (readerExtensions def)
          } ::
          ReaderOptions
  pandoc@(Pandoc (Meta meta) _) <- readMarkdown readerOpts filePath

  let title = unpack $ maybe "" stringify $ M.lookup "title" meta -- TODO: dry
  let pubDate = unpack $ maybe "" stringify $ M.lookup "pubDate" meta
  let imageMeta = case M.lookup "image" meta of
        Just (MetaMap m) -> m
        _ -> M.empty
  let imageUrl = unpack $ maybe "" stringify $ M.lookup "url" imageMeta
  let imageAlt = unpack $ maybe "" stringify $ M.lookup "alt" imageMeta
  let metaData =
        Frontmatter
          { title,
            pubDate,
            image = ImageData {url = imageUrl, alt = imageAlt}
          }

  let writerOpts = def {writerHighlightStyle = Just pygments} :: WriterOptions
  res <- writeHtml5String writerOpts pandoc
  return (metaData, res)

main :: IO ()
main = do
  let destDir = "out"
  destDirExists <- doesDirectoryExist destDir
  when destDirExists $ removeDirectoryRecursive destDir
  copyContents "static" destDir
  blogPosts <- loadMarkdownFiles "./blog_posts"
  mapM_ showStuff blogPosts -- TODO: delete
  renderPages blogPosts "src/templates/post-preview.html" "src/pages" destDir
  createPostPages blogPosts "src/templates/post.html" destDir
  where
    showStuff :: (FilePath, Frontmatter, Text) -> IO ()
    showStuff (fp, fm, t) = do
      print fp
      print fm
      print t
