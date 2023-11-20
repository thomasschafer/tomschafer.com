import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Map qualified as M
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy qualified as Lazy
import Debug.Trace (trace)
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
import Text.Pandoc.Highlighting (pygments)
import Text.Pandoc.Options (WriterOptions, writerHighlightStyle)
import Text.Pandoc.Shared (stringify)
import Text.Replace (Replace (Replace), replaceWithList)

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

renderPages :: [BlogPost] -> Text -> Text -> FilePath -> FilePath -> IO ()
renderPages blogPosts pageTemplate postPreviewTemplate srcDir destDir = do
  pages <- listDirectory srcDir
  mapM_ renderAndCopy pages
  where
    renderPost :: BlogPost -> String
    renderPost (_, frontmatter, _) =
      unpack . toStrict $
        replaceWithList replacements (fromStrict postPreviewTemplate)
      where
        replacements =
          [ Replace "{% imgUrl %}" (pack $ url $ image frontmatter),
            Replace "{% imgAlt %}" (pack $ alt $ image frontmatter),
            Replace "{% title %}" (pack $ title frontmatter),
            Replace "{% pubDate %}" (pack $ pubDate frontmatter),
            Replace "{% description %}" (pack $ description frontmatter),
            Replace "{% postUrl %}" (pack $ postFileName frontmatter)
          ]

    renderAndCopy :: FilePath -> IO ()
    renderAndCopy path = do
      content <- TIO.readFile (srcDir </> path)
      -- TODO: single replace rather than list?
      let page =
            replaceWithList [Replace "{% posts %}" (pack $ concatMap renderPost blogPosts)] $
              replaceWithList [Replace "{% body %}" content, Replace "{% title %}" "Tom Schafer"] $
                fromStrict pageTemplate
      -- TODO: render template strings here, using postPreviewTemplate
      TIO.writeFile (destDir </> path) (toStrict page)

postFileName :: Frontmatter -> String
postFileName = (++ ".html") . map (\c -> if c == ' ' then '-' else toLower c) . title

createPostPages :: [BlogPost] -> Lazy.Text -> FilePath -> IO ()
createPostPages blogPosts pageTemplate destDir =
  mapM_ createPostPage blogPosts
  where
    createPostPage :: BlogPost -> IO ()
    createPostPage (_, frontmatter, content) = do
      let postFilePath = destDir </> postFileName frontmatter
      let textReplacements =
            [ Replace "{% content %}" content,
              Replace "{% title %}" (pack $ title frontmatter),
              Replace "{% pubDate %}" (pack $ pubDate frontmatter),
              Replace "{% imgUrl %}" (pack $ url $ image frontmatter),
              Replace "{% imgAlt %}" (pack $ alt $ image frontmatter)
            ]
      let pageContent = replaceWithList textReplacements pageTemplate
      TIO.writeFile postFilePath (toStrict pageContent)

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
    image :: ImageData,
    description :: String
  }
  deriving (Show)

processMarkdown :: Text -> IO (Frontmatter, Text)
processMarkdown filePath = runIOorExplode $ do
  let readerOpts = def {readerExtensions = enableExtension Ext_yaml_metadata_block (readerExtensions def)}
  result@(Pandoc (Meta meta) _) <- readMarkdown readerOpts filePath

  let maybeMetaToText = unpack . maybe "" stringify
  let title = maybeMetaToText $ M.lookup "title" meta
  let pubDate = maybeMetaToText $ M.lookup "pubDate" meta
  let description = maybeMetaToText $ M.lookup "description" meta
  let imageMeta = case M.lookup "image" meta of
        Just (MetaMap m) -> m
        _ -> M.empty
  let imageUrl = maybeMetaToText $ M.lookup "url" imageMeta
  let imageAlt = maybeMetaToText $ M.lookup "alt" imageMeta
  let metaData =
        Frontmatter
          { title,
            pubDate,
            image = ImageData {url = imageUrl, alt = imageAlt},
            description
          }

  let writerOpts = def {writerHighlightStyle = Just pygments}
  res <- writeHtml5String writerOpts result
  liftIO $ putStrLn $ unpack res
  return (metaData, res)

clearDestDir :: FilePath -> IO ()
clearDestDir destDir = do
  destDirExists <- doesDirectoryExist destDir
  when destDirExists $ removeDirectoryRecursive destDir

createPages :: FilePath -> IO ()
createPages destDir = do
  blogPosts <- loadMarkdownFiles "./blog_posts"
  pageTemplate <- TIO.readFile "src/templates/page.html"
  postPreviewTemplate <- TIO.readFile "src/templates/post-preview.html"
  partialPostTemplate <- TIO.readFile "src/templates/post.html"
  let postTemplate = replaceWithList [Replace "{% body %}" partialPostTemplate] (fromStrict pageTemplate)

  renderPages blogPosts pageTemplate postPreviewTemplate "src/pages" destDir
  createPostPages blogPosts postTemplate destDir

main :: IO ()
main = do
  let destDir = "out"
  clearDestDir destDir
  copyContents "static" destDir
  createPages destDir