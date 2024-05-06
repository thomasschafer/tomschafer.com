import Control.Monad (when)
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy qualified as Lazy
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.Environment (getEnv)
import System.FilePath (takeExtension, (</>))
import Text.Pandoc
  ( Meta (Meta),
    Pandoc (Pandoc),
    ReaderOptions (readerExtensions),
    def,
    pandocExtensions,
    readMarkdown,
    runIOorExplode,
    writeHtml5String,
  )
import Text.Pandoc.Definition (MetaValue (MetaMap))
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
      [ Replace "{% imgUrl %}" (pack $ url $ image frontmatter)
      , Replace "{% imgAlt %}" (pack $ alt $ image frontmatter)
      , Replace "{% title %}" (pack $ title frontmatter)
      , Replace "{% pubDate %}" (pack $ pubDate frontmatter)
      , Replace "{% description %}" (pack $ description frontmatter)
      , Replace "{% postUrl %}" (pack $ postFileName frontmatter)
      ]

  renderAndCopy :: FilePath -> IO ()
  renderAndCopy path = do
    content <- TIO.readFile (srcDir </> path)
    let blogPostReplacements = [Replace "{% posts %}" (pack $ concatMap renderPost blogPosts)]
    siteUrl <- getEnv "SITE_URL"
    let pageReplacements =
          [ Replace "{% body %}" content
          , -- TODO: don't hardcode these in here
            Replace "{% title %}" "Tom Schafer"
          , Replace "{% ogTitle %}" "Tom Schafer"
          , Replace "{% ogDesc %}" "My blog"
          , Replace "{% ogUrl %}" (pack siteUrl)
          , Replace "{% ogType %}" "website"
          , Replace "{% ogImg %}" (pack $ siteUrl ++ "images/home-page.png")
          ]
    let page = replaceWithList blogPostReplacements $ replaceWithList pageReplacements $ fromStrict pageTemplate
    TIO.writeFile (destDir </> path) (toStrict page)

postFileName :: Frontmatter -> String
postFileName = (++ ".html") . map (\c -> if c == ' ' then '-' else toLower c) . title

createPostPages :: [BlogPost] -> Lazy.Text -> FilePath -> IO ()
createPostPages blogPosts pageTemplate destDir =
  mapM_ createPostPage blogPosts
 where
  createPostPage :: BlogPost -> IO ()
  createPostPage (_, frontmatter, content) = do
    let postFilePath = postFileName frontmatter
    siteUrl <- getEnv "SITE_URL"
    let textReplacements =
          [ Replace "{% content %}" content
          , Replace "{% title %}" (pack $ title frontmatter)
          , Replace "{% pubDate %}" (pack $ pubDate frontmatter)
          , Replace "{% imgUrl %}" (pack $ url $ image frontmatter)
          , Replace "{% imgAlt %}" (pack $ alt $ image frontmatter)
          , Replace "{% ogTitle %}" (pack $ title frontmatter)
          , Replace "{% ogDesc %}" (pack $ description frontmatter)
          , Replace "{% ogUrl %}" (pack $ siteUrl ++ postFilePath)
          , Replace "{% ogType %}" "article"
          , Replace "{% ogImg %}" (pack $ siteUrl ++ url (image frontmatter))
          ]
    let pageContent = replaceWithList textReplacements pageTemplate
    TIO.writeFile (destDir </> postFilePath) (toStrict pageContent)

isMarkdown :: FilePath -> Bool
isMarkdown file = takeExtension file `elem` [".md", ".markdown"]

sortPostsRevChron :: BlogPost -> BlogPost -> Ordering
sortPostsRevChron = flip . comparing $ pubDate . (\(_, fm, _) -> fm)

type BlogPost = (FilePath, Frontmatter, Text)

loadBlogPosts :: String -> IO [BlogPost]
loadBlogPosts dir = do
  allFiles <- listDirectory dir
  let mdFiles = filter isMarkdown allFiles
  blogPosts <- mapM processFile mdFiles
  return $ sortBy sortPostsRevChron blogPosts
 where
  processFile file = do
    let filePath = dir </> file
    (metaData, content) <- processBlogPost filePath
    return (file, metaData, content)

data ImageData = ImageData {url :: String, alt :: String}
  deriving (Show)

data Frontmatter = Frontmatter
  { title :: String
  , pubDate :: String
  , image :: ImageData
  , description :: String
  }
  deriving (Show)

updateBlogPostHtml :: Text -> Text
updateBlogPostHtml =
  toStrict
    . replaceWithList
      [ Replace "<a href=\"http" "<a target=\"_blank\" href=\"http"
      , Replace "<a\nhref=\"http" "<a target=\"_blank\" href=\"http"
      ]
    . fromStrict

processBlogPost :: String -> IO (Frontmatter, Text)
processBlogPost blogPostPath = do
  blogPostContent <- TIO.readFile blogPostPath
  runIOorExplode $ do
    let readerOpts = def{readerExtensions = pandocExtensions}
    content@(Pandoc (Meta meta) _) <- readMarkdown readerOpts blogPostContent

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
            { title
            , pubDate
            , image = ImageData{url = imageUrl, alt = imageAlt}
            , description
            }
    result <- writeHtml5String def content
    return (metaData, updateBlogPostHtml result)

clearDestDir :: FilePath -> IO ()
clearDestDir destDir = do
  destDirExists <- doesDirectoryExist destDir
  when destDirExists $ removeDirectoryRecursive destDir

createPages :: FilePath -> IO ()
createPages destDir = do
  blogPosts <- loadBlogPosts "./blog_posts"
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
