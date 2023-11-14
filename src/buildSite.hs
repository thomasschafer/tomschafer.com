{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM, when)
import Data.Aeson (FromJSON)
import Data.ByteString qualified as B
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml
import Debug.Trace (trace)
import GHC.Generics (Generic)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.FilePath (takeExtension, (</>))
import System.IO (readFile)
import Text.Blaze.Html (Html)
import Text.Pandoc
  ( Extension (..),
    Meta (Meta),
    MetaValue,
    Pandoc (Pandoc),
    PandocError,
    ReaderOptions (readerExtensions, readerStandalone),
    WriterOptions (WriterOptions),
    def,
    enableExtension,
    lookupMeta,
    readMarkdown,
    runIO,
    runIOorExplode,
    writeHtml5,
    writeHtml5String,
  )
import Text.Pandoc.Error (PandocError)

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

-- data ImageData = ImageData {url :: String, alt :: String}

data Frontmatter = Frontmatter
  { title :: String,
    pubDate :: String
    -- image :: ImageData
  }
  deriving (Show)

processMarkdown :: Text -> IO (Frontmatter, Text)
processMarkdown filePath = runIOorExplode $ do
  pandoc@(Pandoc (Meta meta) _) <-
    readMarkdown
      ( def
          { readerStandalone = True,
            readerExtensions =
              enableExtension Ext_yaml_metadata_block (readerExtensions def)
          }
      )
      filePath
  let fromMeta k = show . fromJust $ M.lookup k meta
  let metaData =
        Frontmatter
          { title = fromMeta "title",
            pubDate = fromMeta "pubDate"
            -- image = fromMeta "image"
          }
  res <- writeHtml5String def pandoc
  return (metaData, res)

main :: IO ()
main = do
  let destDir = "out"
  destDirExists <- doesDirectoryExist destDir
  when destDirExists $ removeDirectoryRecursive destDir
  putStrLn "Hello 9"
  copyContents "static" destDir
  blogPosts <- loadMarkdownFiles "./blog_posts"
  mapM_ showStuff blogPosts
  -- TODO: Load blog posts from "blog_posts" dir
  renderPages "src/pages" "src/templates/post-preview.html" destDir
  createPostPages "src/templates/post.html" destDir
  where
    showStuff :: (FilePath, Frontmatter, Text) -> IO ()
    showStuff (fp, fm, t) = do
      print "Showing:::"
      print fp
      print fm
      print t
      print " --- "
