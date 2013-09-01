module Entry
    ( hasEntry
    , hasArticle
    , EntryMap
    , Language(..)
    , fromString
    , loadEntry
    ) where

import Import
import System.Directory (doesFileExist)
import Control.Monad
import Control.Applicative
import Yesod.Markdown
import Text.Pandoc.Options
import qualified Data.Text as Text (lines, unpack, pack)

put :: Bool -> t -> [t]
put cond value = if cond then [value] else []

ext = pandocExtensions <> githubMarkdownExtensions <> multimarkdownExtensions

hblogReader :: ReaderOptions
hblogReader = yesodDefaultReaderOptions {readerExtensions = ext}
hblogWriter :: WriterOptions
hblogWriter = yesodDefaultWriterOptions {writerHighlight = True, writerExtensions = ext}

markdownToHtml' :: Markdown -> (String, String, Html)
markdownToHtml' md = ( title ls
                     , date ls
                     , writePandoc hblogWriter pandoc
                     )
  where
    pandoc = parseMarkdown hblogReader md
    ls = map Text.unpack . take 5 . Text.lines . unMarkdown $ md
    title (('T':'i':'t':'l':'e':':':t):_) = t
    title (_:xs)                          = title xs
    title [] = ""
    author (('A':'u':'t':'h':'o':'r':':':t):_) = t
    author (_:xs)                              = author xs
    author [] = ""
    date (('D':'a':'t':'e':':':t):_) = t
    date (_:xs)                      = date xs
    date []                          = ""

-- | Tell if an entry is available
hasEntry :: String -> IO [Language]
hasEntry postName = do
  frE <- doesFileExist $ "posts/" ++ postName ++ "/fr.md"
  enE <- doesFileExist $ "posts/" ++ postName ++ "/en.md"
  return $ (put enE En) <|> (put frE Fr)

-- | Tell if the article is available
hasArticle :: String -> IO [Language]
hasArticle articleName = do
  frE <- doesFileExist $ "articles/" ++ articleName ++ "/fr.md"
  enE <- doesFileExist $ "articles/" ++ articleName ++ "/en.md"
  return $ put enE En <|> put frE Fr

loadEntry :: String -> Language -> Widget
loadEntry entryName lang = do
  file <- return $ case lang of
    Fr -> "posts/" ++ entryName ++ "/fr.md"
    En -> "posts/" ++ entryName ++ "/en.md"
  (title, _, html) <- liftIO $ markdownToHtml' `liftM` markdownFromFile file
  setTitle $ [shamlet|#{title}|]
  toWidget html
