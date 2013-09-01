module Handler.Post where

import Import
import Entry
import Data.Text (unpack)

-- | We are looking for the entry entryName, but don't know the
-- language. So we find the best language available and
-- redirect to the post.
-- | If no such entry exists, we 404.
getPostR :: String -> Handler Html
getPostR entryName = do
  langs <- (map unpack) `fmap` languages
  lang <- return . fromString . fromMaybe "fr" $ listToMaybe langs
  available <- liftIO $ hasEntry entryName
  case available of
    [] -> notFound
    ls@(l : _) -> case lang `elem` ls of
      True  -> redirect $ PostLR entryName (show lang)
      False -> redirect $ PostLR entryName (show l)

-- | We know which post and which language we want.
-- | 404 if the entry or the language is unavailable.
getPostLR :: String -> String -> Handler Html
getPostLR entryName languageName = do
  lang <- return . fromString $ languageName
  case show lang /= languageName of
    True -> notFound
    False -> do
      available <- liftIO $ hasEntry entryName
      case available of
        [] -> notFound
        ls -> case lang `elem` ls of
          True  -> defaultLayout $ loadEntry entryName lang
          False -> notFound
