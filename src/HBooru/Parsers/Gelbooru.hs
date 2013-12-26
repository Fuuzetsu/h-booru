{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HBooru.Parsers.Gelbooru where

import Control.Applicative hiding (many)
import Control.Monad.Trans.Resource
import Data.List
import HBooru.Types
import Data.Conduit
import Data.Text (Text, unpack)
import Text.Read (readMaybe)
import Data.XML.Types
import Text.XML.Stream.Parse

data GelbooruRating = Safe | Questionable | Explicit deriving (Show, Eq)
type GelbooruTag = String

instance Tag GelbooruTag where
  showTag = show

data GelbooruImage = GelbooruImage { height :: Integer
                                   , score :: Integer
                                   , file_url :: String
                                   , parent_id :: Maybe Integer
                                   , sample_url :: String
                                   , sample_width :: Integer
                                   , sample_height :: Integer
                                   , preview_url :: String
                                   , rating :: GelbooruRating
                                   , tags :: [GelbooruTag]
                                   , id :: Integer
                                   , width :: Integer
                                   , change :: String
                                   , md5 :: String
                                   , creator_id :: Integer
                                   , has_children :: Bool
                                   , created_at :: String
                                   , status :: String
                                   , source :: String
                                   , has_notes :: Bool
                                   , has_comments :: Bool
                                   , preview_width :: Integer
                                   , preview_height :: Integer
                                   } deriving Show

data SampleGelbooruImage = SampleGelbooruImage { sHeight, sWidth :: Integer
                                               , sLink :: String
                                               } deriving Show

data GelbooruImageParser

instance BImage GelbooruImage where

instance BParser GelbooruImageParser XMLResponse GelbooruImage where
  hardLimit _ = Limit 100
  parseImages r = let x = getResponse r in []
  tagURL _ ts =
    let tags = intercalate "+" $ map showTag ts
    in "http://gelbooru.com/index.php?page=dapi&s=post&q=index&limit=100&tags="
       ++ tags ++ "&pid=0"

parseRating "e" = Explicit
parseRating "s" = Safe
parseRating "q" = Questionable

parseTags = words

parseBool "false" = False
parseBool "true" = True

xmlParseImage ∷ MonadThrow m ⇒ ConduitM Event o m (Maybe GelbooruImage)
xmlParseImage =
  tagName "post"
  (((,,,,,,,,,,,,,,,,,,,,,,) <$>
    r "height"
    <*> r "score"
    <*> r "file_url"
    <*> r "parent_id"
    <*> r "sample_url"
    <*> r "sample_width"
    <*> r "sample_height"
    <*> r "preview_url"
    <*> r "rating"
    <*> r "tags"
    <*> r "id"
    <*> r "width"
    <*> r "change"
    <*> r "md5"
    <*> r "creator_id"
    <*> r "has_children"
    <*> r "created_at"
    <*> r "status"
    <*> r "source"
    <*> r "has_notes"
    <*> r "has_comments"
    <*> r "preview_width"
    <*> r "preview_height")
   <* ignoreAttrs) $
  \(height, score, file_url, parent_id, sample_url, sample_width, sample_height,
    preview_url, rating, tags, id, width, change, md5, creator_id, has_children,
    created_at, status, source, has_notes, has_comments, preview_width,
    preview_height) → do
    return $ GelbooruImage
      { height = read $ unpack height
      , score = read $ unpack score
      , file_url = unpack file_url
      , parent_id = readMaybe $ unpack parent_id
      , sample_url = unpack sample_url
      , sample_width = read $ unpack sample_width
      , sample_height = read $ unpack sample_height
      , preview_url = unpack preview_url
      , rating = parseRating $ unpack rating
      , tags = parseTags $ unpack tags
      , HBooru.Parsers.Gelbooru.id = read $ unpack id
      , width = read $ unpack width
      , change = unpack change
      , md5 = unpack md5
      , creator_id = read $ unpack creator_id
      , has_children = parseBool $ unpack has_children
      , created_at = unpack created_at
      , status = unpack status
      , source = unpack source
      , has_notes = parseBool $ unpack has_notes
      , has_comments = parseBool $ unpack has_comments
      , preview_width = read $ unpack preview_width
      , preview_height = read $ unpack preview_height
      }
  where
    r = requireAttr


xmlParseImages ∷ MonadThrow m ⇒ ConduitM Event o m (Maybe [GelbooruImage])
xmlParseImages = tagName "posts" ignoreAttrs . const $ many xmlParseImage

-- test :: IO [GelbooruImage]
-- test = do
--   runResourceT $
--     parseFile def "/tmp/shana.xml" $$ force "posts required" xmlParseImages
