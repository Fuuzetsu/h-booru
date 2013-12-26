{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module HBooru.Parsers.Gelbooru where

import Control.Applicative hiding (many)
import Data.List
import HBooru.Types
import Text.Read (readMaybe)
import Text.XML.HXT.Core

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
  parseImages = runLA (xreadDoc >>> getChildren >>> parseImage) . getResponse
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

parseImage :: ArrowXml cat => cat XmlTree GelbooruImage
parseImage = hasName "post" >>> proc x -> do
  height <- getAttrValue "height" -< x
  score <- getAttrValue "score" -< x
  file_url <- getAttrValue "file_url" -< x
  parent_id <- getAttrValue "parent_id" -< x
  sample_url <- getAttrValue "sample_url" -< x
  sample_width <- getAttrValue "sample_width" -< x
  sample_height <- getAttrValue "sample_height" -< x
  preview_url <- getAttrValue "preview_url" -< x
  rating <- getAttrValue "rating" -< x
  tags <- getAttrValue "tags" -< x
  id <- getAttrValue "id" -< x
  width <- getAttrValue "width" -< x
  change <- getAttrValue "change" -< x
  md5 <- getAttrValue "md5" -< x
  creator_id <- getAttrValue "creator_id" -< x
  has_children <- getAttrValue "has_children" -< x
  created_at <- getAttrValue "created_at" -< x
  status <- getAttrValue "status" -< x
  source <- getAttrValue "source" -< x
  has_notes <- getAttrValue "has_notes" -< x
  has_comments <- getAttrValue "has_comments" -< x
  preview_width <- getAttrValue "preview_width" -< x
  preview_height <- getAttrValue "preview_height" -< x
  returnA -< GelbooruImage
      { height = read height
      , score = read score
      , file_url = file_url
      , parent_id = readMaybe parent_id
      , sample_url = sample_url
      , sample_width = read sample_width
      , sample_height = read sample_height
      , preview_url = preview_url
      , rating = parseRating rating
      , tags = parseTags tags
      , HBooru.Parsers.Gelbooru.id = read id
      , width = read width
      , change = change
      , md5 = md5
      , creator_id = read creator_id
      , has_children = parseBool has_children
      , created_at = created_at
      , status = status
      , source = source
      , has_notes = parseBool has_notes
      , has_comments = parseBool has_comments
      , preview_width = read preview_width
      , preview_height = read preview_height
      }
