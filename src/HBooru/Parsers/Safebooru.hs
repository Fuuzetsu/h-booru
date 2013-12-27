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
{-# LANGUAGE TypeFamilies #-}
module HBooru.Parsers.Safebooru where

import Data.List
import Data.String
import HBooru.Types
import Text.Read (readMaybe)
import Text.XML.HXT.Core

type SafebooruTag = String

instance Tag SafebooruTag where
  showTag = show

data SafebooruPost = SafebooruPost { height ∷ Integer
                                   , score ∷ Integer
                                   , file_url ∷ String
                                   , parent_id ∷ Maybe Integer
                                   , sample_url ∷ String
                                   , sample_width ∷ Integer
                                   , sample_height ∷ Integer
                                   , preview_url ∷ String
                                   , rating ∷ Rating
                                   , tags ∷ [SafebooruTag]
                                   , id ∷ Integer
                                   , width ∷ Integer
                                   , change ∷ String
                                   , md5 ∷ String
                                   , creator_id ∷ Integer
                                   , has_children ∷ Bool
                                   , created_at ∷ String
                                   , status ∷ String
                                   , source ∷ String
                                   , has_notes ∷ Bool
                                   , has_comments ∷ Bool
                                   , preview_width ∷ Integer
                                   , preview_height ∷ Integer
                                   } deriving Show

data Safebooru = Safebooru

instance Postable Safebooru XML where
  postUrl _ ts =
    let tags = intercalate "+" $ map showTag ts
    in "http://safebooru.org/index.php?page=dapi&s=post&q=index&limit=100&tags="
       ++ tags ++ "&pid=0"

instance Site Safebooru where
  hardLimit _ = Limit 100

parseRating :: (Eq a, IsString a) => a -> Rating
parseRating "e" = Explicit
parseRating "s" = Safe
parseRating "q" = Questionable


parseTags :: String -> [String]
parseTags = words

parseBool :: (Eq a, IsString a) => a -> Bool
parseBool "false" = False
parseBool "true" = True

parsePost ∷ ArrowXml cat => cat XmlTree SafebooruPost
parsePost = hasName "post" >>> proc x → do
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
  returnA -< SafebooruPost
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
      , HBooru.Parsers.Safebooru.id = read id
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

instance PostParser Safebooru XML where
  type ImageTy Safebooru XML = SafebooruPost
  parseResponse _ = runLA (xreadDoc /> parsePost) . getResponse

instance Counted Safebooru XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
