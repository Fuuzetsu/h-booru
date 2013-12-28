{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  HBooru.Parsers.GenericBooru
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- | Many booru sites use the same engine (Gelbooru engine) so instead of having
-- identical parsers across many modules, we simply alias our 'GenericPost' to
-- whatever we need.
module HBooru.Parsers.GenericBooru where

import Prelude hiding (id)
import Data.String
import HBooru.Types
import HBooru.Parsers.GenericBooru.TH
import Text.Read (readMaybe)
import Text.XML.HXT.Core hiding (mkName)
import Language.Haskell.TH (mkName)

-- | A post we might expect from many of the sites as a lot of them seem to be
-- based on the Gelbooru engine.
$(makePost (mkName "GenericPost"))

-- | Fairly naïve parser for all attributes in sites running vanilla
-- Gelbooru engine. While it catches all attributes in a typical XML post
-- result, we trust that they are consistent and use 'read' to deserialise some
-- values. For sites with non-default replies, custom parsers have to be written.
parsePost ∷ ArrowXml cat ⇒ cat XmlTree GenericPost
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
  returnA -< GenericPost
      { heightT = read height
      , scoreT = read score
      , file_urlT = file_url
      , parent_idT = readMaybe parent_id
      , sample_urlT = sample_url
      , sample_widthT = read sample_width
      , sample_heightT = read sample_height
      , preview_urlT = preview_url
      , ratingT = parseRating rating
      , tagsT = parseTags tags
      , idT = read id
      , widthT = read width
      , changeT = change
      , md5T = md5
      , creator_idT = read creator_id
      , has_childrenT = parseBool has_children
      , created_atT = created_at
      , statusT = status
      , sourceT = source
      , has_notesT = parseBool has_notes
      , has_commentsT = parseBool has_comments
      , preview_widthT = read preview_width
      , preview_heightT = read preview_height
      }

-- | Parses a string returned from a Gelbooru-like site into
-- one of the commonly used 'Rating's. Note that this is a partial function
-- so you should make sure that the site in question only ever returns the
-- values in a format specified in the function
parseRating :: String -> Rating
parseRating "e" = Explicit
parseRating "s" = HBooru.Types.Safe
parseRating "q" = Questionable

-- | Splits returned tag string into separate 'Tag's. For Gelbooru-like
-- sites, this is just the question of splitting on whitespace.
parseTags :: String -> [Tag]
parseTags = words

-- | Reads a lowercase 'Bool' string representation into its Haskell type. If we
-- can't parse the boolean, return 'Nothing'.
parseBool :: String -> Maybe Bool
parseBool "false" = Just False
parseBool "true" = Just True
parseBool _ = Nothing
