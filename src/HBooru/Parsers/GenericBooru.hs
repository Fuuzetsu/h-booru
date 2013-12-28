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
import Text.Read (readMaybe)
import Text.XML.HXT.Core hiding (mkName)
import Language.Haskell.TH

-- | A post we might expect from many of the sites as a lot of them seem to be
-- based on the Gelbooru engine.
data GenericPost = GenericPost { height ∷ Integer
                               , score ∷ Integer
                               , file_url ∷ String
                               , parent_id ∷ Maybe Integer
                               , sample_url ∷ String
                               , sample_width ∷ Integer
                               , sample_height ∷ Integer
                               , preview_url ∷ String
                               , rating ∷ Rating
                               , tags ∷ [String]
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

-- Feels like LISP man
-- | Effectively casts into a different data type that takes the same arguments
-- as 'GenericPost'. This is a (hopefully temporary) hack used to have the
-- ability to parse to a generic post but get a different data type out later,
-- in the specific parsers.
fromGeneric g n = n
                  (height g)
                  (score g)
                  (file_url g)
                  (parent_id g)
                  (sample_url g)
                  (sample_width g)
                  (sample_height g)
                  (preview_url g)
                  (rating g)
                  (tags g)
                  (id g)
                  (width g)
                  (change g)
                  (md5 g)
                  (creator_id g)
                  (has_children g)
                  (created_at g)
                  (status g)
                  (source g)
                  (has_notes g)
                  (has_comments g)
                  (preview_width g)
                  (preview_height g)


-- | Template Haskell function which is able to generate 'GenericPost'-alike
-- type declarations for cases where we want to use this format but need a
-- different data type. It can be used by using @TemplateHaskell@ extension and
-- calling @$(makePost ('mkName' \"YourTypeName\"))@ at the top level. Hopefully
-- a temporary measure until the author thinks of a better way to provide
-- generic Gelbooru-like post parsing while casting out to different data types
-- that's OK to write.
makePost :: Name -> Q [Dec]
makePost n =
  fmap (:[]) $ dataD (cxt []) n []
  [ recC n
    [ varStrictType (mkName "height") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "score") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "file_url") $ strictType notStrict [t| String |]
    , varStrictType (mkName "parent_id") $ strictType notStrict
                                             [t| Maybe Integer |]
    , varStrictType (mkName "sample_url") $ strictType notStrict [t| String |]
    , varStrictType (mkName "sample_width") $ strictType notStrict
                                                [t| Integer |]
    , varStrictType (mkName "sample_height") $ strictType notStrict
                                                 [t| Integer |]
    , varStrictType (mkName "preview_url") $ strictType notStrict [t| String |]
    , varStrictType (mkName "rating") $ strictType notStrict [t| Rating |]
    , varStrictType (mkName "tags") $ strictType notStrict [t| [String] |]
    , varStrictType (mkName "id") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "width") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "change") $ strictType notStrict [t| String |]
    , varStrictType (mkName "md5") $ strictType notStrict [t| String |]
    , varStrictType (mkName "creator_id") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "has_children") $ strictType notStrict [t| Bool |]
    , varStrictType (mkName "created_at") $ strictType notStrict [t| String |]
    , varStrictType (mkName "status") $ strictType notStrict [t| String |]
    , varStrictType (mkName "source") $ strictType notStrict [t| String |]
    , varStrictType (mkName "has_notes") $ strictType notStrict [t| Bool |]
    , varStrictType (mkName "has_comments") $ strictType notStrict [t| Bool |]
    , varStrictType (mkName "preview_width") $ strictType notStrict
                                                 [t| Integer |]
    , varStrictType (mkName "preview_height") $ strictType notStrict
                                                 [t| Integer |]
    ]
  ] [mkName "Show", mkName "Eq"]

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
      , HBooru.Parsers.GenericBooru.id = read id
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

-- | Reads a lowercase 'Bool' string representation into its Haskell type. Note
-- that this is a partial function.
parseBool :: String -> Bool
parseBool "false" = False
parseBool "true" = True
