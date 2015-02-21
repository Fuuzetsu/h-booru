{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  HBooru.Parsers.Ichijou
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://ichijou.org/ Ichijou/vectorbooru>.
module HBooru.Parsers.Ichijou where

import Data.List
import HBooru.Parsers.FieldParsers
import HBooru.Types
import Text.XML.HXT.Core hiding (mkName)

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Ichijou = Ichijou deriving (Show, Eq)

-- | Ichijou post record alias
type IchijouPost = PR
  '[ "creator_id"
   , "md5"
   , "status"
   , "preview_height"
   , "has_notes"
   , "author"
   , "source"
   , "score"
   , "file_size"
   , "sample_width"
   , "width"
   , "file_url"
   , "sample_height"
   , "has_children"
   , "tags"
   , "change"
   , "preview_url"
   , "has_comments"
   , "id"
   , "sample_url"
   , "rating"
   , "created_at"
   , "preview_width"
   , "parent_id"
   , "height"
   ]

-- | Parser arrow used for Ichijou.
parsePost ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒ cat XmlTree IchijouPost
parsePost = hasName "post"
  >>> creator_idA <:+> md5A <:+> statusA <:+> preview_heightA <:+> has_notesA
  <:+> authorA <:+> sourceA <:+> scoreA <:+> file_sizeA <:+> sample_widthA
  <:+> widthA <:+> file_urlA <:+> sample_heightA <:+> has_childrenA <:+> tagsA
  <:+> changeA <:+> preview_urlA <:+> has_commentsA <:+> idA <:+> sample_urlA
  <:+> ratingA <:+> created_atA <:+> preview_widthA <:+> parent_idA <:+> heightA

instance Postable Ichijou XML where
  postUrl _ _ ts =
    let tags' = intercalate "+" ts
    in "http://ichijou.org/post/index.xml?tags=" ++ tags'
  hardLimit _ _ = Limit 1000
  tagLimit _ _ = Limit 2

instance PostablePaged Ichijou XML where
  postUrlPaged s r ts i = postUrl s r ts ++ "&page=" ++ show (i + 1)

instance Site Ichijou where

instance PostParser Ichijou XML where
  type ImageTy Ichijou XML = IchijouPost
  parseResponse _ = runLA (xreadDoc /> parsePost) . getResponse

instance Counted Ichijou XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
