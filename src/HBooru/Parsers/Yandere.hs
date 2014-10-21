{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  HBooru.Parsers.Yandere
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://yande.re/ Yandere>,
-- known in the past as <http://moe.imouto.org MoeImouto>.
module HBooru.Parsers.Yandere where

import Data.List
import HBooru.Parsers.FieldParsers
import HBooru.Types
import Text.XML.HXT.Core hiding (mkName)

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Yandere = Yandere deriving (Show, Eq)

-- | Alias for a record representing typical Yandere post.
type YanderePost = PR
  '[ "actual_preview_height"
   , "actual_preview_width"
   , "author"
   , "change"
   , "created_at"
   , "creator_id"
   , "file_size"
   , "file_url"
   , "frames"
   , "frames_pending"
   , "frames_pending_string"
   , "frames_string"
   , "has_children"
   , "height"
   , "id"
   , "is_held"
   , "is_shown_in_index"
   , "jpeg_file_size"
   , "jpeg_height"
   , "jpeg_url"
   , "jpeg_width"
   , "md5"
   , "preview_height"
   , "preview_url"
   , "preview_width"
   , "rating"
   , "sample_file_size"
   , "sample_height"
   , "sample_url"
   , "sample_width"
   , "score"
   , "source"
   , "status"
   , "tags"
   , "width"
   ]

-- | Parser arrow for XML Yandere posts.
parsePost ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒ cat XmlTree YanderePost
parsePost = hasName "post"
  >>> actual_preview_heightA <:+> actual_preview_widthA <:+> authorA
  <:+> changeA <:+> created_atA <:+> creator_idA <:+> file_sizeA <:+> file_urlA
  <:+> framesA <:+> frames_pendingA <:+> frames_pending_stringA
  <:+> frames_stringA <:+> has_childrenA <:+> heightA <:+> idA <:+> is_heldA
  <:+> is_shown_in_indexA <:+> jpeg_file_sizeA <:+> jpeg_heightA <:+> jpeg_urlA
  <:+> jpeg_widthA <:+> md5A <:+> preview_heightA <:+> preview_urlA
  <:+> preview_widthA <:+> ratingA <:+> sample_file_sizeA <:+> sample_heightA
  <:+> sample_urlA <:+> sample_widthA <:+> scoreA <:+> sourceA <:+> statusA
  <:+> tagsA <:+> widthA

instance Postable Yandere XML where
  postUrl _ _ ts =
    let tgs = intercalate "+" ts
    in "https://yande.re/post/index.xml?tags=" ++ tgs
  hardLimit _ _ = Limit 1000

instance PostablePaged Yandere XML where
  postUrlPaged s r ts i = postUrl s r ts ++ "&page=" ++ show (i + 1)

instance Site Yandere where

instance PostParser Yandere XML where
  type ImageTy Yandere XML = YanderePost
  parseResponse _ = runLA (xreadDoc /> parsePost) . getResponse

instance Counted Yandere XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
