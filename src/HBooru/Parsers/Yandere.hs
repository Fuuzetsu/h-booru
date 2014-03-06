{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  HBooru.Parsers.Yandere
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://yande.re/ Yandere>,
-- known in the past as <http://mou.imouto.org MouImouto>.
module HBooru.Parsers.Yandere where

import Data.List
import Data.Vinyl
import HBooru.Parsers.FieldParsers
import HBooru.Types
import Text.XML.HXT.Core hiding (mkName)

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Yandere = Yandere

-- | Alias for a record representing typical Yandere post.
type YanderePost = PlainRec
  '[ "actual_preview_height" ::: Integer
   , "actual_preview_width" ::: Integer
   , "author" ::: String
   , "change" ::: String
   , "created_at" ::: String
   , "creator_id" ::: Integer
   , "file_size" ::: Integer
   , "file_url" ::: String
   , "frames" ::: String
   , "frames_pending" ::: String
   , "frames_pending_string" ::: String
   , "frames_string" ::: String
   , "has_children" ::: Bool
   , "height" ::: Integer
   , "id" ::: Integer
   , "is_held" ::: Bool
   , "is_shown_in_index" ::: Bool
   , "jpeg_file_size" ::: Integer
   , "jpeg_height" ::: Integer
   , "jpeg_url" ::: String
   , "jpeg_width" ::: Integer
   , "md5" ::: String
   , "preview_height" ::: Integer
   , "preview_url" ::: String
   , "preview_width" ::: Integer
   , "rating" ::: Rating
   , "sample_file_size" ::: Integer
   , "sample_height" ::: Integer
   , "sample_url" ::: String
   , "sample_width" ::: Integer
   , "score" ::: Integer
   , "source" ::: String
   , "status" ::: String
   , "tags" ::: [Tag]
   , "width" ::: Integer
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
  hardLimit _ = Limit 100

instance Site Yandere where

instance PostParser Yandere XML where
  type ImageTy Yandere XML = YanderePost
  parseResponse _ = runLA (xreadDoc /> parsePost) . getResponse

instance Counted Yandere XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
