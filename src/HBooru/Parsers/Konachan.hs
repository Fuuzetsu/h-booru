{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  HBooru.Parsers.Konachan
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://konachan.com/ Konachan>.

module HBooru.Parsers.Konachan where

import Data.List
import Data.Vinyl
import HBooru.Parsers.FieldParsers
import HBooru.Types
import Text.XML.HXT.Core hiding (mkName)

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Konachan = Konachan

-- | Konachan post record
type KonachanPost = PlainRec
  '[ "actual_preview_height" ::: Integer
   , "actual_preview_width" ::: Integer
   , "author" ::: String
   , "change" ::: String
   , "created_at" ::: String
   , "file_size" ::: String
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
   , "jpeg_file_size" ::: String
   , "jpeg_height" ::: Integer
   , "jpeg_url" ::: String
   , "jpeg_width" ::: Integer
   , "md5" ::: String
   , "preview_height" ::: Integer
   , "preview_url" ::: String
   , "preview_width" ::: Integer
   , "rating" ::: Rating
   , "sample_file_size" ::: String
   , "sample_height" ::: Integer
   , "sample_url" ::: String
   , "sample_width" ::: Integer
   , "score" ::: Integer
   , "source" ::: String
   , "status" ::: String
   , "tags" ::: [Tag]
   , "width" ::: Integer
   ]

-- | Parser arrow used for Konachan.
parsePost ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒ cat XmlTree KonachanPost
parsePost = hasName "post"
  >>> actual_preview_heightA <:+> actual_preview_widthA <:+> authorA
  <:+> changeA  <:+> created_atA <:+> file_sizeA <:+> file_urlA
  <:+> framesA <:+> frames_pendingA <:+> frames_pending_stringA
  <:+> frames_stringA <:+> has_childrenA
  <:+> heightA <:+> idA <:+> is_heldA <:+> is_shown_in_indexA
  <:+> jpeg_file_sizeA <:+> jpeg_heightA <:+> jpeg_urlA <:+> jpeg_widthA
  <:+> md5A <:+> preview_heightA <:+> preview_urlA <:+> preview_widthA
  <:+> ratingA <:+> sample_file_sizeA <:+> sample_heightA <:+> sample_urlA
  <:+> sample_widthA <:+> scoreA <:+> sourceA <:+> statusA <:+> tagsA
  <:+> widthA

instance Postable Konachan XML where
  postUrl _ _ ts =
    let tags' = intercalate "+" ts
    in "https://konachan.com/post/index.xml?tags=" ++ tags'
  hardLimit _ = Limit 100

instance Site Konachan where

instance PostParser Konachan XML where
  type ImageTy Konachan XML = KonachanPost
  parseResponse _ = runLA (xreadDoc /> parsePost) . getResponse

instance Counted Konachan XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
