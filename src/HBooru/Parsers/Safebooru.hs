{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  HBooru.Parsers.Safebooru
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://safebooru.org/ safebooru>.
module HBooru.Parsers.Safebooru where

import Data.List
import Data.Vinyl
import HBooru.Parsers.FieldParsers
import HBooru.Types
import Text.XML.HXT.Core hiding (mkName)

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Safebooru = Safebooru

-- | Safebooru post record.
type SafebooruPost = R
  '[ "height"
   , "score"
   , "file_url"
   , "parent_id"
   , "sample_url"
   , "sample_width"
   , "sample_height"
   , "preview_url"
   , "rating"
   , "tags"
   , "id"
   , "width"
   , "change"
   , "md5"
   , "creator_id"
   , "has_children"
   , "created_at"
   , "status"
   , "source"
   , "has_notes"
   , "has_comments"
   , "preview_width"
   , "preview_height"
   ]

-- | Parser arrow used for Safebooru.
parsePost ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒ cat XmlTree SafebooruPost
parsePost = hasName "post"
  >>> heightA <:+> scoreA <:+> file_urlA <:+> parent_idA <:+> sample_urlA
  <:+> sample_widthA <:+> sample_heightA <:+> preview_urlA <:+> ratingA
  <:+> tagsA <:+> idA <:+> widthA <:+> changeA <:+> md5A <:+> creator_idA
  <:+> has_childrenA <:+> created_atA <:+> statusA <:+> sourceA <:+> has_notesA
  <:+> has_commentsA <:+> preview_widthA <:+> preview_heightA

instance Postable Safebooru XML where
  postUrl _ _ ts =
    let tags' = intercalate "+" ts
    in "http://safebooru.org/index.php?page=dapi&s=post&q=index&limit=100&tags="
       ++ tags'
  hardLimit _ _ = Limit 100

instance PostablePaged Safebooru XML

instance Site Safebooru where

instance PostParser Safebooru XML where
  type ImageTy Safebooru XML = SafebooruPost
  parseResponse _ = runLA (xreadDoc /> parsePost) . getResponse

instance Counted Safebooru XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
