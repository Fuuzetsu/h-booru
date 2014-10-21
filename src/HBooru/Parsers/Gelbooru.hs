{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  HBooru.Parsers.Gelbooru
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://gelbooru.com/ Gelbooru>.
module HBooru.Parsers.Gelbooru where

import Data.List
import HBooru.Parsers.FieldParsers
import HBooru.Types
import Text.XML.HXT.Core hiding (mkName)

-- | Record used for Gelbooru posts
type GelbooruPost = PR
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

-- | XML parser for Gelbooru used by "Postable Gelbooru XML" instance.
parsePost ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒ cat XmlTree GelbooruPost
parsePost = hasName "post"
  >>> heightA <:+> scoreA <:+> file_urlA <:+> parent_idA <:+> sample_urlA
  <:+> sample_widthA <:+> sample_heightA <:+> preview_urlA <:+> ratingA
  <:+> tagsA <:+> idA <:+> widthA <:+> changeA <:+> md5A <:+> creator_idA
  <:+> has_childrenA <:+> created_atA <:+> statusA <:+> sourceA <:+> has_notesA
  <:+> has_commentsA <:+> preview_widthA <:+> preview_heightA

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Gelbooru = Gelbooru deriving (Show, Eq)

instance Postable Gelbooru XML where
  postUrl _ _ ts =
    let tags' = intercalate "+" ts
    in "http://gelbooru.com/index.php?page=dapi&s=post&q=index&limit=100&tags="
       ++ tags'
  hardLimit _ _ = Limit 100

instance PostablePaged Gelbooru XML

instance Site Gelbooru where

instance PostParser Gelbooru XML where
  type ImageTy Gelbooru XML = GelbooruPost
  parseResponse _ = runLA (xreadDoc /> parsePost) . getResponse

instance Counted Gelbooru XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
