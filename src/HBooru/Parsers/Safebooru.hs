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
type SafebooruPost = PlainRec
  '[ "height" ::: Integer
   , "score" ::: Integer
   , "file_url" ::: String
   , "parent_id" ::: Maybe Integer
   , "sample_url" ::: String
   , "sample_width" ::: Integer
   , "sample_height" ::: Integer
   , "preview_url" ::: String
   , "rating" ::: Rating
   , "tags" ::: [Tag]
   , "id" ::: Integer
   , "width" ::: Integer
   , "change" ::: String
   , "md5" ::: String
   , "creator_id" ::: Integer
   , "has_children" ::: Bool
   , "created_at" ::: String
   , "status" ::: String
   , "source" ::: String
   , "has_notes" ::: Maybe Bool
   , "has_comments" ::: Maybe Bool
   , "preview_width" ::: Integer
   , "preview_height" ::: Integer
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
       ++ tags' ++ "&pid=0"
  hardLimit _ = Limit 100

instance Site Safebooru where

instance PostParser Safebooru XML where
  type ImageTy Safebooru XML = SafebooruPost
  parseResponse _ = runLA (xreadDoc /> parsePost) . getResponse

instance Counted Safebooru XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
