{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

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
import Data.Vinyl
import HBooru.Parsers.FieldParsers
import HBooru.Types
import Text.XML.HXT.Core hiding (mkName)

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Ichijou = Ichijou

-- | Ichijou post record alias
type IchijouPost = PlainRec
  '[ "creator_id" ::: Integer
   , "md5" ::: String
   , "status" ::: String
   , "preview_height" ::: Integer
   , "has_notes" ::: Maybe Bool
   , "author" ::: String
   , "source" ::: String
   , "score" ::: Integer
   , "file_size" ::: Integer
   , "sample_width" ::: Integer
   , "width" ::: Integer
   , "file_url" ::: String
   , "sample_height" ::: Integer
   , "has_children" ::: Bool
   , "tags" ::: [Tag]
   , "change" ::: String
   , "preview_url" ::: String
   , "has_comments" ::: Maybe Bool
   , "id" ::: Integer
   , "sample_url" ::: String
   , "rating" ::: Rating
   , "created_at" ::: String
   , "preview_width" ::: Integer
   , "parent_id" ::: Maybe Integer
   , "height" ::: Integer
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

instance PostablePaged Ichijou XML where
  postUrlPaged s r ts i = postUrl s r ts ++ "&page=" ++ show (i + 1)

instance Site Ichijou where

instance PostParser Ichijou XML where
  type ImageTy Ichijou XML = IchijouPost
  parseResponse _ = runLA (xreadDoc /> parsePost) . getResponse

instance Counted Ichijou XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
