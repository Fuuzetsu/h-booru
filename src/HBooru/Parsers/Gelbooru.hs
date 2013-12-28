{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  HBooru.Parsers.Gelbooru
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://gelbooru.com/ Gelbooru>.
module HBooru.Parsers.Gelbooru where

import Data.List
import HBooru.Types
import Text.XML.HXT.Core hiding (mkName)
import qualified HBooru.Parsers.GenericBooru as G
import Language.Haskell.TH (mkName)

-- | Data type for Gelbooru posts generated using 'G.makePost'.
$(G.makePost (mkName "GelbooruPost"))

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Gelbooru = Gelbooru

instance Postable Gelbooru XML where
  postUrl _ _ ts =
    let tags = intercalate "+" ts
    in "http://gelbooru.com/index.php?page=dapi&s=post&q=index&limit=100&tags="
       ++ tags ++ "&pid=0"
  hardLimit _ = Limit 100

instance Site Gelbooru where

instance PostParser Gelbooru XML where
  type ImageTy Gelbooru XML = GelbooruPost
  parseResponse _ = map (`G.fromGeneric` GelbooruPost)
                    . runLA (xreadDoc /> G.parsePost) . getResponse

instance Counted Gelbooru XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
