{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  HBooru.Parsers.Konachan
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://konachan.com/ Konachan>.

module HBooru.Parsers.Konachan where

import           Data.List
import qualified HBooru.Parsers.GenericBooru as G
import           HBooru.Parsers.GenericBooru.TH (makePost)
import           HBooru.Types
import           Language.Haskell.TH (mkName)
import           Text.XML.HXT.Core hiding (mkName)

-- | Data type for Konachan posts generated using 'makePost'.
$(makePost (mkName "KonachanPost"))

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Konachan = Konachan

instance Postable Konachan XML where
  postUrl _ _ ts =
    let tags = intercalate "+" ts
    in "https://konachan.com/post/index.xml?tags=" ++ tags
  hardLimit _ = Limit 100

instance Site Konachan where

instance PostParser Konachan XML where
  type ImageTy Konachan XML = KonachanPost
  parseResponse _ = map (`betweenPosts` KonachanPost)
                    . runLA (xreadDoc /> G.parsePost) . getResponse

instance Counted Konachan XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
