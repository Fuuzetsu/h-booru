{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  HBooru.Parsers.Ichijou
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://safebooru.org/ safebooru>.
module HBooru.Parsers.Ichijou where

import           Data.List
import qualified HBooru.Parsers.GenericBooru as G
import           HBooru.Parsers.GenericBooru.TH (makePost)
import           HBooru.Types
import           Language.Haskell.TH.Syntax (mkName)
import           Text.XML.HXT.Core hiding (mkName)

-- | Data type for safebooru posts generated using 'makePost'.
$(makePost (mkName "IchijouPost"))

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Ichijou = Ichijou

instance Postable Ichijou XML where
  postUrl _ _ ts =
    let tags = intercalate "+" ts
    in "http://ichijou.org/index.xml?tags=" ++ tags
  hardLimit _ = Limit 100

instance Site Ichijou where

instance PostParser Ichijou XML where
  type ImageTy Ichijou XML = IchijouPost
  parseResponse _ = map (`betweenPosts` IchijouPost)
                    . runLA (xreadDoc /> G.parsePost) . getResponse

instance Counted Ichijou XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
