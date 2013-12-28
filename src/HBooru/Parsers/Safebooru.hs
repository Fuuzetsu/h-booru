{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  HBooru.Parsers.Safebooru
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://safebooru.org/ safebooru>.
module HBooru.Parsers.Safebooru where

import           Data.List
import qualified HBooru.Parsers.GenericBooru as G
import           HBooru.Parsers.GenericBooru.TH (makePost)
import           HBooru.Types
import           Language.Haskell.TH.Syntax (mkName)
import           Text.XML.HXT.Core hiding (mkName)

-- | Data type for safebooru posts generated using 'G.makePost'.
$(makePost (mkName "SafebooruPost"))

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Safebooru = Safebooru

instance Postable Safebooru XML where
  postUrl _ _ ts =
    let tags = intercalate "+" ts
    in "http://safebooru.org/index.php?page=dapi&s=post&q=index&limit=100&tags="
       ++ tags ++ "&pid=0"
  hardLimit _ = Limit 100

instance Site Safebooru where

instance PostParser Safebooru XML where
  type ImageTy Safebooru XML = SafebooruPost
  parseResponse _ = map (`G.fromGeneric` SafebooruPost)
                    . runLA (xreadDoc /> G.parsePost) . getResponse

instance Counted Safebooru XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
