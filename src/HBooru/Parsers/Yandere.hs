{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  HBooru.Parsers.Yandere
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module for parsing content from <http://yande.re/ Yandere>,
-- known in the past as <http://mou.imouto.org MouImouto>.
module HBooru.Parsers.Yandere where

import           Data.List
import qualified HBooru.Parsers.GenericBooru as G
import           HBooru.Parsers.GenericBooru.TH (makePost)
import           HBooru.Types
import           Language.Haskell.TH (mkName)
import           Text.XML.HXT.Core hiding (mkName)

-- | Data type for Yandere posts generated using 'makePost'.
$(makePost (mkName "YanderePost"))

-- | We use this type and its 'Site' instance to distinguish
-- between various parsers.
data Yandere = Yandere

instance Postable Yandere XML where
  postUrl _ _ ts =
    let tags = intercalate "+" ts
    in "https://yande.re/post/index.xml?tags=" ++ tags
  hardLimit _ = Limit 100

instance Site Yandere where

instance PostParser Yandere XML where
  type ImageTy Yandere XML = YanderePost
  parseResponse _ = map (`betweenPosts` YanderePost)
                    . runLA (xreadDoc /> G.parsePost) . getResponse

instance Counted Yandere XML where
  parseCount _ = read . head . runLA (xreadDoc >>> hasName "posts"
                                      >>> getAttrValue "count") . getResponse
