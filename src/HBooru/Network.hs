{-# LANGUAGE UnicodeSyntax #-}
-- |
-- Module      :  HBooru.Network
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module providing functions to interface with some booru sites.
-- Amongst other things, it should (semi-transparently) handle post count
-- limits. The user should simply be able to ask for all images with certain
-- rather than worrying about hard limits per page set by the sites &c.
module HBooru.Network where

import Control.Applicative ((<$>))
import HBooru.Types
import Network.HTTP.Conduit (simpleHttp)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.UTF8

-- | Given a 'Site', 'DataFormat' and a list of 'Tag's, naively fetch the first
-- page or so and parse it to the appropriate image type. Both the site and the
-- format need to together form an instance of 'Postable' and the data format
-- has to exist in an instance of 'CoerceResponse'. Uses 'fetchPostPage' to
-- fetch the data.
fetchTaggedPosts
  ∷ (Postable s d, CoerceResponse d r) ⇒ s → d → [Tag] → IO [ImageTy s d]
fetchTaggedPosts s d ts = parseResponse s <$> fetchPostPage s d ts

-- | Given an instance of 'Postable', 'CoerceResponse', and a list of 'Tag's,
-- fetch the post page.
fetchPostPage ∷ (Postable s d, CoerceResponse d r) ⇒ s → d → [Tag] → IO r
fetchPostPage s d ts = toResponse d . toString . toStrict
                       <$> simpleHttp (postUrl s d ts)

-- | Uses 'fetchPostPage' to parse the number of posts available based on
-- provided 'Tag's.
fetchPostCount
  ∷ (Postable s r, Counted s r, CoerceResponse r a) ⇒ s → r → [Tag] → IO Integer
fetchPostCount s d ts = parseCount s <$> fetchPostPage s d ts
