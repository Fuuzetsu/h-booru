{-# LANGUAGE UnicodeSyntax #-}
-- |
-- Module      :  HBooru.Network
-- Copyright   :  (c) Mateusz Kowalczyk 2013
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
-- has to exist in an instance of 'CoerceResponse'.
fetchTaggedPosts
  :: (Postable s d, CoerceResponse d r) ⇒ s → d → [Tag] → IO [ImageTy s d]
fetchTaggedPosts s d ts = do
  parseResponse s . toResponse d . toString . toStrict
  <$> simpleHttp (postUrl s d ts)
