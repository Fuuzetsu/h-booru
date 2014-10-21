{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

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

-- | As 'fetchTaggedPosts' but works with sites which allow indexing by page.
fetchTaggedPostsIndexed ∷ (CoerceResponse r a, PostablePaged s r)
                          ⇒ s → r → [Tag] → Integer → IO [ImageTy s r]
fetchTaggedPostsIndexed s d ts i =
  parseResponse s <$> fetchPostPageIndexed s d ts i

-- | Given an instance of 'Postable', 'CoerceResponse', and a list of 'Tag's,
-- fetch the post page.
fetchPostPage ∷ (Postable s d, CoerceResponse d r) ⇒ s → d → [Tag] → IO r
fetchPostPage s d ts = fetchResponse (postUrl s d ts) d

-- | Given an instance of 'Postable', 'CoerceResponse', and a list of 'Tag's,
-- fetch the post page.
fetchPostPageIndexed ∷ (PostablePaged s d, CoerceResponse d r)
                       ⇒ s → d → [Tag] → Integer → IO r
fetchPostPageIndexed s d ts i = fetchResponse (postUrlPaged s d ts i) d

-- | Given a URL and protocol, tries to fetch a response.
fetchResponse ∷ CoerceResponse r r' ⇒ String → r → IO r'
fetchResponse u r = toResponse r . toString . toStrict <$> simpleHttp u

-- | Uses 'fetchPostPage' to parse the number of posts available based on
-- provided 'Tag's.
fetchPostCount
  ∷ (Postable s r, Counted s r, CoerceResponse r a) ⇒ s → r → [Tag] → IO Integer
fetchPostCount s d ts = parseCount s <$> fetchPostPage s d ts

-- | Attemps to fetch all posts from a site, from all its pages. The
-- upper limit of images per page is used.
fetchAllTaggedPosts
  ∷ (CoerceResponse r a, PostablePaged s r) ⇒ s → r → [Tag] → IO [ImageTy s r]
fetchAllTaggedPosts s r ts = do
  count ← fromIntegral <$> fetchPostCount s r ts ∷ IO Double
  let pages = case hardLimit s r of
        NoLimit → 0
        Limit x → max 0 (ceiling $ (count / fromIntegral x) - 1)
  concat <$> mapM (fetchTaggedPostsIndexed s r ts) [0 .. pages]
