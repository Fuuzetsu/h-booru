{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import Data.ByteString.Lazy (toStrict, writeFile)
import Data.ByteString.UTF8
import Data.Either (rights)
import Data.Monoid (mconcat)
import Data.Traversable (traverse)
import Data.List (intersect)
import Data.List.Split (chunksOf)
import HBooru.Parsers.Ichijou
import HBooru.Types
import Network.HTTP.Conduit (simpleHttp, HttpException(..))

-- | Given a 'Site', 'DataFormat' and a list of 'Tag's, naively fetch the first
-- page or so and parse it to the appropriate image type. Both the site and the
-- format need to together form an instance of 'Postable' and the data format
-- has to exist in an instance of 'CoerceResponse'. Uses 'fetchPostPage' to
-- fetch the data.
fetchTaggedPosts
  ∷ (Postable s d, CoerceResponse d r) ⇒ s → d → [Tag] → ExcIO [ImageTy s d]
fetchTaggedPosts s d ts = parseResponse s <$> fetchPostPage s d ts

-- | As 'fetchTaggedPosts' but works with sites which allow indexing by page.
fetchTaggedPostsIndexed ∷ (CoerceResponse r a, PostablePaged s r)
                          ⇒ s → r → [Tag] → Integer → ExcIO [ImageTy s r]
fetchTaggedPostsIndexed s d ts i =
  parseResponse s <$> fetchPostPageIndexed s d ts i

-- | Given an instance of 'Postable', 'CoerceResponse', and a list of 'Tag's,
-- fetch the post page.
fetchPostPage ∷ (Postable s d, CoerceResponse d r) ⇒ s → d → [Tag] → ExcIO r
fetchPostPage s d ts = fetchResponse (postUrl s d ts) d

-- | Given an instance of 'Postable', 'CoerceResponse', and a list of 'Tag's,
-- fetch the post page.
fetchPostPageIndexed ∷ (PostablePaged s d, CoerceResponse d r)
                       ⇒ s → d → [Tag] → Integer → ExcIO r
fetchPostPageIndexed s d ts i = fetchResponse (postUrlPaged s d ts i) d

-- | Given a URL and protocol, tries to fetch a response.
fetchResponse ∷ CoerceResponse r r' ⇒ String → r → ExcIO r'
fetchResponse u r = do
  liftIO (try (simpleHttp u)) >>= \case
    Left (e ∷ HttpException) → throwError $ Network e
    Right x → return . toResponse r . toString . toStrict $ x

-- | Uses 'fetchPostPage' to parse the number of posts available based on
-- provided 'Tag's.
fetchPostCount
  ∷ (Postable s r, Counted s r, CoerceResponse r a) ⇒ s → r → [Tag]
  → ExcIO Integer
fetchPostCount s d ts = parseCount s <$> fetchPostPage s d ts

-- | Attemps to fetch all posts from a site, from all its pages. The
-- upper limit of images per page is used.
--
-- This function deals with sites that impose a limit on number of
-- tags accepted by taking intersection of requests for all the
-- various tags to get around the limitation, hence the 'Eq'
-- constraint.
fetchAllTaggedPosts ∷ (CoerceResponse r a, PostablePaged s r, Eq (ImageTy s r))
                      ⇒ s → r → [Tag] → IO [ImageTy s r]
fetchAllTaggedPosts s r ts = case tagLimit s r of
  Limit x → traverse run (chunksOf (fromIntegral x) ts) >>= return . \case
    [] → []
    i:is → Prelude.foldr Data.List.intersect i is
  _ → run ts
  where
    run [] = return []
    run ts' = do
     runErrorT (fetchPostCount s r ts') >>= \case
      Left e → print e >> return []
      Right i → do
        let count = fromIntegral i ∷ Double
            pages = case hardLimit s r of
              NoLimit → 0
              Limit x → max 0 (ceiling $ (count / fromIntegral x) - 1)
        r' ← mapM (runErrorT . fetchTaggedPostsIndexed s r ts) [0 .. pages]
        return . concat $ rights r'

data DownloadStatus = OK String
                    | Failed (Either HttpException IOException, String)
                    | EndOfQueue
                    deriving Show

-- | Downloads the given files. Writes the status information back to
-- the provided TChan.
downloadFiles ∷ [(String, FilePath)] -- ^ URL with save location
              → TChan DownloadStatus -- ^ Channel to send back status info on
              → Int -- ^ Max threads to run at once. Bounded to minimum of 1.
              → IO ()
downloadFiles ts ds mt = do
  tv ← atomically $ newTVar ts
  count ← atomically $ newTVar (0 ∷ Int)
  let maxThreads = max 1 mt
      wf x = x `seq` Data.ByteString.Lazy.writeFile x
      modCount f = atomically $ modifyTVar count (\x -> max 0 (f x))
      runDownload (url, path) =
        try (simpleHttp url) >>= \case
          Left (e ∷ HttpException) →
            atomically . writeTChan ds $ Failed (Left e, url)
          Right c → try (wf path c) >>= atomically . \case
            Left (e ∷ IOException) → writeTChan ds $ Failed (Right e, url)
            Right _ → writeTChan ds $ OK url
      readVs = atomically $ liftM2 (,) (readTVar tv) (readTVar count)
      spawnThreads = readVs >>= \case
        ([], 0) → atomically $ writeTChan ds EndOfQueue
        (ys, n) | n >= maxThreads → spawnThreads
                | otherwise → case ys of
                  [] → threadDelay 10000 >> spawnThreads
                  x:xs → void $ do
                    atomically (writeTVar tv xs)
                    forkIO $ modCount succ >> runDownload x >> modCount pred
                    spawnThreads
  spawnThreads
