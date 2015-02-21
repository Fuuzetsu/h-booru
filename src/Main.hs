{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (void, filterM)
import Data.Map
import Data.Monoid
import Data.Vinyl (rget, Rec, RElem)
import Data.Vinyl.TypeLevel (RIndex)
import HBooru.Network
import HBooru.Parsers.Gelbooru
import HBooru.Parsers.Ichijou
import HBooru.Parsers.Konachan
import HBooru.Parsers.Safebooru
import HBooru.Parsers.Yandere
import HBooru.Types
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.FilePath

main ∷ IO ()
main = getArgs >>= \case
  [] → putStr help
  "-d":d:ts → downloadTo d ts
  xs → fetchImageLinks xs >>= \x → mapM_ putStrLn [ y | Right y ← x ]

-- | Downloads all the images with the given tag to the given
-- directory.
downloadTo ∷ FilePath → [Tag] → IO ()
downloadTo fp xs = doesDirectoryExist fp >>= \case
  False → print (fp <> " doesn't exist.") >> exitFailure
  True → do
    let f p = run <$> fetchAllTaggedPosts p XML xs
          where
            run s =
              fromList [ (unVAL $ md5 `rget` r,
                          unVAL $ file_url `rget` r) | Right r ← s ]

    g ← f Gelbooru
    i ← f Ichijou
    k ← f Konachan
    s ← f Safebooru
    y ← f Yandere
    let ls = g <> i <> k <> s <> y
        mkFp (m, u) = let e = snd $ splitExtension u
                      in fp </> m <.> e
    dcs ← Prelude.map (fp </>) <$> getDirectoryContents fp
    let notInFiles = (`notElem` dcs) . snd
        fs = Prelude.filter notInFiles [ (snd x, mkFp x) | x ← assocs ls ]
        lfs = show $ length fs
    ds ← atomically newTChan
    got ← newTVarIO 0
    let loop = atomically (readTChan ds) >>= \case
          EndOfQueue → putStrLn "Done."
          x → do
            v ← atomically $ modifyTVar got succ >> readTVar got
            putStrLn (concat ["(", show v, "/", lfs, "): ", show x]) >> loop

    void . forkIO $ downloadFiles fs ds 5
    loop


-- | Fetches a map of images, with keys being the md5 and values being
-- URLs.
fetchImageLinks ∷ [Tag] → IO [Parse String]
fetchImageLinks xs = do
  let f p = Prelude.map getInfo <$> fetchAllTaggedPosts p XML xs
        where
          getInfo (Left (PF m)) = Left . PF $ unwords [show p, m]
          getInfo (Right r) = return . unVAL $ file_url `rget` r
  (g, i, k, s, y) ← runConcurrently $ (,,,,)
                    <$> Concurrently (f Gelbooru)
                    <*> Concurrently (f Ichijou)
                    <*> Concurrently (f Konachan)
                    <*> Concurrently (f Safebooru)
                    <*> Concurrently (f Yandere)
  let ls = g <> i <> k <> s <> y
  return $ length ls `seq` ls

help ∷ String
help = unlines $
  [ "Usage: h-booru tag1 [tag2] … [tagN]"
  , ""
  , "Prints a list of links matching the tags"
  , ""
  , "h-booru -d DIRECTORY tag1 [tag2] … [tagN]"
  , ""
  , "Downloads the files with the given tags to the given directory."
  , "Naming scheme is md5.originalextension"
  , "The downloader will skip files it already sees downloaded, by filename."
  ]
