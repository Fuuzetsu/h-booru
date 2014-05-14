{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative ((<$>))
import Data.Vinyl
import HBooru.Network
import HBooru.Parsers.Gelbooru
import HBooru.Parsers.Ichijou
import HBooru.Parsers.Konachan
import HBooru.Parsers.Safebooru
import HBooru.Parsers.Yandere
import HBooru.Types
import System.Environment (getArgs)

main ∷ IO ()
main = getArgs >>= \case
  [] → putStrLn help
  xs → fetchImageLinks xs >>= mapM_ putStrLn

fetchImageLinks ∷ [Tag] → IO [String]
fetchImageLinks xs = do
  let f p = map (file_url `rGet`) <$> fetchAllTaggedPosts p XML xs
  g ← f Gelbooru
  i ← f Ichijou
  k ← f Konachan
  s ← f Safebooru
  y ← f Yandere
  let ls = g ++ i ++ k ++ s ++ y
  return $ length ls `seq` ls

help ∷ String
help = unlines $
       [ "Usage: h-booru tag1 [tag2] … [tagn]"
       , ""
       , "Prints a list of links matching the tags"
       ]
