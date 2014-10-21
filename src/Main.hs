{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Control.Applicative ((<$>))
import Data.Vinyl (rGet)
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
  xs → fetchImageLinks xs >>= \x → mapM_ putStrLn [ y | Right y ← x ]

fetchImageLinks ∷ [Tag] → IO [Parse String]
fetchImageLinks xs = do
  let f p = map getUrl <$> fetchAllTaggedPosts p XML xs
        where
          getUrl (Left (PF m)) = Left . PF $ unwords [show p, m]
          getUrl (Right r) = return $ file_url `rGet` r
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
