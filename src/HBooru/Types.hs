{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
module HBooru.Types
       (Limit(..)
       , Tag
       )
       where

import Network.HTTP
import Network.HTTP.Base

data Limit = NoLimit | Limit Integer
data Image
data Tag = Tag String

data XMLResponse = XMLResponse String
data JSONResponse = JSONResponse String

class BooruResponse a where

instance BooruResponse XMLResponse where
instance BooruResponse JSONResponse where

-- | We can have different image info from different APIs, maybe?
class BooruImage r p i | r p -> i where

class BooruImage r a i => BooruParser r a i where
  hardLimit :: a -> Limit
  processResponse :: BooruResponse r => r -> [i]


data GelbooruImage = GelbooruImage { height, width :: Integer
                                   , imageLink :: String
                                   }
data GelbooruParser
data Hole

instance BooruImage XMLResponse GelbooruParser GelbooruImage where

instance BooruParser XMLResponse GelbooruParser GelbooruImage where
  hardLimit _ = Limit 100
  processResponse (XMLResponse r) = [GelbooruImage 53 32 "http://fake.net"]
