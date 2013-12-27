{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
module HBooru.Types

       where

import Control.Applicative
import Data.List
import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Stream hiding (simpleHTTP)
import Text.Read
import Text.XML.Stream.Parse

class Show a ⇒ Tag a where
  showTag ∷ a → String

data Limit = NoLimit | Limit Integer

data XMLResponse = XMLResponse String
data JSONResponse = JSONResponse String

data Hole

class BResponse r where
  getResponse ∷ r → String

instance BResponse XMLResponse where
  getResponse (XMLResponse x) = x

instance BResponse JSONResponse where
  getResponse (JSONResponse x) = x

class BImage i where

class BParser p r where

class BParser p r ⇒ BCount p r | r → p where
  parseCount ∷ r → Integer

class (BImage i, BResponse r) ⇒ BImageParser p r i | r i → p where
  hardLimit ∷ p → Limit
  parseImages ∷ r → [i]
  tagURL ∷ Tag t ⇒ r → [t] → String
