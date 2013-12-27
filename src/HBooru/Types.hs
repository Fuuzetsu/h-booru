{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module HBooru.Types where

class DataFormat a where

data XML = XML deriving (Show, Eq)
data JSON = JSON deriving (Show, Eq)

instance DataFormat XML where
instance DataFormat JSON where

class Response r ⇒ CoerceResponse x r | x → r, r → x where
  toResponse ∷ x → String → r
  fromResponse ∷ r → x

instance CoerceResponse XML XMLResponse where
  toResponse _ = XMLResponse
  fromResponse _ = XML

instance CoerceResponse JSON JSONResponse where
  toResponse _ = JSONResponse
  fromResponse _ = JSON

class (Site s, Response r) ⇒ PostParser s r where
  type ImageTy s r
  parseResponse ∷ s → r → [ImageTy s r]

class (Site s, Response r) ⇒ Counted s r where
  parseCount ∷ s → r → Integer

class (Site s, DataFormat r) ⇒ Postable s r where
  postUrl ∷ Tag t ⇒ s → [t] → String

class Site s where
  hardLimit ∷ s → Maybe Limit

data Rating = Safe | Questionable | Explicit deriving (Show, Eq)

class Show a ⇒ Tag a where
  showTag ∷ a → String

data Limit = NoLimit | Limit Integer deriving (Show, Eq)

data XMLResponse = XMLResponse String deriving Show
data JSONResponse = JSONResponse String  deriving Show

class Response r where
  getResponse ∷ r → String

instance Response XMLResponse where
  getResponse (XMLResponse x) = x

instance Response JSONResponse where
  getResponse (JSONResponse x) = x
