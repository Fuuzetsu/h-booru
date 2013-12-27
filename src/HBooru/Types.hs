{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module HBooru.Types where

class DataFormat a where

data XML = XML
data JSON = JSON

instance DataFormat XML where
instance DataFormat JSON where

class Response r ⇒ ToResponse x r | x → r where
  toResponse ∷ x → String → r

instance ToResponse XML XMLResponse where
  toResponse _ = XMLResponse

instance ToResponse JSON JSONResponse where
  toResponse _ = JSONResponse

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
