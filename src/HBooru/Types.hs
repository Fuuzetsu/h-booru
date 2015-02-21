{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  HBooru.Types
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module definining types used by the library.
module HBooru.Types where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.Error
import Data.Proxy
import GHC.TypeLits (Symbol)
import Data.Vinyl
import Network.HTTP.Conduit (HttpException(..))
import Prelude
import Text.XML.HXT.Core hiding (mkName, (<+>))

-- | Tags used for searching in sites. No special escaping is done.
-- Note that many sites would treat a tag like \"striped panties\"
-- as two separate tags and you wouldn't get the results you were after.
type Tag = String

-- | Data format used by various 'Site's. See instances for currently used
-- formats.
class DataFormat a where

-- | Used as one of the data formats.
data XML = XML deriving Show

-- | Used as one of the data formats.
data JSON = JSON deriving Show

instance DataFormat XML where
instance DataFormat JSON where

-- | Thanks to this class, we're able to provide instances converting
-- from a 'DataFormat' to 'Response'. This is useful if we need a 'DataFormat'
-- while we only have a type that's an instance of 'Response'. Note that the
-- functional dependency currently requires that there is only one way to coerce
-- between two types.
class Response r ⇒ CoerceResponse x r | x → r, r → x where
  -- | Given something and a 'String', we get the appropriate 'Response'.
  -- For example with @instance 'CoerceResponse' 'XML' 'XMLResponse'@:
  --
  -- >>> toResponse XML "<SomeXML></SomeXML>"
  -- XMLReponse "<SomeXML></SomeXML>"
  toResponse ∷ x → String → r
  -- | Given some kind of 'Response', we get the appropriate value back,
  -- depending on the class instance.
  -- For example with @instance 'CoerceResponse' 'XML' 'XMLResponse'@:
  --
  -- >>> fromResponse $ XMLReponse "<SomeXML></SomeXML>"
  -- XML
  fromResponse ∷ r → x

instance CoerceResponse XML XMLResponse where
  toResponse _ = XMLResponse
  fromResponse _ = XML

instance CoerceResponse JSON JSONResponse where
  toResponse _ = JSONResponse
  fromResponse _ = JSON

-- | Class specifying a parser that can fetch posts. A post usually
-- consists of links to the image, samples, and some meta-data. The
-- reason for this class is that sometimes we might get different
-- information based on the 'DataFormat' we use so we use type
-- families to denote this rather than forcing the library user to
-- make do with our best guess on what goes into the post. It also
-- allows us to use different post types for sites that provide
-- different information.
class (Site s, DataFormat r) ⇒ PostParser s r where
  type ImageTy s r
  -- | Given a parser working with 'DataFormat' specified by an instance of
  -- this class, we require through 'CoerceResponse' that it is able to parse
  -- responses in the format so what we actually pass into this function is
  -- the 'Site' this parser works with (so that we can pick the appropriate data
  -- type for the posts) and a 'Response' matching the 'DataFormat' (through a
  -- class instance). For @PostParser 'Gelbooru' 'XML'@ instance, example use
  -- might go like
  --
  -- @
  -- do fc \<- 'XMLResponse' <$> 'readFile' \"gelbooruResponse.xml\"
  --    -- the type of images is actually inferred for us
  --    let images ∷ ['HBooru.Parsers.Gelbooru.GelbooruPost']
  --        images = parseResponse 'HBooru.Parsers.Gelbooru.Gelbooru' fc
  --    return images
  -- @
  --
  -- The cool thing is that we can't feed anything but 'XMLResponse' to an
  -- XML parser.
  parseResponse ∷ CoerceResponse r r' ⇒ s → r' → [ImageTy s r]

-- | Describes whether a response from a 'Site' in given 'DataFormat'
-- allows us to get the information about total number of posts matching our
-- query. Some sites don't provide this information.
class (Site s, DataFormat r) ⇒ Counted s r where
  -- | Parses out the number of available images from a response.
  parseCount ∷ CoerceResponse r r' ⇒ s → r' → Integer

class (Counted s r, Postable s r) ⇒ PostablePaged s r where
  -- | Similar to 'postUrl' but requests images from specific page if
  -- the site allows it.
  postUrlPaged ∷ s → r → [Tag] → Integer → String
  postUrlPaged s r ts i = postUrl s r ts ++ "&pid=" ++ show i

-- | If we can make an API request to 'Site' in a specific 'DataFormat', we can
-- use instances of this class to pass in
class PostParser s r ⇒ Postable s r where
  -- | Given a 'Site', a 'DataFormat' and a list of 'Tag's, an instance of this
  -- class should be able to return a 'String' at which we can find data in
  -- 'DataFormat' format that honours our tags. This is effectively a URL
  -- builder for POST requests.
  postUrl ∷ s → r → [Tag] → String
  -- | Provides information about whether there's a hard limit on the amount of
  -- posts we can fetch from the site at once. The reason for this function here
  -- rather than in 'Site' is that we might be parsing data without an API we
  -- can post to at all and we're getting our data through other means.
  hardLimit ∷ s → r → Limit
  -- | Limit on a number of tags we can ask for at once. Some sites
  -- only allow a few tags to be requested at once but if we know this
  -- information ahead of time, we can make up to that by making
  -- multiple requests and unifying the results. Waste of bandwidth on
  -- both sides due to braindead setting. Also means that there's no
  -- reliable way of telling how many posts we're actually looking at
  -- before unification…
  --
  -- Defaults to 'NoLimit'.
  tagLimit ∷ s → r → Limit
  tagLimit _ _ = NoLimit

-- | Describes a site for a parser. The reason why this isn't a simple data type
-- is to allow us to write additional parsers in the future without modifying
-- this library if we wish to do so.
class Show s ⇒ Site s where

-- | Rating used on *booru sites.
data Rating = Safe | Questionable | Explicit deriving (Show, Eq)

-- | Denotes whethere there's a hard limit on the number of posts
-- we can fetch at a time from a site. NoLimit implies that we can fetch
-- everything at once and not that we don't know. See 'Counted' for a way to
-- potentially retrieve number of posts present on the site.
data Limit = NoLimit | Limit Integer deriving (Show, Eq)

-- | One of the formats we can receive responses from sites in. For things
-- like parsers parametrisation, use 'XML' instead and use methods in
-- 'CoerceResponse' if you need to.
data XMLResponse = XMLResponse String deriving Show

-- | One of the formats we can receive responses from sites in. For things
-- like parsers parametrisation, use 'JSON' instead and use methods in
-- 'CoerceResponse' if you need to.
data JSONResponse = JSONResponse String deriving Show

-- | Specifies what is considered a response. You'll almost certainly also
-- want new 'DataFormat' and 'CoerceResponse' instances if you're adding some
-- here. This class assumes that all responses carry the response in a string we
-- can extract. Note that this is not for use as network response if you're
-- scraping, only for putting data into after you have done all the error
-- checking and whatnot.
class Response r where
  -- | Extract the response string.
  getResponse ∷ r → String

instance Response XMLResponse where
  getResponse (XMLResponse x) = x

instance Response JSONResponse where
  getResponse (JSONResponse x) = x

instance Functor (LA XmlTree) where
  fmap f (LA g) = LA $ fmap fmap fmap f g

instance Applicative (LA XmlTree) where
  pure x = LA . const $ return x
  (<*>) = ap

instance Monad (LA XmlTree) where
  return = pure
  mx >>= f = (arr (\a -> mx >>> arr (\x -> (f x, a)) >>>
                      app) &&& arr Prelude.id)
             >>> app

-- | Parse failures from various parsers
newtype ParseFailure = PF String deriving (Show, Eq)

instance Error ParseFailure where
  noMsg = PF noMsg
  strMsg = PF . strMsg

-- | Alias for our parser monad with failure possibility
type Parse = Either ParseFailure

data RealWorldExcs = Network HttpException
                   | IOE IOException
                   | SomethingElse String
                   deriving (Show)

instance Error RealWorldExcs where
  noMsg = SomethingElse noMsg
  strMsg = SomethingElse . strMsg

type ExcIO a = ErrorT RealWorldExcs IO a

type family EL (f ∷ Symbol) ∷ ★ where
  EL "height"                = Integer
  EL "score"                 = Integer
  EL "file_url"              = String
  EL "parent_id"             = Maybe Integer
  EL "sample_url"            = String
  EL "sample_width"          = Integer
  EL "sample_height"         = Integer
  EL "preview_url"           = String
  EL "rating"                = Rating
  EL "tags"                  = [Tag]
  EL "id"                    = Integer
  EL "width"                 = Integer
  EL "change"                = Int
  EL "md5"                   = String
  EL "creator_id"            = Integer
  EL "has_children"          = Bool
  EL "created_at"            = String
  EL "status"                = String
  EL "source"                = String
  EL "has_notes"             = Maybe Bool
  EL "has_comments"          = Maybe Bool
  EL "preview_width"         = Integer
  EL "preview_height"        = Integer
  EL "author"                = String
  EL "frames"                = String
  EL "frames_pending"        = String
  EL "frames_pending_string" = String
  EL "frames_string"         = String
  EL "is_held"               = Bool
  EL "is_shown_in_index"     = Bool
  EL "jpeg_file_size"        = Integer
  EL "jpeg_height"           = Integer
  EL "jpeg_url"              = String
  EL "jpeg_width"            = Integer
  EL "sample_file_size"      = Integer
  EL "actual_preview_height" = Integer
  EL "actual_preview_width"  = Integer
  EL "file_size"             = Integer


newtype VAL s = VAL { unVAL ∷ EL s }

-- Seems evil, requires UndecidableInstances…
deriving instance Show (EL x) ⇒ Show (VAL x)
deriving instance Eq (EL x) ⇒ Eq (VAL x)
deriving instance Ord (EL x) ⇒ Ord (VAL x)

-- | 'R' wrapped in a 'Parse'.
type PR a = Parse (Rec VAL a)

-- * Commonly used fields

height ∷ Proxy "height"
height = Proxy

score ∷ Proxy "score"
score = Proxy

file_url ∷ Proxy "file_url"
file_url = Proxy

parent_id ∷ Proxy "parent_id"
parent_id = Proxy

sample_url ∷ Proxy "sample_url"
sample_url = Proxy

sample_width ∷ Proxy "sample_width"
sample_width = Proxy

sample_height ∷ Proxy "sample_height"
sample_height = Proxy

preview_url ∷ Proxy "preview_url"
preview_url = Proxy

rating ∷ Proxy "rating"
rating = Proxy

tags ∷ Proxy "tags"
tags = Proxy

id ∷ Proxy "id"
id = Proxy

width ∷ Proxy "width"
width = Proxy

change ∷ Proxy "change"
change = Proxy

md5 ∷ Proxy "md5"
md5 = Proxy

creator_id ∷ Proxy "creator_id"
creator_id = Proxy

has_children ∷ Proxy "has_children"
has_children = Proxy

created_at ∷ Proxy "created_at"
created_at = Proxy

status ∷ Proxy "status"
status = Proxy

source ∷ Proxy "source"
source = Proxy

has_notes ∷ Proxy "has_notes"
has_notes = Proxy

has_comments ∷ Proxy "has_comments"
has_comments = Proxy

preview_width ∷ Proxy "preview_width"
preview_width = Proxy

preview_height ∷ Proxy "preview_height"
preview_height = Proxy

author ∷ Proxy "author"
author = Proxy

frames ∷ Proxy "frames"
frames = Proxy

frames_pending ∷ Proxy "frames_pending"
frames_pending = Proxy

frames_pending_string ∷ Proxy "frames_pending_string"
frames_pending_string = Proxy

frames_string ∷ Proxy "frames_string"
frames_string = Proxy

is_held ∷ Proxy "is_held"
is_held = Proxy

is_shown_in_index ∷ Proxy "is_shown_in_index"
is_shown_in_index = Proxy

jpeg_file_size ∷ Proxy "jpeg_file_size"
jpeg_file_size = Proxy

jpeg_height ∷ Proxy "jpeg_height"
jpeg_height = Proxy

jpeg_url ∷ Proxy "jpeg_url"
jpeg_url = Proxy

jpeg_width ∷ Proxy "jpeg_width"
jpeg_width = Proxy

sample_file_size ∷ Proxy "sample_file_size"
sample_file_size = Proxy

actual_preview_height ∷ Proxy "actual_preview_height"
actual_preview_height = Proxy

actual_preview_width ∷ Proxy "actual_preview_width"
actual_preview_width = Proxy

file_size ∷ Proxy "file_size"
file_size = Proxy
