{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  HBooru.Types
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module definining types used by the library.
module HBooru.Types where

import Prelude hiding (id)
import Control.Applicative
import Data.Vinyl
import Data.Vinyl.Unicode
import Data.Vinyl.Validation

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

-- | Class specifying a parser that can fetch posts. A post usually consists of
-- links to the image, samples, and some meta-data. See
-- 'HBooru.Parsers.GenericBooru.GenericPost' for the kind of thing we usually
-- get out. The reason for this class is that sometimes we might get different
-- information based on the 'DataFormat' we use so we use type families to
-- denote this rather than forcing the library user to make do with our best
-- guess on what goes into the post. It also allows us to use different post
-- types for sites that provide different information.
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
  parseCount ∷ CoerceResponse r r' ⇒ s → r' → Integer

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
  hardLimit ∷ s → Limit

-- | Describes a site for a parser. The reason why this isn't a simple data type
-- is to allow us to write additional parsers in the future without modifying
-- this library if we wish to do so.
class Site s where

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

-- | Class representing a best-case scenario post. We use this
-- to convert between different posts for each site while providing
-- uniform access. The methods are just the attributes of posts seen
-- on Gelbooru-like sites.
class Post a where
  height ∷ a → Integer
  score ∷ a → Integer
  file_url ∷ a → String
  parent_id ∷ a → Maybe Integer
  sample_url ∷ a → String
  sample_width ∷ a → Integer
  sample_height ∷ a → Integer
  preview_url ∷ a → String
  rating ∷ a → Rating
  tags ∷ a → [String]
  id ∷ a → Integer
  width ∷ a → Integer
  change ∷ a → String
  md5 ∷ a → String
  creator_id ∷ a → Integer
  has_children ∷ a → Maybe Bool
  created_at ∷ a → String
  status ∷ a → String
  source ∷ a → String
  has_notes ∷ a → Maybe Bool
  has_comments ∷ a → Maybe Bool
  preview_width ∷ a → Integer
  preview_height ∷ a → Integer
  betweenPosts ∷ Post b ⇒ a → PostConstructor b → b
  betweenPosts g c =
    c (height g) (score g) (file_url g) (parent_id g) (sample_url g)
      (sample_width g) (sample_height g) (preview_url g) (rating g)
      (tags g) (id g) (width g) (change g) (md5 g) (creator_id g)
      (has_children g) (created_at g) (status g) (source g) (has_notes g)
      (has_comments g) (preview_width g) (preview_height g)

type GenericFields =
  [ "height" ::: Integer
  , "score" ::: Integer
  , "file_url" ::: String
  , "parent_id" ::: Maybe Integer
  , "sample_url" ::: String
  , "sample_width" ::: Integer
  , "sample_height" ::: Integer
  , "preview_url" ::: String
  , "rating" ::: Rating
  , "tags" ::: [String]
  , "id" ::: Integer
  , "width" ::: Integer
  , "change" ::: String
  , "md5" ::: String
  , "creator_id" ::: Integer
  , "has_children" ::: Maybe Bool
  , "created_at" ::: Integer
  , "status" ::: String
  , "source" ::: String
  , "has_notes" ::: Maybe Bool
  , "has_comments" ::: Maybe Bool
  , "preview_width" ::: Integer
  , "preview_height" ::: Integer
  ]


height'' ∷ "height" ::: Integer
height'' = Field

score'' ∷ "score" ::: Integer
score'' = Field

file_url'' ∷ "file_url" ::: String
file_url'' = Field

parent_id'' ∷ "parent_id" ::: Maybe Integer
parent_id'' = Field

sample_url'' ∷ "sample_url" ::: String
sample_url'' = Field

sample_width'' ∷ "sample_width" ::: Integer
sample_width'' = Field

sample_height'' ∷ "sample_height" ::: Integer
sample_height'' = Field

preview_url'' ∷ "preview_url" ::: String
preview_url'' = Field

rating'' ∷ "rating" ::: Rating
rating'' = Field

tags'' ∷ "tags" ::: [String]
tags'' = Field

id'' ∷ "id" ::: Integer
id'' = Field

width'' ∷ "width" ::: Integer
width'' = Field

change'' ∷ "change" ::: String
change'' = Field

md5'' ∷ "md5" ::: String
md5'' = Field

creator_id'' ∷ "creator_id" ::: Integer
creator_id'' = Field

has_children'' ∷ "has_children" ::: Bool
has_children'' = Field

created_at'' ∷ "created_at" ::: String
created_at'' = Field

status'' ∷ "status" ::: String
status'' = Field

source'' ∷ "source" ::: String
source'' = Field

has_notes'' ∷ "has_notes" ::: Maybe Bool
has_notes'' = Field

has_comments'' ∷ "has_comments" ::: Maybe Bool
has_comments'' = Field

preview_width'' ∷ "preview_width" ::: Integer
preview_width'' = Field

preview_height'' ∷ "preview_height" ::: Integer
preview_height'' = Field

author'' ∷ "author" ::: String
author'' = Field

frames'' ∷ "frames" ::: String
frames'' = Field

frames_pending'' ∷ "frames_pending" ::: String
frames_pending'' = Field

frames_pending_string'' ∷ "frames_pending_string" ::: String
frames_pending_string'' = Field

frames_string'' ∷ "frames_string" ::: String
frames_string'' = Field

is_held'' ∷ "is_held" ::: Bool
is_held'' = Field

is_shown_in_index'' ∷ "is_shown_in_index" ::: Bool
is_shown_in_index'' = Field

jpeg_file_size'' ∷ "jpeg_file_size" ::: String
jpeg_file_size'' = Field

jpeg_height'' ∷ "jpeg_height" ::: Integer
jpeg_height'' = Field

jpeg_url'' ∷ "jpeg_url" ::: String
jpeg_url'' = Field

jpeg_width'' ∷ "jpeg_width" ::: Integer
jpeg_width'' = Field

sample_file_size'' ∷ "sample_file_size" ::: String
sample_file_size'' = Field

actual_preview_height'' ∷ "actual_preview_height" ::: Integer
actual_preview_height'' = Field

actual_preview_width'' ∷ "actual_preview_width" ::: Integer
actual_preview_width'' = Field

file_size'' ∷ "file_size" ::: String
file_size'' = Field

type GenericPost' = PlainRec GenericFields

-- | A cludge for use with 'betweenPosts'
type PostConstructor b =
  Integer -> Integer -> String -> Maybe Integer -> String -> Integer -> Integer
  -> String -> Rating -> [String] -> Integer -> Integer -> String -> String
  -> Integer -> Maybe Bool -> String -> String -> String -> Maybe Bool
  -> Maybe Bool -> Integer -> Integer -> b
