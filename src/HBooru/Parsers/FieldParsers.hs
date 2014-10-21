{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  HBooru.Parsers.FieldParsers
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- A collection of arrow parsers for known fields. We can use these
-- (and write new ones) by composing these parsers together for each site we
-- want to parse. As we carry the field information with us, this can later be
-- used when trying to extract the parsed information from the sites into a
-- homogenous list. Currently this moduel only deals with parsing out XML
-- attributes.
module HBooru.Parsers.FieldParsers where

import Control.Applicative
import Control.Exception.Base (Exception)
import Control.Monad
import Control.Monad.Error
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.Vinyl
import HBooru.Types
import Prelude
import Text.Read (readMaybe)
import Text.XML.HXT.Core hiding (mkName, (<+>))

import Data.Vinyl.TyFun

newtype E = E String deriving (Show, Eq, Typeable)
instance Exception E where

-- | Alias for the common constraint blob
type ParseArrow cat = (Functor (cat XmlTree), ArrowXml cat)

-- | Alias for named fields
type Field cat s = cat XmlTree (Parse (R '[s]))

-- | Helper that provides better error messages when a 'read' fails.
readAttr ∷ (ArrowXml cat, Read (App ElF s), Functor (cat XmlTree)) ⇒ String
         → sing s → Field cat s
readAttr s f = readAttrWith s f readMaybe

readAttrWith ∷ (ArrowXml cat, Functor (cat XmlTree)) ⇒ String → sing s
             → (String → Maybe (App ElF s)) → Field cat s
readAttrWith s f r = readCustom s f (\x → handler x $ r x)
  where
    handler inp Nothing = throwError . PF $ mconcat [s, ": '", inp, "'"]
    handler _ (Just x) = return x

readCustom ∷ (ArrowXml cat, Functor (cat XmlTree)) ⇒ String
           → sing s → (String → Parse (App ElF s)) → Field cat s
readCustom s f h = fmap (f =:) . h <$> getAttrValue s

readNormalAttr ∷ (ArrowXml cat, (App ElF s) ~ String, Functor (cat XmlTree))
               ⇒ String → sing s → Field cat s
readNormalAttr s f = return . (f =:) <$> getAttrValue s

-- * Individual attribute parsers

-- | Parser arrow for a "height" XML attribute.
heightA ∷ ParseArrow cat ⇒ Field cat "height"
heightA = readAttr "height" height

-- | Parser arrow for a "score" XML attribute.
scoreA ∷ ParseArrow cat ⇒ Field cat "score"
scoreA = readAttr "score" score

-- | Parser arrow for a "file_url" XML attribute.
file_urlA ∷ ParseArrow cat ⇒ Field cat "file_url"
file_urlA = readNormalAttr "file_url" file_url

-- | Parser arrow for a "parent_id" XML attribute.
parent_idA ∷ ParseArrow cat ⇒ Field cat "parent_id"
parent_idA = readCustom "parent_id" parent_id handler
  where
    handler = return . readMaybe

-- | Parser arrow for a "sample_url" XML attribute.
sample_urlA ∷ ParseArrow cat ⇒ Field cat "sample_url"
sample_urlA = readNormalAttr "sample_url" sample_url

-- | Parser arrow for a "sample_width" XML attribute.
sample_widthA ∷ ParseArrow cat ⇒ Field cat "sample_width"
sample_widthA = readAttr "sample_width" sample_width

-- | Parser arrow for a "sample_height" XML attribute.
sample_heightA ∷ ParseArrow cat ⇒ Field cat "sample_height"
sample_heightA = readAttr "sample_height" sample_height

-- | Parser arrow for a "preview_url" XML attribute.
preview_urlA ∷ ParseArrow cat ⇒ Field cat "preview_url"
preview_urlA = readNormalAttr "preview_url" preview_url

-- | Parser arrow for a "rating" XML attribute.
ratingA ∷ ParseArrow cat ⇒ Field cat "rating"
ratingA = readAttrWith "rating" rating parseRating

-- | Parser arrow for a "tags" XML attribute.
tagsA ∷ ParseArrow cat ⇒ Field cat "tags"
tagsA = readCustom "tags" tags (return . parseTags)

-- | Parser arrow for a "id" XML attribute.
idA ∷ ParseArrow cat ⇒ Field cat "id"
idA = readAttr "id" HBooru.Types.id

-- | Parser arrow for a "width" XML attribute.
widthA ∷ ParseArrow cat ⇒ Field cat "width"
widthA = readAttr "width" width

-- | Parser arrow for a "change" XML attribute.
changeA ∷ ParseArrow cat ⇒ Field cat "change"
changeA = readAttr "change" change

-- | Parser arrow for a "md5" XML attribute.
md5A ∷ ParseArrow cat ⇒ Field cat "md5"
md5A = readNormalAttr "md5" md5

-- | Parser arrow for a "creator_id" XML attribute.
creator_idA ∷ ParseArrow cat ⇒ Field cat "creator_id"
creator_idA = readAttr "creator_id" creator_id

-- | Parser arrow for a "has_children" XML attribute.
has_childrenA ∷ ParseArrow cat ⇒ Field cat "has_children"
has_childrenA = readAttrWith "has_children" has_children parseBool

-- | Parser arrow for a "created_at" XML attribute.
created_atA ∷ ParseArrow cat ⇒ Field cat "created_at"
created_atA = readNormalAttr "created_at" created_at

-- | Parser arrow for a "status" XML attribute.
statusA ∷ ParseArrow cat ⇒ Field cat "status"
statusA = readNormalAttr "status" status

-- | Parser arrow for a "source" XML attribute.
sourceA ∷ ParseArrow cat ⇒ Field cat "source"
sourceA = readNormalAttr "source" source

-- | Parser arrow for a "has_notes" XML attribute.
has_notesA ∷ ParseArrow cat ⇒ Field cat "has_notes"
has_notesA = readAttrWith "has_notes" has_notes (return . parseBool)

-- | Parser arrow for a "has_comments" XML attribute.
has_commentsA ∷ ParseArrow cat ⇒ Field cat "has_comments"
has_commentsA = readAttrWith "has_comments" has_comments (return . parseBool)

-- | Parser arrow for a "preview_width" XML attribute.
preview_widthA ∷ ParseArrow cat ⇒ Field cat "preview_width"
preview_widthA = readAttr "preview_width" preview_width

-- | Parser arrow for a "preview_height" XML attribute.
preview_heightA ∷ ParseArrow cat ⇒ Field cat "preview_height"
preview_heightA = readAttr "preview_height" preview_height

-- | Parser arrow for a "author" XML attribute.
authorA ∷ ParseArrow cat ⇒ Field cat "author"
authorA = readNormalAttr "author" author

-- | Parser arrow for a "actual_preview_height" XML attribute.
actual_preview_heightA ∷ ParseArrow cat ⇒ Field cat "actual_preview_height"
actual_preview_heightA = readAttr "actual_preview_height" actual_preview_height

-- | Parser arrow for a "actual_preview_width" XML attribute.
actual_preview_widthA ∷ ParseArrow cat ⇒ Field cat "actual_preview_width"
actual_preview_widthA = readAttr "actual_preview_width" actual_preview_width

-- | Parser arrow for a "frames" XML attribute.
framesA ∷ ParseArrow cat ⇒ Field cat "frames"
framesA = readNormalAttr "frames" frames

-- | Parser arrow for a "frames_pending" XML attribute.
frames_pendingA ∷ ParseArrow cat ⇒ Field cat "frames_pending"
frames_pendingA = readNormalAttr "frames_pending" frames_pending

-- | Parser arrow for a "frames_pending_string" XML attribute.
frames_pending_stringA ∷ ParseArrow cat ⇒ Field cat "frames_pending_string"
frames_pending_stringA =
  readNormalAttr "frames_pending_string" frames_pending_string

-- | Parser arrow for a "frames_string" XML attribute.
frames_stringA ∷ ParseArrow cat ⇒ Field cat "frames_string"
frames_stringA = readNormalAttr "frames_string" frames_string

-- | Parser arrow for a "is_held" XML attribute.
is_heldA ∷ ParseArrow cat ⇒ Field cat "is_held"
is_heldA = readAttrWith "is_held" is_held parseBool

-- | Parser arrow for a "is_shown_in_index" XML attribute.
is_shown_in_indexA ∷ ParseArrow cat ⇒ Field cat "is_shown_in_index"
is_shown_in_indexA =
  readAttrWith "is_shown_in_index" is_shown_in_index parseBool

-- | Parser arrow for a "jpeg_file_size" XML attribute.
jpeg_file_sizeA ∷ ParseArrow cat ⇒ Field cat "jpeg_file_size"
jpeg_file_sizeA = readAttr "jpeg_file_size" jpeg_file_size

-- | Parser arrow for a "jpeg_height" XML attribute.
jpeg_heightA ∷ ParseArrow cat ⇒ Field cat "jpeg_height"
jpeg_heightA = readAttr "jpeg_height" jpeg_height

-- | Parser arrow for a "jpeg_url" XML attribute.
jpeg_urlA ∷ ParseArrow cat ⇒ Field cat "jpeg_url"
jpeg_urlA = readNormalAttr "jpeg_url" jpeg_url

-- | Parser arrow for a "jpeg_width" XML attribute.
jpeg_widthA ∷ ParseArrow cat ⇒ Field cat "jpeg_width"
jpeg_widthA = readAttr "jpeg_width" jpeg_width

-- | Parser arrow for a "sample_file_size" XML attribute.
sample_file_sizeA ∷ ParseArrow cat ⇒ Field cat "sample_file_size"
sample_file_sizeA = readAttr "sample_file_size" sample_file_size

-- | Parser arrow for a "file_size" XML attribute.
file_sizeA ∷ ParseArrow cat ⇒ Field cat "file_size"
file_sizeA = readAttr "file_size" file_size

-- * Parsing helpers

-- | Parses a string returned from a Gelbooru-like site into
-- one of the commonly used 'Rating's. Note that this is a partial function
-- so you should make sure that the site in question only ever returns the
-- values in a format specified in the function
parseRating ∷ String → Maybe Rating
parseRating "e" = Just Explicit
parseRating "s" = Just HBooru.Types.Safe
parseRating "q" = Just Questionable
parseRating _ = Nothing

-- | Splits returned tag string into separate 'Tag's. For Gelbooru-like
-- sites, this is just the question of splitting on whitespace.
parseTags ∷ String → [Tag]
parseTags = words

-- | Reads a lowercase 'Bool' string representation into its Haskell type. If we
-- can't parse the boolean, return 'Nothing'.
parseBool ∷ String → Maybe Bool
parseBool "false" = Just False
parseBool "true" = Just True
parseBool _ = Nothing

infixr 5 <:+>
-- | A little helper that lifts '<+>' into 'Arrow' which allows us to
-- compose parsers returning records very easily.
(<:+>) ∷ Arrow cat ⇒ cat b (Parse (R as))
       → cat b (Parse (R bs))
       → cat b (Parse (R (as ++ bs)))
x <:+> y = arr (uncurry pnd) <<< x &&& y
  where
    pnd ∷ Parse (R a) → Parse (R b) → Parse (R (a ++ b))
    pnd (Left x') _ = Left x'
    pnd _ (Left y') = Left y'
    pnd (Right x') (Right y') = Right (x' <+> y')
