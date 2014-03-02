{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Module      :  HBooru.Parsers.FieldParsers
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- A collection of arrow parsers for known fields. We can use these
-- (and write new ones) by composing these parsers together for each site we
-- want to parse. As we carry the field information with us, this can later be
-- used when trying to extract the parsed information from the sites into a
-- homogenous list.
module HBooru.Parsers.FieldParsers where

import Control.Applicative
import Data.Maybe
import Data.Vinyl
import HBooru.Types
import Prelude hiding (id)
import Text.Read (readMaybe)
import Text.XML.HXT.Core hiding (mkName, (<+>))

heightA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["height" ::: Integer])
heightA = (height'' =:) . read <$> getAttrValue "height"

scoreA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
         cat XmlTree (PlainRec '["score" ::: Integer])
scoreA = (score'' =:) . read <$> getAttrValue "score"

file_urlA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
            cat XmlTree (PlainRec '["file_url" ::: String])
file_urlA = (file_url'' =:) <$> getAttrValue "file_url"

parent_idA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
             cat XmlTree (PlainRec '["parent_id" ::: Maybe Integer])
parent_idA = (parent_id'' =:) . readMaybe <$> getAttrValue "parent_id"

sample_urlA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
              cat XmlTree (PlainRec '["sample_url" ::: String])
sample_urlA = (sample_url'' =:) <$> getAttrValue "sample_url"

sample_widthA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                cat XmlTree (PlainRec '["sample_width" ::: Integer])
sample_widthA = (sample_width'' =:) . read <$> getAttrValue "sample_width"

sample_heightA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                 cat XmlTree (PlainRec '["sample_height" ::: Integer])
sample_heightA = (sample_height'' =:) . read <$> getAttrValue "sample_height"

preview_urlA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
               cat XmlTree (PlainRec '["preview_url" ::: String])
preview_urlA = (preview_url'' =:) <$> getAttrValue "preview_url"

ratingA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["rating" ::: Rating])
ratingA = (rating'' =:) . parseRating <$> getAttrValue "rating"

tagsA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
        cat XmlTree (PlainRec '["tags" ::: [String]])
tagsA = (tags'' =:) . parseTags <$> getAttrValue "tags"

idA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
      cat XmlTree (PlainRec '["id" ::: Integer])
idA = (id'' =:) . read <$> getAttrValue "id"

widthA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
         cat XmlTree (PlainRec '["width" ::: Integer])
widthA = (width'' =:) . read <$> getAttrValue "width"

changeA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["change" ::: String])
changeA = (change'' =:) <$> getAttrValue "change"

md5A ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
       cat XmlTree (PlainRec '["md5" ::: String])
md5A = (md5'' =:) <$> getAttrValue "md5"

creator_idA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
              cat XmlTree (PlainRec '["creator_id" ::: Integer])
creator_idA = (creator_id'' =:) . read <$> getAttrValue "creator_id"

has_childrenA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                cat XmlTree (PlainRec '["has_children" ::: Bool])
has_childrenA = (has_children'' =:) . fromMaybe (error "has_childrenA")
                . parseBool <$> getAttrValue "has_children"

created_atA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
              cat XmlTree (PlainRec '["created_at" ::: String])
created_atA = (created_at'' =:) <$> getAttrValue "created_at"

statusA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["status" ::: String])
statusA = (status'' =:) <$> getAttrValue "status"

sourceA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["source" ::: String])
sourceA = (source'' =:) <$> getAttrValue "source"

has_notesA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
             cat XmlTree (PlainRec '["has_notes" ::: Maybe Bool])
has_notesA = (has_notes'' =:) . parseBool <$> getAttrValue "has_notes"

has_commentsA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                cat XmlTree (PlainRec '["has_comments" ::: Maybe Bool])
has_commentsA = (has_comments'' =:) . parseBool <$> getAttrValue "has_comments"

preview_widthA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                 cat XmlTree (PlainRec '["preview_width" ::: Integer])
preview_widthA = (preview_width'' =:) . read <$> getAttrValue "preview_width"

preview_heightA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                  cat XmlTree (PlainRec '["preview_height" ::: Integer])
preview_heightA = (preview_height'' =:) . read <$> getAttrValue "preview_height"

authorA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["author" ::: String])
authorA = (author'' =:) <$> getAttrValue "author"

actual_preview_heightA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                         cat XmlTree
                             (PlainRec '["actual_preview_height" ::: Integer])
actual_preview_heightA = (actual_preview_height'' =:) . read
                         <$> getAttrValue "actual_preview_height"

actual_preview_widthA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                         cat XmlTree
                             (PlainRec '["actual_preview_width" ::: Integer])
actual_preview_widthA = (actual_preview_width'' =:) . read
                        <$> getAttrValue "actual_preview_width"

framesA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["frames" ::: String])
framesA = (frames'' =:) <$> getAttrValue "frames"

frames_pendingA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                  cat XmlTree (PlainRec '["frames_pending" ::: String])
frames_pendingA = (frames_pending'' =:) <$> getAttrValue "frames_pending"

frames_pending_stringA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                         cat XmlTree
                             (PlainRec '["frames_pending_string" ::: String])
frames_pending_stringA = (frames_pending_string'' =:)
                         <$> getAttrValue "frames_pending_string"

frames_stringA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                 cat XmlTree (PlainRec '["frames_string" ::: String])
frames_stringA = (frames_string'' =:) <$> getAttrValue "frames_string"

is_heldA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
           cat XmlTree (PlainRec '["is_held" ::: Bool])
is_heldA = (is_held'' =:) . fromMaybe (error "is_heldA") . parseBool
           <$> getAttrValue "is_held"

is_shown_in_indexA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                     cat XmlTree (PlainRec '["is_shown_in_index" ::: Bool])
is_shown_in_indexA = (is_shown_in_index'' =:)
                     . fromMaybe (error "is_show_in_indexA") . parseBool
                     <$> getAttrValue "is_shown_in_index"

jpeg_file_sizeA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                  cat XmlTree (PlainRec '["jpeg_file_size" ::: String])
jpeg_file_sizeA = (jpeg_file_size'' =:) <$> getAttrValue "jpeg_file_size"

jpeg_heightA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
               cat XmlTree (PlainRec '["jpeg_height" ::: Integer])
jpeg_heightA = (jpeg_height'' =:) . read <$> getAttrValue "jpeg_height"

jpeg_urlA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
            cat XmlTree (PlainRec '["jpeg_url" ::: String])
jpeg_urlA = (jpeg_url'' =:) <$> getAttrValue "jpeg_url"

jpeg_widthA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
              cat XmlTree (PlainRec '["jpeg_width" ::: Integer])
jpeg_widthA = (jpeg_width'' =:) . read <$> getAttrValue "jpeg_width"

sample_file_sizeA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                    cat XmlTree (PlainRec '["sample_file_size" ::: String])
sample_file_sizeA = (sample_file_size'' =:) <$> getAttrValue "sample_file_size"

file_sizeA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                    cat XmlTree (PlainRec '["file_size" ::: String])
file_sizeA = (file_size'' =:) <$> getAttrValue "file_size"


-- | Parses a string returned from a Gelbooru-like site into
-- one of the commonly used 'Rating's. Note that this is a partial function
-- so you should make sure that the site in question only ever returns the
-- values in a format specified in the function
parseRating :: String -> Rating
parseRating "e" = Explicit
parseRating "s" = HBooru.Types.Safe
parseRating "q" = Questionable
parseRating _ = error "Failed to parse rating"

-- | Splits returned tag string into separate 'Tag's. For Gelbooru-like
-- sites, this is just the question of splitting on whitespace.
parseTags :: String -> [Tag]
parseTags = words

-- | Reads a lowercase 'Bool' string representation into its Haskell type. If we
-- can't parse the boolean, return 'Nothing'.
parseBool :: String -> Maybe Bool
parseBool "false" = Just False
parseBool "true" = Just True
parseBool _ = Nothing


infixr 5 <:+>
-- | A little helper that lifts '<+>' into 'Arrow' which allows us to
-- compose parsers returning records very easily.
(<:+>) ∷ Arrow cat ⇒ cat b (Rec as f) → cat b (Rec bs f)
       → cat b (Rec (as ++ bs) f)
x <:+> y = arr (uncurry (<+>)) <<< x &&& y
