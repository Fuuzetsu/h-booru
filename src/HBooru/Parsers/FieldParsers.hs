{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

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
import Data.Maybe
import Data.Vinyl
import HBooru.Types
import Prelude hiding (id)
import Text.Read (readMaybe)
import Text.XML.HXT.Core hiding (mkName, (<+>))

-- * Individual attribute parsers

-- | Parser arrow for a "height" XML attribute.
heightA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["height" ::: Integer])
heightA = (height =:) . read <$> getAttrValue "height"

-- | Parser arrow for a "score" XML attribute.
scoreA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
         cat XmlTree (PlainRec '["score" ::: Integer])
scoreA = (score =:) . read <$> getAttrValue "score"

-- | Parser arrow for a "file_url" XML attribute.
file_urlA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
            cat XmlTree (PlainRec '["file_url" ::: String])
file_urlA = (file_url =:) <$> getAttrValue "file_url"

-- | Parser arrow for a "parent_id" XML attribute.
parent_idA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
             cat XmlTree (PlainRec '["parent_id" ::: Maybe Integer])
parent_idA = (parent_id =:) . readMaybe <$> getAttrValue "parent_id"

-- | Parser arrow for a "sample_url" XML attribute.
sample_urlA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
              cat XmlTree (PlainRec '["sample_url" ::: String])
sample_urlA = (sample_url =:) <$> getAttrValue "sample_url"

-- | Parser arrow for a "sample_width" XML attribute.
sample_widthA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                cat XmlTree (PlainRec '["sample_width" ::: Integer])
sample_widthA = (sample_width =:) . read <$> getAttrValue "sample_width"

-- | Parser arrow for a "sample_height" XML attribute.
sample_heightA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                 cat XmlTree (PlainRec '["sample_height" ::: Integer])
sample_heightA = (sample_height =:) . read <$> getAttrValue "sample_height"

-- | Parser arrow for a "preview_url" XML attribute.
preview_urlA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
               cat XmlTree (PlainRec '["preview_url" ::: String])
preview_urlA = (preview_url =:) <$> getAttrValue "preview_url"

-- | Parser arrow for a "rating" XML attribute.
ratingA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["rating" ::: Rating])
ratingA = (rating =:) . parseRating <$> getAttrValue "rating"

-- | Parser arrow for a "tags" XML attribute.
tagsA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
        cat XmlTree (PlainRec '["tags" ::: [String]])
tagsA = (tags =:) . parseTags <$> getAttrValue "tags"

-- | Parser arrow for a "id" XML attribute.
idA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
      cat XmlTree (PlainRec '["id" ::: Integer])
idA = (id =:) . read <$> getAttrValue "id"

-- | Parser arrow for a "width" XML attribute.
widthA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
         cat XmlTree (PlainRec '["width" ::: Integer])
widthA = (width =:) . read <$> getAttrValue "width"

-- | Parser arrow for a "change" XML attribute.
changeA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["change" ::: String])
changeA = (change =:) <$> getAttrValue "change"

-- | Parser arrow for a "md5" XML attribute.
md5A ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
       cat XmlTree (PlainRec '["md5" ::: String])
md5A = (md5 =:) <$> getAttrValue "md5"

-- | Parser arrow for a "creator_id" XML attribute.
creator_idA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
              cat XmlTree (PlainRec '["creator_id" ::: Integer])
creator_idA = (creator_id =:) . read <$> getAttrValue "creator_id"

-- | Parser arrow for a "has_children" XML attribute.
has_childrenA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                cat XmlTree (PlainRec '["has_children" ::: Bool])
has_childrenA = (has_children =:) . fromMaybe (error "has_childrenA")
                . parseBool <$> getAttrValue "has_children"

-- | Parser arrow for a "created_at" XML attribute.
created_atA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
              cat XmlTree (PlainRec '["created_at" ::: String])
created_atA = (created_at =:) <$> getAttrValue "created_at"

-- | Parser arrow for a "status" XML attribute.
statusA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["status" ::: String])
statusA = (status =:) <$> getAttrValue "status"

-- | Parser arrow for a "source" XML attribute.
sourceA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["source" ::: String])
sourceA = (source =:) <$> getAttrValue "source"

-- | Parser arrow for a "has_notes" XML attribute.
has_notesA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
             cat XmlTree (PlainRec '["has_notes" ::: Maybe Bool])
has_notesA = (has_notes =:) . parseBool <$> getAttrValue "has_notes"

-- | Parser arrow for a "has_comments" XML attribute.
has_commentsA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                cat XmlTree (PlainRec '["has_comments" ::: Maybe Bool])
has_commentsA = (has_comments =:) . parseBool <$> getAttrValue "has_comments"

-- | Parser arrow for a "preview_width" XML attribute.
preview_widthA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                 cat XmlTree (PlainRec '["preview_width" ::: Integer])
preview_widthA = (preview_width =:) . read <$> getAttrValue "preview_width"

-- | Parser arrow for a "preview_height" XML attribute.
preview_heightA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                  cat XmlTree (PlainRec '["preview_height" ::: Integer])
preview_heightA = (preview_height =:) . read <$> getAttrValue "preview_height"

-- | Parser arrow for a "author" XML attribute.
authorA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["author" ::: String])
authorA = (author =:) <$> getAttrValue "author"

-- | Parser arrow for a "actual_preview_height" XML attribute.
actual_preview_heightA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                         cat XmlTree
                             (PlainRec '["actual_preview_height" ::: Integer])
actual_preview_heightA = (actual_preview_height =:) . read
                         <$> getAttrValue "actual_preview_height"

-- | Parser arrow for a "actual_preview_width" XML attribute.
actual_preview_widthA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                         cat XmlTree
                             (PlainRec '["actual_preview_width" ::: Integer])
actual_preview_widthA = (actual_preview_width =:) . read
                        <$> getAttrValue "actual_preview_width"

-- | Parser arrow for a "frames" XML attribute.
framesA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
          cat XmlTree (PlainRec '["frames" ::: String])
framesA = (frames =:) <$> getAttrValue "frames"

-- | Parser arrow for a "frames_pending" XML attribute.
frames_pendingA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                  cat XmlTree (PlainRec '["frames_pending" ::: String])
frames_pendingA = (frames_pending =:) <$> getAttrValue "frames_pending"

-- | Parser arrow for a "frames_pending_string" XML attribute.
frames_pending_stringA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                         cat XmlTree
                             (PlainRec '["frames_pending_string" ::: String])
frames_pending_stringA = (frames_pending_string =:)
                         <$> getAttrValue "frames_pending_string"

-- | Parser arrow for a "frames_string" XML attribute.
frames_stringA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                 cat XmlTree (PlainRec '["frames_string" ::: String])
frames_stringA = (frames_string =:) <$> getAttrValue "frames_string"

-- | Parser arrow for a "is_held" XML attribute.
is_heldA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
           cat XmlTree (PlainRec '["is_held" ::: Bool])
is_heldA = (is_held =:) . fromMaybe (error "is_heldA") . parseBool
           <$> getAttrValue "is_held"

-- | Parser arrow for a "is_shown_in_index" XML attribute.
is_shown_in_indexA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                     cat XmlTree (PlainRec '["is_shown_in_index" ::: Bool])
is_shown_in_indexA = (is_shown_in_index =:)
                     . fromMaybe (error "is_show_in_indexA") . parseBool
                     <$> getAttrValue "is_shown_in_index"

-- | Parser arrow for a "jpeg_file_size" XML attribute.
jpeg_file_sizeA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                  cat XmlTree (PlainRec '["jpeg_file_size" ::: String])
jpeg_file_sizeA = (jpeg_file_size =:) <$> getAttrValue "jpeg_file_size"

-- | Parser arrow for a "jpeg_height" XML attribute.
jpeg_heightA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
               cat XmlTree (PlainRec '["jpeg_height" ::: Integer])
jpeg_heightA = (jpeg_height =:) . read <$> getAttrValue "jpeg_height"

-- | Parser arrow for a "jpeg_url" XML attribute.
jpeg_urlA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
            cat XmlTree (PlainRec '["jpeg_url" ::: String])
jpeg_urlA = (jpeg_url =:) <$> getAttrValue "jpeg_url"

-- | Parser arrow for a "jpeg_width" XML attribute.
jpeg_widthA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
              cat XmlTree (PlainRec '["jpeg_width" ::: Integer])
jpeg_widthA = (jpeg_width =:) . read <$> getAttrValue "jpeg_width"

-- | Parser arrow for a "sample_file_size" XML attribute.
sample_file_sizeA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                    cat XmlTree (PlainRec '["sample_file_size" ::: String])
sample_file_sizeA = (sample_file_size =:) <$> getAttrValue "sample_file_size"

-- | Parser arrow for a "file_size" XML attribute.
file_sizeA ∷ (Functor (cat XmlTree), ArrowXml cat) ⇒
                    cat XmlTree (PlainRec '["file_size" ::: String])
file_sizeA = (file_size =:) <$> getAttrValue "file_size"


-- * Parsing helpers

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
