{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  HBooru.Parsers.GenericBooru.TH
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- | Helper module for "HBooru.Parsers.GenericBooru" due to Template Haskell
-- limitation of being unable to splice inside of the same module of
-- definitions.
module HBooru.Parsers.GenericBooru.TH where

import HBooru.Types
import Language.Haskell.TH

-- | Template Haskell function which is able to generate 'GenericPost'-alike
-- type declarations for cases where we want to use this format but need a
-- different data type. It can be used by using @TemplateHaskell@ extension and
-- calling @$(makePost ('mkName' \"YourTypeName\"))@ at the top level. Hopefully
-- a temporary measure until the author thinks of a better way to provide
-- generic Gelbooru-like post parsing while casting out to different data types
-- that's OK to write.
makePost :: Name -> Q [Dec]
makePost n =
  fmap (:[]) $ dataD (cxt []) n []
  [ recC n
    [ varStrictType (mkName "height") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "score") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "file_url") $ strictType notStrict [t| String |]
    , varStrictType (mkName "parent_id") $ strictType notStrict
                                             [t| Maybe Integer |]
    , varStrictType (mkName "sample_url") $ strictType notStrict [t| String |]
    , varStrictType (mkName "sample_width") $ strictType notStrict
                                                [t| Integer |]
    , varStrictType (mkName "sample_height") $ strictType notStrict
                                                 [t| Integer |]
    , varStrictType (mkName "preview_url") $ strictType notStrict [t| String |]
    , varStrictType (mkName "rating") $ strictType notStrict [t| Rating |]
    , varStrictType (mkName "tags") $ strictType notStrict [t| [String] |]
    , varStrictType (mkName "id") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "width") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "change") $ strictType notStrict [t| String |]
    , varStrictType (mkName "md5") $ strictType notStrict [t| String |]
    , varStrictType (mkName "creator_id") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "has_children") $ strictType notStrict
                                                [t| Maybe Bool |]
    , varStrictType (mkName "created_at") $ strictType notStrict [t| String |]
    , varStrictType (mkName "status") $ strictType notStrict [t| String |]
    , varStrictType (mkName "source") $ strictType notStrict [t| String |]
    , varStrictType (mkName "has_notes") $ strictType notStrict
                                             [t| Maybe Bool |]
    , varStrictType (mkName "has_comments") $ strictType notStrict
                                                [t| Maybe Bool |]
    , varStrictType (mkName "preview_width") $ strictType notStrict
                                                 [t| Integer |]
    , varStrictType (mkName "preview_height") $ strictType notStrict
                                                 [t| Integer |]
    ]
  ] [mkName "Show", mkName "Eq"]
