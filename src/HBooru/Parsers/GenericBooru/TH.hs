{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- |
-- Module      :  HBooru.Parsers.GenericBooru.TH
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Helper module for "HBooru.Parsers.GenericBooru" due to Template Haskell
-- limitation of being unable to splice inside of the same module of
-- definitions.
module HBooru.Parsers.GenericBooru.TH where

import Control.Applicative
import HBooru.Types
import Language.Haskell.TH

-- | A TH helper which makes an instance along with the data type using
-- 'makePost'' and 'makePostInstance'.
makePost ∷ Name → Q [Dec]
makePost n = liftA2 (++) (makePost' n) (makePostInstance n)

-- | Template Haskell function which is able to generate 'GenericPost'-alike
-- type declarations for cases where we want to use this format but need a
-- different data type. It can be used by using @TemplateHaskell@ extension and
-- calling @$(makePost ('mkName' \"YourTypeName\"))@ at the top level. Hopefully
-- a temporary measure until the author thinks of a better way to provide
-- generic Gelbooru-like post parsing while casting out to different data types
-- that's OK to write.
makePost' ∷ Name → Q [Dec]
makePost' n =
  fmap (:[]) $ dataD (cxt []) n []
  [ recC n
    [ varStrictType (mkName "heightT") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "scoreT") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "file_urlT") $ strictType notStrict [t| String |]
    , varStrictType (mkName "parent_idT") $ strictType notStrict
                                             [t| Maybe Integer |]
    , varStrictType (mkName "sample_urlT") $ strictType notStrict [t| String |]
    , varStrictType (mkName "sample_widthT") $ strictType notStrict
                                                [t| Integer |]
    , varStrictType (mkName "sample_heightT") $ strictType notStrict
                                                 [t| Integer |]
    , varStrictType (mkName "preview_urlT") $ strictType notStrict [t| String |]
    , varStrictType (mkName "ratingT") $ strictType notStrict [t| Rating |]
    , varStrictType (mkName "tagsT") $ strictType notStrict [t| [String] |]
    , varStrictType (mkName "idT") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "widthT") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "changeT") $ strictType notStrict [t| String |]
    , varStrictType (mkName "md5T") $ strictType notStrict [t| String |]
    , varStrictType (mkName "creator_idT") $ strictType notStrict [t| Integer |]
    , varStrictType (mkName "has_childrenT") $ strictType notStrict
                                                [t| Maybe Bool |]
    , varStrictType (mkName "created_atT") $ strictType notStrict [t| String |]
    , varStrictType (mkName "statusT") $ strictType notStrict [t| String |]
    , varStrictType (mkName "sourceT") $ strictType notStrict [t| String |]
    , varStrictType (mkName "has_notesT") $ strictType notStrict
                                             [t| Maybe Bool |]
    , varStrictType (mkName "has_commentsT") $ strictType notStrict
                                                [t| Maybe Bool |]
    , varStrictType (mkName "preview_widthT") $ strictType notStrict
                                                 [t| Integer |]
    , varStrictType (mkName "preview_heightT") $ strictType notStrict
                                                 [t| Integer |]
    ]
  ] [mkName "Show", mkName "Eq"]

-- | Template Haskell function which creates 'Post' instances for things made
-- with 'makePost'.
makePostInstance ∷ Name → Q [Dec]
makePostInstance n =
  return [InstanceD
          []
          (ConT (mkName "Post") `AppT` ConT n)
          [ onG "height", onG "score", onG "file_url", onG "parent_id"
          , onG "sample_url", onG "sample_width", onG "sample_height"
          , onG "preview_url", onG "rating", onG "tags", onG "id"
          , onG "width", onG "change", onG "md5", onG "creator_id"
          , onG "has_children", onG "created_at", onG "status"
          , onG "source", onG "has_notes", onG "has_comments"
          , onG "preview_width", onG "preview_height"
          ]
         ]
    where
      onG n' = FunD (mkName n')
              [ Clause [VarP (mkName "g")]
                (NormalB (AppE (VarE (mkName $ n' ++ "T"))
                          (VarE (mkName "g")))) []
              ]
