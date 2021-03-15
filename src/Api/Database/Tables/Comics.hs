{- |
Module: Api.Database.Tables.Comics
Description: Route and handlers for '/comics'
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>
Stability: experimental
-}

-- This is disabled for the Lens definitions
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Api.Database.Tables.Comics
    ( ComicT (Comic)
    , Comic
    , ComicId
    , NewComic (NewComic)
    , UpdateComic (UpdateComic)
    , comicId
    , comicName
    , comicDescription
    , comicAuthor
    , comicUrl
    ) where

import           Data.Aeson    (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON,
                                parseJSON, toJSON)
import           Data.Char     (toLower)
import           Database.Beam (Beamable, Columnar, LensFor (LensFor), Table (..), tableLenses)

-- | Comics Table
data ComicT f
  = Comic
      { _id          :: Columnar f Int32
      , _name        :: Columnar f Text
      , _description :: Columnar f Text
      , _author      :: Columnar f Text
      , _url         :: Columnar f Text
      }
  deriving stock Generic
instance Beamable ComicT
type Comic   = ComicT Identity
deriving stock instance Show Comic; deriving stock instance Eq Comic
instance FromJSON Comic where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 1 }
instance ToJSON Comic where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 1 }

instance Table ComicT where
    data PrimaryKey ComicT f = ComicId (Columnar f Int32)
        deriving stock Generic
    primaryKey = ComicId . _id
instance Beamable (PrimaryKey ComicT)
type ComicId = PrimaryKey ComicT Identity
deriving stock instance Show ComicId; deriving stock instance Eq ComicId

Comic (LensFor comicId)
      (LensFor comicName)
      (LensFor comicDescription)
      (LensFor comicAuthor)
      (LensFor comicUrl)
        = tableLenses

-- | A copy of Comic without the ID field
--   Used for submitting a new object before ID is calculated
--   or for full replacements
data NewComic
  = NewComic
      { _newName        :: Text
      , _newDescription :: Text
      , _newAuthor      :: Text
      , _newUrl         :: Text
      }
  deriving stock (Show, Generic)
instance FromJSON NewComic where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 4 }
instance ToJSON NewComic where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 4 }
--
-- | A copy of Comic without the ID field, and all fields are optional
--   Used for submitting a partial updates
data UpdateComic
  = UpdateComic
      { _updateName        :: Maybe Text
      , _updateDescription :: Maybe Text
      , _updateAuthor      :: Maybe Text
      , _updateUrl         :: Maybe Text
      }
  deriving stock (Show, Generic)
instance FromJSON UpdateComic where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 7 }
instance ToJSON UpdateComic where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 7 }
