{- |
Module: Api.Database.Tables.Users
Description: Route and handlers for '/users'
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

module Api.Database.Tables.Users
    ( UserT (User)
    , User
    , UserId
    , userEmail
    , userName
    ) where

import           Data.Aeson    (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON,
                                parseJSON, toJSON)
import           Data.Char     (toLower)
import           Database.Beam (Beamable, Columnar, LensFor (LensFor), Table (..), tableLenses)

-- | Users Table
data UserT f
  = User
      { _userEmail :: Columnar f Text
      , _userName  :: Columnar f Text
      }
  deriving stock Generic
instance Beamable UserT
type User = UserT Identity
deriving stock instance Show User
deriving stock instance Eq User
instance FromJSON User where
    parseJSON =  genericParseJSON defaultOptions { fieldLabelModifier = map toLower . drop 5 }
instance ToJSON User where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 5 }

instance Table UserT where
    data PrimaryKey UserT f = UserEmail (Columnar f Text) deriving stock Generic
    primaryKey = UserEmail . _userEmail
instance Beamable (PrimaryKey UserT)
type UserId = PrimaryKey UserT Identity
deriving stock instance Show UserId; deriving stock instance Eq UserId


User (LensFor userEmail)
     (LensFor userName)
       = tableLenses
