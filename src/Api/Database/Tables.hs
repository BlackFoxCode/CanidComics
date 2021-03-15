{- |
Module: Api.Database.Tables
Description: Route and handlers for '/tables'
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>
Stability: experimental

Re-export all tables. Will likely also include
related helper functions at a later point.
-}

module Api.Database.Tables
   ( module Comics
   , module Users
   ) where

import           Api.Database.Tables.Comics as Comics
import           Api.Database.Tables.Users  as Users
