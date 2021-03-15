{- |
Module: Api.Database
Description: Database Setup and Connection Management
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>
Stability: experimental

Database Common
-}

-- This is disabled for the Lens definitions
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies       #-}

module Api.Database
  ( -- * Database definitions
    CanidComicsDb (..)
  , canidComicsDb
  , -- ** Database table lenses
    comics
  , users
  , -- * Connection functions
    withBeam
  , withDBConnection
  )
where

import           Configuration.Dotenv       (defaultConfig, loadFile)
import           Control.Exception
import           Database.Beam              (Database, DatabaseSettings, TableEntity, TableLens (TableLens), dbLenses,
                                             defaultDbSettings)
import           Database.Beam.Postgres     (Pg, runBeamPostgres, runBeamPostgresDebug)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection, close, connectPostgreSQL,
                                             postgreSQLConnectionString)
import           System.Directory           (doesFileExist)
import           System.Environment         (lookupEnv)

import           Api.Database.Tables

-- | Database Definition
data CanidComicsDb f
  = CanidComicsDb
      { _comics :: f (TableEntity ComicT)
      , _users  :: f (TableEntity UserT)
      }
  deriving stock Generic
  deriving anyclass (Database be)

canidComicsDb :: DatabaseSettings be CanidComicsDb
canidComicsDb = defaultDbSettings

CanidComicsDb (TableLens comics)
              (TableLens users)
               = dbLenses

-- | Simple bracket wrapper to ensure database connections are closed
withDBConnection :: (Connection -> IO a) -> IO a
withDBConnection = bracket openPostgres close

-- | Wrapper for runBeamPostgres that enables debug printing to STDOUT if API_DEBUG set in env
withBeam :: Pg a -> IO a
withBeam f = do
  isDebug <- lookupEnv ("API_DEBUG" :: String)
  let run = case isDebug of
        Nothing -> runBeamPostgres
        Just _  -> runBeamPostgresDebug (putStrLn :: String -> IO ())
  withDBConnection $ \c -> run c f

-- | Connect to PostgreSQL using environment variables for configuration
openPostgres :: IO Connection
openPostgres = do
  e <- doesFileExist ".env"
  when e $ void (loadFile defaultConfig)

  hostname <- lookupEnv ("API_DB_HOST" :: String)
  port     <- lookupEnv ("API_DB_PORT" :: String)
  username <- lookupEnv ("API_DB_USER" :: String)
  password <- lookupEnv ("API_DB_PASS" :: String)
  database <- lookupEnv ("API_DB_DB"   :: String)

  let port' = case (readEither (fromMaybe "5432" port) :: Either Text Word16) of
        Right p -> p
        Left _  -> error "Could not recognise PORT"

  let connectionInfo =
        ConnectInfo
          (fromMaybe "localhost" hostname)
          port'
          (fromMaybe "canidcomics" username)
          (fromMaybe "" password)
          (fromMaybe "canidcomics" database)
  connectPostgreSQL $ postgreSQLConnectionString connectionInfo
