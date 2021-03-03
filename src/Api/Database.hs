{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

Database Common
-}

module Api.Database
    ( connectPSql
    ) where

import Configuration.Dotenv     (loadFile, defaultConfig)
import Control.Exception        (bracket)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Database.HDBC            (disconnect)
import System.Environment       (getEnv)
import System.Directory         (doesFileExist)


connectPSql :: IO Connection
connectPSql = do
    e <- doesFileExist ".env"
    when e $ void (loadFile defaultConfig)

    hostname <- getEnv "API_DB_HOST"
    port     <- getEnv "API_DB_PORT"
    username <- getEnv "API_DB_USER"
    password <- getEnv "API_DB_PASS"
    database <- getEnv "API_DB_DB"

    bracket (connectPostgreSQL (  "host="      ++ hostname
                               ++ " port="     ++ port
                               ++ " user="     ++ username
                               ++ " password=" ++ password
                               ++ " dbname="   ++ database))
            disconnect
            return
