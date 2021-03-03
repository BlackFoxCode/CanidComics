{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

Comics API
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Comics
    ( ComicAPI
    , Comic(..)
    , comicAPI
    ) where

import Servant
import Data.Aeson
import Database.HDBC

import Api.Database

type ComicAPI = Get '[JSON] [Comic]
           :<|> ReqBody '[JSON] Comic :> Post '[JSON] Comic

data Comic = Comic
    { name        :: String
    , description :: Maybe String
    , url         :: String
    } deriving stock (Eq, Show, Generic)
instance ToJSON Comic
instance FromJSON Comic

comicAPI :: Server ComicAPI
comicAPI = listComics :<|> newComic

listComics :: Handler [Comic]
listComics = do
    liftIO $ putStrLn "List"
    conn <- liftIO connectPSql
    results <- liftIO $ quickQuery' conn
               "SELECT * FROM comics"
               []
    let rows = map convRow results
    return rows
  where convRow :: [SqlValue] -> Comic
        convRow [_, sqlName, sqlDesc, sqlUrl] =
            Comic (fromSql sqlName) (fromSql sqlDesc) (fromSql sqlUrl)
        convRow _ = error "Unexpected SQL output"

newComic :: Comic -> Handler Comic
newComic comicJSON = do
    liftIO $ putStrLn "Create"
    conn <- liftIO connectPSql
    lastID <- liftIO .  withTransaction conn $ \dbh -> do
        ins <- liftIO $ prepare dbh "INSERT INTO comics (name, description, url) VALUES (?, ?, ?)"
        execute ins (fromComic comicJSON)
    liftIO . putStrLn $ "Created new Comic with ID: " ++ show lastID
    return comicJSON
  where fromComic (Comic n d u) = [toSql n, toSql d, toSql u]
