{- |
Module: Api.Comics
Description: Routes and handlers for '/comics'
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>
Stability: experimental

Comics API
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Comics
    ( ComicAPI
    , comicAPI
    , listComics
    , newComic
    , deleteComic
    ) where

import           Control.Lens
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Servant

import           Api.Database
import           Api.Database.Tables.Comics

type ComicAPI = Get '[JSON] [Comic]
           :<|> ReqBody '[JSON] NewComic :> Post '[JSON] Comic
           :<|> Capture "comicID" Int32 :> DeleteNoContent '[JSON] NoContent

comicAPI :: Server ComicAPI
comicAPI = listComics :<|> newComic :<|> deleteComic

listComics :: Handler [Comic]
listComics = liftIO $ withBeam $ runSelectReturningList $ select (all_ (canidComicsDb ^. comics))


newComic :: NewComic -- ^ object to be inserted into database
         -> Handler Comic
newComic (NewComic n d a u) = liftIO $ do
    [result] <- withBeam $ runInsertReturningList $ insert (canidComicsDb ^. comics) $
                insertExpressions [Comic default_ (val_ n) (val_ d) (val_ a) (val_ u)]
    return result


deleteComic :: Int32 -> Handler NoContent
deleteComic cid = do
    hasRow <- liftIO . withBeam $ do
        row <- runSelectReturningOne $ select $ do
            comic <- all_ (canidComicsDb ^. comics)
            guard_ (comic ^. comicId ==. val_ cid)
            pure comic
        case row of
          Nothing -> return False
          Just _  -> do
              runDelete $ delete (canidComicsDb ^. comics) (\c -> c ^. comicId ==. val_ cid)
              return True
    if hasRow
       then return NoContent
       else throwError err404 { errBody = "Comic does not exist" }

-- updateComic :: Int32 -> UpdateComic -> Handler Comic
-- updateComic cid comic@(UpdateComic n d a u) = do
--     result <- liftIO . withBeam $ do
--         row <- runSelectReturningOne $ select $ do
--             comic <- all_ (canidComicsDb ^. comics)
--             guard_ (comic ^. comicId ==. val_ cid)
--             pure comic
--         case row of
--           Nothing -> return Nothing
--           Just c  -> do
--               runUpdate $ save (canidComicsDb ^. comics) (c 

-- replaceComic :: NewComic -> Handler Comic
-- replaceComic comic@(NewComic n d a u) = return comic
