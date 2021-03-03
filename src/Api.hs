{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

Canid Comics API
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( app
    , api
    ) where

import Servant

import Api.Comics

type API = "comics" :> ComicAPI

api :: Proxy API
api = Proxy

server :: Server API
server = comicAPI

app :: Application
app = serve api server