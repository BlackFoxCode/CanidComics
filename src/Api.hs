{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

Canid Comics API
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( -- * Full API type
      API
    , -- * Servant Proxy
      api
    , -- * Full server handler
      apiHandler
    ) where

import Servant

import Api.Comics

type API = "comics" :> ComicAPI

api :: Proxy API
api = Proxy

apiHandler :: Server API
apiHandler = comicAPI
