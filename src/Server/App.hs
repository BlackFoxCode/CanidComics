
{-# LANGUAGE DataKinds #-}

module Server.App(run, mkApp) where

import qualified Network.Wai.Handler.Warp as Warp
import           Servant                  (Application, Context (EmptyContext), serveWithContext)

import           Api                      (API, apiHandler)

run :: Int -> IO ()
run port = do
  putStrLn $ "running api on port: " <> show port
  app <- mkApp
  Warp.run port app

mkApp :: IO Application
mkApp = do
  let webApiProxy = Proxy :: Proxy API
  return $ serveWithContext webApiProxy EmptyContext apiHandler
