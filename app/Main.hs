module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp
import           Api
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    let port :: Int
        port = if length args > 0
                  then read $ head args
                  else 8888
    run port

run :: Int -> IO ()
run port = do
    putStrLn $ "running api on port: " <> show port
    Warp.run port app
