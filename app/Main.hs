-- {-# LANGUAGE OverlappingInstances #-}

module Main (main) where

import           Server.App
-- import           System.Environment (getArgs)

main :: IO ()
main = run 8888
