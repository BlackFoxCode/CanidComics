{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF rivet-autoimporter #-}
module Main (main) where

import           Configuration.Dotenv
import           Database.Rivet.Adaptor.PostgreSQL
import qualified Database.Rivet.Main               as Rivet
import qualified Database.Rivet                    as R (Migration)
import           System.Directory                  (doesFileExist)
import           System.Environment

migrations :: [(Text, R.Migration IO ())]
-- Created via `rivet-autoimporter` above

main :: IO ()
main = do
    args <- getArgs

    let mode' = if not (null args)
                   then viaNonEmpty head args
                   else error "Usage: [executable] [up|down|status]"

    let mode = case fromMaybe "unkown" mode' of
                 "up"     -> Rivet.MigrateUp
                 "down"   -> Rivet.MigrateDown
                 "status" -> Rivet.MigrateStatus
                 _ -> error "Usage: [executable] [up|down|status]"

    e <- doesFileExist ".env"
    when e $ void (loadSafeFile defaultValidatorMap ".env-schema.yml" defaultConfig)

    hostname <- getEnv "API_DB_HOST"
    port     <- getEnv "API_DB_PORT"
    username <- getEnv "API_DB_USER"
    password <- getEnv "API_DB_PASS"
    database <- getEnv "API_DB_DB"

    let port' = case (readEither port :: Either Text Word16) of
                  Right p -> p
                  Left  _ -> error "Could not recognise PORT"

    adaptor <- setup id (ConnectInfo hostname port' username password database)

    Rivet.main adaptor mode migrations
