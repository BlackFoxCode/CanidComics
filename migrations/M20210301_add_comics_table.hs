{-# LANGUAGE OverloadedStrings #-}
module M20210301_add_comics_table (migrate) where

import Database.Rivet.V0

migrate :: Migration IO ()
migrate = createTable "comics"
         [ ColumnSpec "id"          "SERIAL"       Nothing (Just "PRIMARY KEY")
         , ColumnSpec "name"        "varchar(32)"  Nothing (Just "NOT NULL")
         , ColumnSpec "description" "text"         Nothing  Nothing
         , ColumnSpec "url"         "varchar(128)" Nothing (Just "NOT NULL")
         ]
