{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as Text
import Data.Maybe
import qualified Data.Aeson
import Data.UUID
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Network.HTTP.Types.Status as Status
import Web.Scotty

import Lists
import DB

main = do
    connection <- connectSqlite3 "todo.db"
    DB.lookupList connection (fromJust $ fromString "85349cf0-b692-4e81-b910-24c4839a1f17") >>= print . Data.Aeson.encode
    DB.lookupList connection (fromJust $ fromString "85349cf0-b692-4e81-b910-24c4839a1f17") >>= print . show

    scotty 3000 $ do
        get     "/"                             $ html "WooHoo ToDo!!!"
        get     "/lists"                        $ listAllAction connection
        post    "/lists"                        $ addListAction connection
        get     "/lists/:id"                    $ readListAction connection
        delete  "/lists/:id"                    $ deleteListAction connection
        post    "/lists/:id/items"              $ addItemAction connection
        put     "/lists/:listiId/items/:itemId" $ markItemAction connection
        delete  "/lists/:listId/items/:itemId"  $ deleteItemAction connection
        notFound $ html "Nothing found here :("

---------------------------
-- Route handlers
---------------------------

listAllAction connection = do
    lists <- liftIO $ DB.listAll connection
    json lists

addListAction connection = do
    name <- param "name"
    id <- liftIO $ DB.addList connection name
    maybe (status notFound404)
          (json . ListCreation)
          id

readListAction connection = do
    id <- param "id"
    case fromString id of
         Just uuid -> do
            list <- liftIO . DB.lookupList connection $ uuid
            json list
         Nothing -> status badRequest400

deleteListAction connection = do
    id <- param "id"
    case fromString id of
         Just uuid -> do
            worked <- liftIO . DB.deleteList connection $ uuid
            if worked
               then status accepted202
               else status notFound404
         Nothing -> status badRequest400

addItemAction connection = do
    listId <- param "id"
    entry <- param "entry"
    case fromString listId of
        Just listId -> do
            updatedList <- liftIO $ DB.addItem connection listId entry
            maybe (status internalServerError500)
                  json
                  updatedList
        Nothing -> status badRequest400

deleteItemAction connection = do
    listId <- param "listId"
    itemId <- param "itemId"
    case fromString listId of
         Just listUuid -> 
            case fromString itemId of
                 Just itemUuid -> do
                    worked <- liftIO $ DB.deleteItem connection listUuid itemUuid
                    if worked
                        then status accepted202
                        else status notFound404
                 Nothing -> status badRequest400
         Nothing -> status badRequest400

markItemAction connection = do
    listId <- param "listId"
    itemId <- param "itemId"
    marked <- param "marked"
    case fromString listId of
         Just listUuid -> 
            case fromString itemId of
                 Just itemUuid -> 
                    case queryStringToBool marked of
                        Just isMarked -> do
                            worked <- liftIO $ DB.setItemStatus connection listUuid itemUuid isMarked
                            if worked
                                then status accepted202
                                else status notFound404
                        Nothing -> status badRequest400
                 Nothing -> status badRequest400
         Nothing -> status badRequest400

        -- FOR SURE change this to use the Either monad. 

queryStringToBool :: String -> Maybe Bool
queryStringToBool "true"  = Just True
queryStringToBool "false" = Just False
queryStringToBool _       = Nothing






