{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as Text
import Data.Maybe
import Data.UUID
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Network.HTTP.Types.Status as Status
import Web.Scotty

import Lists
import DB

main = do
    connection <- connectSqlite3 "todo.db"
    DB.lookupList connection (fromJust $ fromString "525c0a1f-fcf3-4685-bba0-6c5c7ee8efd7") >>= print . show
    DB.listAll connection >>= print . show

    scotty 3000 $ do
        get     "/" $ html "Let's get this ToDone!"
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
    html . Text.pack $ show lists

addListAction connection = do
    name <- param "name"
    id <- liftIO $ DB.addList connection name
    maybe (status notFound404)
          (\newId -> html . Text.pack $ "{id:" ++ toString newId ++ "}")
          id

readListAction connection = do
    id <- param "id"
    case fromString id of
         Just uuid -> do
            list <- liftIO . DB.lookupList connection $ uuid
            html . Text.pack . show $ list
         Nothing -> status badRequest400

deleteListAction connection = do
    id <- param "id"
    case fromString id of
         Just uuid -> do
            worked <- liftIO . DB.deleteList connection $ uuid
            if worked
               then html . Text.pack $ "Deleted!"
               else html . Text.pack $ "No list with that ID!"
         Nothing -> status badRequest400

addItemAction connection = do
    listId <- param "id"
    entry <- param "entry"
    case fromString listId of
        Just listId -> do
            updatedList <- liftIO $ DB.addItem connection listId entry
            maybe (status internalServerError500)
                  (\l -> html . Text.pack $ show l)
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
                        then html $ Text.pack "Deleted!"
                        else html $ Text.pack "No matching list item!"
                 Nothing -> html "Not a valid Item UUID!"
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
                                then html $ Text.pack "Deleted!"
                                else html $ Text.pack "No matching list item!"
                        Nothing -> status badRequest400
                 Nothing -> status badRequest400
         Nothing -> status badRequest400

        -- FOR SURE change this to use the Either monad. 

queryStringToBool :: String -> Maybe Bool
queryStringToBool "true"  = Just True
queryStringToBool "false" = Just False
queryStringToBool _       = Nothing






