{-# LANGUAGE OverloadedStrings #-}

module DB where 

import Data.Maybe
import qualified Data.Text.Encoding as TE
import Database.HDBC
import Database.HDBC.Sqlite3
import Database.HDBC.SqlValue
import Control.Monad
import Data.UUID
import Data.UUID.V4(nextRandom)

import Lists

addList :: Connection -> ListName -> IO(Maybe ID)
addList connection name = do
    id <- nextRandom
    rows <- run connection "INSERT INTO Lists (id, name) VALUES (?, ?)" [toSql $ toString id, toSql name]
    commit connection
    return $ if rows == 1 then Just id else Nothing

deleteList :: Connection -> ID -> IO(Bool)
deleteList connection id = do 
    exists <- listExists connection id
    if exists
       then do
           deleteExistingListItems connection id
           deleteExistingList      connection id
       else return False

addItem :: Connection -> ID -> ListEntry -> IO(Maybe List)
addItem connection listID newEntry = do
    id <- nextRandom
    rows <- run connection "INSERT INTO Items (id, value, list_id) VALUES (?, ?, ?)" 
        [toSql $ toString id, toSql newEntry, toSql $ toString listID]
    commit connection
    lookupList connection listID

setItemStatus :: Connection -> ID -> ID -> Bool -> IO(Bool)
setItemStatus connection listId itemId marked = do
    rows <- run connection 
        ("UPDATE Items"
        ++ " SET marked=?"
        ++ " WHERE id=? AND list_id=?") [toSql $ fromEnum marked, toSql $ toString itemId, toSql $ toString listId]
    commit connection
    return $ rows == 1

deleteItem :: Connection -> ID -> ID -> IO(Bool)
deleteItem connection listId itemId = do
    rows <- run connection "DELETE FROM Items WHERE id=? AND list_id=?" [toSql $ toString itemId, toSql $ toString listId]
    commit connection
    return $ rows == 1

listAll :: Connection -> IO([ListSummary])
listAll connection = do
    results <- quickQuery' connection "SELECT id, name FROM Lists" []
    return $ map buildListSummary results

lookupList :: Connection -> ID -> IO(Maybe List)
lookupList connection id = do
    exists <- listExists connection id
    if exists
       then fmap Just $ lookupExistingList connection id
       else return Nothing

----------------------
-- internal functions
----------------------

buildListSummary :: [SqlValue] -> ListSummary
buildListSummary values = ListSummary {Lists.summaryId=i, Lists.summaryName=n}
    where i = idFromSql (values !! 0)
          n = TE.decodeUtf8 $ fromSql (values !! 1) :: ListEntry

lookupExistingList :: Connection -> ID -> IO(List)
lookupExistingList connection id = do
    listRows <- quickQuery' connection "SELECT name, id FROM Lists WHERE Lists.id=?" [toSql $ toString id]
    itemRows <- quickQuery' connection 
        ("SELECT id, value, marked FROM Items WHERE list_id =?") [toSql $ toString id]
    return $ rowsToList (listRows !! 0) itemRows

rowsToList :: [SqlValue] -> [[SqlValue]] -> List
rowsToList listRow itemRows = List {listId=id, listName=name, items=map buildListItem itemRows}
    where name = TE.decodeUtf8 $ fromSql (listRow !! 0) :: ListName
          id   = idFromSql (listRow !! 1)

buildListItem :: [SqlValue] -> ListItem
buildListItem values = ListItem {Lists.itemId=i, Lists.value=v, Lists.done=d }
    where i = idFromSql $ values !! 0
          v = TE.decodeUtf8 $ fromSql (values !! 1) :: ListEntry
          d = (fromSql (values !! 2) :: Int) > 0

listExists :: Connection -> ID -> IO(Bool)
listExists connection id = do
    results <- quickQuery' connection ("SELECT * FROM Lists WHERE Lists.id=?") [toSql $ toString id]
    return . (>0) . length $ results

idFromSql :: SqlValue -> ID
idFromSql value = fromJust $ fromString (fromSql value :: String)

deleteExistingListItems :: Connection -> ID -> IO([Bool])
deleteExistingListItems connection id = do
    list <- lookupExistingList connection id
    let itemIds = map itemId (items list)
    sequence $ map (deleteItem connection id) itemIds

deleteExistingList :: Connection -> ID -> IO(Bool)
deleteExistingList connection id = do
    rows <- run connection "DELETE FROM Lists WHERE id=?" [toSql $ toString id]
    commit connection
    return $ rows == 1

