{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lists(
    ID, ListName, ListEntry, ListItem(..), List(..), ListSummary(..), ListCreation(..)
    ) where 

import Data.Aeson
import Data.ByteString.Lazy.Internal
import Data.Text
import Data.UUID
import Data.UUID.Aeson

type ListName  = Text
type ListEntry = Text
type ID        = UUID

data ListItem = ListItem {itemId :: ID, value :: ListEntry, done :: Bool} deriving Show
data List = List {listId :: ID, listName :: ListName, items :: [ListItem]} deriving Show
data ListSummary = ListSummary {summaryId :: ID, summaryName :: ListName} deriving Show
data ListCreation = ListCreation {newId :: ID} deriving Show

instance FromJSON ListItem where
  parseJSON = withObject "listItem" $ \o -> do
    itemId <- o .: "id"
    value  <- o .: "value"
    done  <- o .: "done"
    return ListItem{..}

instance ToJSON ListItem where
  toJSON ListItem{..} = object [
    "id" .= itemId,
    "value" .= value,
    "done"  .= done  ]

instance FromJSON List where
  parseJSON = withObject "list" $ \o -> do
    listId <- o .: "id"
    listName  <- o .: "name"
    items  <- o .: "items"
    return List{..}

instance ToJSON List where
  toJSON List{..} = object [
    "id" .= listId,
    "name" .= listName,
    "items"  .= items  ]

instance FromJSON ListSummary where
  parseJSON = withObject "listSummary" $ \o -> do
    summaryId <- o .: "id"
    summaryName  <- o .: "name"
    return ListSummary{..}

instance ToJSON ListSummary where
  toJSON ListSummary{..} = object [
    "id" .= summaryId,
    "name"  .= summaryName  ]

instance FromJSON ListCreation where
  parseJSON = withObject "listSummary" $ \o -> do
    newId <- o .: "id"
    return ListCreation{..}

instance ToJSON ListCreation where
  toJSON ListCreation{..} = object [
    "id" .= newId]
