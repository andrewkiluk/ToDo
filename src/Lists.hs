{-# LANGUAGE OverloadedStrings #-}

module Lists(
    ID, ListName, ListEntry, ListItem(..), List(..), ListSummary(..)
    ) where 

import Data.Aeson
import Data.ByteString.Lazy.Internal
import Data.UUID

type ListName  = ByteString
type ListEntry = ByteString
type ID        = UUID

data ListItem = ListItem {itemId :: ID, value :: ListEntry, done :: Bool} deriving Show
data List = List {listId :: ID, listName :: ListName, items :: [ListItem]} deriving Show
data ListSummary = ListSummary {summaryId :: ID, summaryName :: ListName} deriving Show

