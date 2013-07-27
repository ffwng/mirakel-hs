{-# LANGUAGE RecordWildCards #-}
module Model
    ( module Model.Types
    , UserGeneric(..), User, UserId
    , ListGeneric(..), List, ListId
    , TaskGeneric(..), Task, TaskId
    , Unique(..)
    , EntityField(..)
    , migrateAll
    ) where

import Model.Types

import Prelude
import Yesod
import Database.Persist.Quasi

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Data.Word (Word)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (UserGeneric b) where
    toJSON User{..} = object ["name" .= userName, "email" .= userEmail]

instance ToJSON (ListGeneric b) where
    toJSON List{..} = object
        [ "name" .= listName
        , "user-id" .= listUserId
        , "created-at" .= listCreatedAt
        , "updated-at" .= listUpdatedAt
        ]

instance ToJSON (TaskGeneric b) where
    toJSON Task{..} = object
        [ "name" .= taskName
        , "content" .= taskContent
        , "list-id" .= taskListId
        , "created-at" .= taskCreatedAt
        , "updated-at" .= taskUpdatedAt
        , "priority" .= taskPriority
        , "due" .= taskDue
        , "done" .= taskDone
        ]
