{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Model.Update
    ( UserUpdate(..)
    , ListUpdate(..)
    , TaskUpdate(..)
    , updateUser, updateList, updateTask
    ) where

import Model

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Word
import qualified Data.HashMap.Strict as H
import Data.Time

import Control.Applicative

data UserUpdate = UserUpdate
    { userUpdateName :: Maybe Text
    , userUpdateEmail :: Maybe Text
    , userUpdatePassword :: Maybe ByteString
    }
    deriving Show

data ListUpdate = ListUpdate
    { listUpdateName :: Maybe Text
    , listUpdateSorting :: Maybe TasksSorting
    }
    deriving Show

data TaskUpdate = TaskUpdate
    { taskUpdateName :: Maybe Text
    , taskUpdateContent :: Maybe (Maybe Text)
    , taskUpdatePriority :: Maybe Word
    , taskUpdateDue :: Maybe (Maybe UTCTime)
    , taskUpdateDone :: Maybe Bool
    }
    deriving Show

(.:??) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:?? key = case H.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> Just <$> parseJSON v

instance FromJSON UserUpdate where
    parseJSON (Object v) = UserUpdate <$>
                           v .:?? "name" <*>
                           v .:?? "email" <*>
                           v .:?? "password"
    parseJSON _ = empty

instance FromJSON ListUpdate where
    parseJSON (Object v) = ListUpdate <$>
                           v .:?? "name" <*>
                           v .:?? "sorting"
    parseJSON _ = empty

instance FromJSON TaskUpdate where
    parseJSON (Object v) = TaskUpdate <$>
                           v .:?? "name" <*>
                           v .:?? "content" <*>
                           v .:?? "priority" <*>
                           v .:?? "due" <*>
                           v .:?? "done"
    parseJSON _ = empty

updateUser :: User -> UserUpdate -> User
updateUser u@User{..} UserUpdate{..} = u
    { userName = fromMaybe userName userUpdateName
    , userEmail = fromMaybe userEmail userUpdateEmail
    , userPassword = fromMaybe userPassword userUpdatePassword
    }

updateList :: UTCTime -> List -> ListUpdate -> List
updateList cur l@List{..} ListUpdate{..} = l
    { listUpdatedAt = cur
    , listName = fromMaybe listName listUpdateName
    , listSorting = fromMaybe listSorting listUpdateSorting
    }

updateTask :: UTCTime -> Task -> TaskUpdate -> Task
updateTask cur t@Task{..} TaskUpdate{..} = t
    { taskUpdatedAt = cur
    , taskName = fromMaybe taskName taskUpdateName
    , taskContent = fromMaybe taskContent taskUpdateContent
    , taskPriority = fromMaybe taskPriority taskUpdatePriority
    , taskDue = fromMaybe taskDue taskUpdateDue
    , taskDone = fromMaybe taskDone taskUpdateDone
    }
