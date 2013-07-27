{-# LANGUAGE RecordWildCards #-}
module Model.Create
    ( UserCreate(..)
    , ListCreate(..)
    , TaskCreate(..)
    , createUser, createList, createTask
    ) where

import Model

import Data.Aeson.TH
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Word
import Data.Char
import Data.Time

data UserCreate = UserCreate
    { userCreateName :: Text
    , userCreateEmail :: Text
    , userCreatePassword :: ByteString
    }
    deriving Show

data ListCreate = ListCreate
    { listCreateName :: Text
    , listCreateSorting :: TasksSorting
    }
    deriving Show

data TaskCreate = TaskCreate
    { taskCreateName :: Text
    , taskCreateContent :: Maybe Text
    , taskCreatePriority :: Word
    , taskCreateDue :: Maybe UTCTime
    , taskCreateDone :: Bool
    }
    deriving Show

deriveFromJSON (map toLower . drop 10) ''UserCreate

deriveFromJSON (map toLower . drop 10) ''ListCreate

deriveFromJSON (map toLower . drop 10) ''TaskCreate


createUser :: UserCreate -> User
createUser UserCreate{..} = User
    { userName = userCreateName
    , userEmail = userCreateEmail
    , userPassword = userCreatePassword
    }

createList :: UserId -> UTCTime -> Position -> ListCreate -> List
createList uid cur pos ListCreate{..} = List
    { listName = listCreateName
    , listUserId = uid
    , listCreatedAt = cur
    , listUpdatedAt = cur
    , listPosition = pos
    , listSorting = listCreateSorting
    }

createTask :: ListId -> UTCTime -> TaskCreate -> Task
createTask lid cur TaskCreate{..} = Task
    { taskName = taskCreateName
    , taskContent = taskCreateContent
    , taskListId = lid
    , taskCreatedAt = cur
    , taskUpdatedAt = cur
    , taskPriority = taskCreatePriority
    , taskDue = taskCreateDue
    , taskDone = taskCreateDone
    }
