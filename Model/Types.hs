{-# LANGUAGE OverloadedStrings #-}
module Model.Types
    ( Position
    , TasksSorting(..)
    ) where

import Database.Persist
import Database.Persist.Sql
import Yesod.Core.Dispatch

import Data.Aeson
import Data.Word
import qualified Data.Text as T

import Control.Applicative

type Position = Word

data TasksSorting = Done | Due | Priority | Created
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance PersistField TasksSorting where
    toPersistValue s = PersistInt64 . fromIntegral $ fromEnum s

    fromPersistValue (PersistInt64 i) | inBounds = Right . toEnum $ fromIntegral i
        where inBounds = let i' = fromIntegral i in i' >= a && i' <= b
              a = fromEnum (minBound :: TasksSorting)
              b = fromEnum (maxBound :: TasksSorting)
    fromPersistValue _ = Left $ T.pack "unexpected value"

instance PersistFieldSql TasksSorting where
    sqlType _ = SqlInt32

instance PathPiece TasksSorting where
    fromPathPiece t = case T.unpack t of
        "done" -> pure Done
        "due" -> pure Due
        "priority" -> pure Priority
        "created" -> pure Created
        _ -> empty

    toPathPiece t = case t of
        Done -> "done"
        Due -> "due"
        Priority -> "priority"
        Created -> "created"

instance ToJSON TasksSorting where
    toJSON = String . toPathPiece

instance FromJSON TasksSorting where
    parseJSON = withText "sorting" $ \t -> case fromPathPiece t of
        Just s -> pure s
        Nothing -> fail "unexpected sorting"

