module Import
    ( module Yesod
    , module Foundation
    , module Database.Persist.Sqlite
    , module Data.Monoid
    , module Control.Applicative
    , Text
    , module Util.ListMovement
    , module Util.Auth
    , module Model
    , module Model.Types
    , module Model.Create
    , module Model.Update
    , toJSONWithId
    ) where

import Yesod   hiding (Route(..))
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat), (<>))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Database.Persist.Sqlite

import Util.ListMovement
import Util.Auth

import Model
import Model.Types
import Model.Create
import Model.Update

import qualified Data.HashMap.Strict as H

toJSONWithId :: (ToJSON a, ToJSON b) => a -> b -> Value
toJSONWithId i v = case toJSON v of
    Object o -> Object $ H.singleton "id" (toJSON i) `H.union` o
    v' -> v'
