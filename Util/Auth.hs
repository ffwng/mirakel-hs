module Util.Auth
    ( Auth(..)
    , getAuth
    , getUserId
    , combine
    ) where

import Model

import Yesod
import Database.Persist.Sql

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T

getUserId :: (YesodPersist a, PersistUnique (YesodDB a),
              PersistMonadBackend (YesodDB a) ~ SqlBackend)
          => HandlerT a IO UserId
getUserId = do
    Auth uid <- getAuth
    return uid

data Auth = NoAuth | UnAuth | Auth UserId
    deriving Show

getAuth :: (YesodPersist a, PersistUnique (YesodDB a),
            PersistMonadBackend (YesodDB a) ~ SqlBackend)
        => HandlerT a IO Auth
getAuth = do
    header <- lookupHeader "Authorization"
    case header of
        Nothing -> return NoAuth
        Just v -> case Base64.decode (B.drop 6 v) of
            Left _ -> return NoAuth
            Right str -> checkStr str
    where
        checkStr str = case B.break (== ':') str of
            (user, pw) | not (B.null pw) -> checkAuth user (B.tail pw)
            _ -> return NoAuth
        checkAuth user pw = do
            mu <- runDB $ getBy (UniqueName $ T.decodeUtf8 user)
            case mu of
                Nothing -> return UnAuth
                Just (Entity uid u) -> return $ if userPassword u == pw
                                                then Auth uid
                                                else UnAuth
combine :: AuthResult -> AuthResult -> AuthResult
Authorized `combine` Authorized = Authorized
a@(Unauthorized _) `combine` _ = a
_ `combine` a@(Unauthorized _) = a
_ `combine` _ = AuthenticationRequired
