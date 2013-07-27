{-# LANGUAGE NamedFieldPuns #-}
module Foundation
    ( Mirakel (..)
    , resourcesMirakel
    , Widget
    , Route (..)
    , Handler
    ) where

import Model
import Util.ListMovement
import Util.Auth

import Prelude
import Yesod
import Database.Persist.Sql

import Control.Monad
import Control.Applicative

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Mirakel = Mirakel { connectionPool :: ConnectionPool }

-- Set up i18n messages. See the message folder.
mkMessage "Mirakel" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Mirakel" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Mirakel where
    makeSessionBackend _ = return Nothing

    isAuthorized route _ = do
        auth <- getAuth
        res <- case auth of
            NoAuth -> return AuthenticationRequired
            UnAuth -> return unauth
            Auth uid -> checkAuth route uid
        when (res /= Authorized) $ addHeader "WWW-Authenticate" "Basic Realm=\"Mirakel\""
        return res
        where
            unauth = Unauthorized "Permission denied"

            checkAuth FaviconR _ = return Authorized
            checkAuth RobotsR _ = return Authorized

            checkAuth ListsR _ = return Authorized
            checkAuth (ListR lid) uid = userOwnsList uid lid
            checkAuth (ListSortByR lid _) uid = userOwnsList uid lid
            checkAuth (ListMoveR lid First) uid = userOwnsList uid lid
            checkAuth (ListMoveR lid1 (After lid2)) uid =
                liftM2 combine (userOwnsList uid lid1) (userOwnsList uid lid2)
            checkAuth (ListTasksR lid) uid = userOwnsList uid lid

            checkAuth (TaskR tid) uid = userOwnsTask uid tid
            checkAuth (TaskToggleDoneR tid) uid = userOwnsTask uid tid

            userOwnsList uid lid = do
                list <- runDB $ get404 lid
                return $ if listUserId list == uid
                         then Authorized
                         else unauth

            userOwnsTask uid tid = do
                lid <- runDB $ taskListId <$> get404 tid
                userOwnsList uid lid

-- How to run database actions.
instance YesodPersist Mirakel where
    type YesodPersistBackend Mirakel = SqlPersistT

    runDB action = do
        Mirakel{ connectionPool } <- getYesod
        runSqlPool action connectionPool

