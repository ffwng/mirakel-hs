{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Control.Monad.Logger
import Control.Monad.Trans.Resource

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.List
import Handler.Task

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Mirakel" resourcesMirakel

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    logWare   = if development then logStdoutDev
                               else logStdout

makeFoundation :: AppConfig DefaultEnv Extra -> IO Mirakel
makeFoundation conf = runResourceT . runNoLoggingT $ do
    dbconf <- liftIO $ withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Import.loadConfig >>= applyEnv
    p <- liftIO $ createPoolConfig (dbconf :: Settings.PersistConfig)
    runPool dbconf (runMigration migrateAll) p
    return $ Mirakel p

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
