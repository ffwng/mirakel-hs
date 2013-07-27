-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings
    ( PersistConfig
    , staticDir
    , Extra
    , module Settings.Development
    ) where

import Database.Persist.Sqlite (SqliteConf)
import Settings.Development

-- | Which Persistent backend this site is using.
type PersistConfig = SqliteConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

type Extra = ()
