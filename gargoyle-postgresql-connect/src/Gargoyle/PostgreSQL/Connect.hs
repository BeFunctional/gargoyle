module Gargoyle.PostgreSQL.Connect where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Gargoyle (withGargoyle)
import Gargoyle.PostgreSQL (defaultInitOpts)
import Gargoyle.PostgreSQL.Nix (postgresNixWithInitOpts)
import System.Directory (doesFileExist)


-- | Like 'withDb'' but turns the connection string into a connection
-- 'Pool' for you and using 'error' on failure.
withDb :: String -> (Pool Connection -> IO a) -> IO a
withDb dbPath = withDbWithOpts dbPath defaultInitOpts

-- | 'withDb' variation that specifies options to pass to @initdb@
withDbWithOpts :: String -- ^ Path to a database
               -> [String] -- ^ Options to pass to @initdb@
               -> (Pool Connection -> IO a) -- ^ Function using connection pool
               -> IO a
withDbWithOpts dbPath initOpts f =
  either error pure =<< withDbWithInitOpts' dbPath initOpts (openDb >=> f)

-- | Convert a connection string into a connection 'Pool'.
openDb :: ByteString -> IO (Pool Connection)
openDb dbUri = createPool (connectPostgreSQL dbUri) close 1 5 20

-- | Connects to a database using information at the given filepath.
-- The given filepath can be either a folder (for a local db)
-- or a file with a database connection string.
--
-- 'withDb'' takes a String, which represents the path to a database, and a
-- function that returns database connection information as arguments in
-- order to open and start the database. Otherwise, it will create the
-- database for you if it doesn't exist.
withDb' :: String -> (ByteString -> IO a) -> IO (Either String a)
withDb' dbPath = withDbWithInitOpts' dbPath defaultInitOpts

-- | 'withDb'' variation that specifies options to pass to @initdb@
withDbWithInitOpts' :: String -- ^ Path to a database
                    -> [String] -- ^ Options to pass to @initdb@
                    -> (ByteString -> IO a) -- ^ Function using database connection string
                    -> IO (Either String a)
withDbWithInitOpts' dbPath initOpts f = do
  dbExists <- doesFileExist dbPath
  if dbExists
    -- use the file contents as the URI for an existing server
    then C8.readFile dbPath >>= \b -> case C8.lines b of
      [] -> pure $ Left "DB connection string configuration file is empty"
      -- TODO: Consider also blowing up if more than one line for next breaking release
      x:_ -> Right <$> f x
    -- otherwise assume it's a folder for a local database
    else do
      g <- postgresNixWithInitOpts initOpts
      Right <$> withGargoyle g dbPath f
