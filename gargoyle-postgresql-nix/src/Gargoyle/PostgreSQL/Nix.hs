{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargoyle.PostgreSQL.Nix where

import Data.ByteString (ByteString)

import Gargoyle
import Gargoyle.PostgreSQL

import Paths_gargoyle_postgresql_nix
import System.Which

postgresNix :: IO (Gargoyle FilePath ByteString)
postgresNix = postgresNixWithInitOpts defaultInitOpts

postgresNixWithInitOpts :: [String] -- ^ Options to pass to @initdb@
                        -> IO (Gargoyle FilePath ByteString)
postgresNixWithInitOpts initOpts = do
  bindir <- getBinDir
  return $ (mkPostgresGargoyleWithInitOpts initOpts $(staticWhich "pg_ctl") shutdownPostgresFast)
    { _gargoyle_exec = bindir <> "/gargoyle-nix-postgres-monitor"
    }
