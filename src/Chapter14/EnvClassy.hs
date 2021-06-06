{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Chapter14.EnvClassy where

import Control.Lens
import Control.Monad.Reader
import Chapter14.DBClassy

data Env =
  Env { _envDbConfig :: DbConfig
      } deriving Show

-- makeFields ''Env
makeLenses ''Env

instance HasDbConfig Env where
  dbConfig = envDbConfig

env = Env (DbConfig "db.example.com" 100)

env1 = env ^. databaseUrl
-- "db.example.com"
env2 = env ^. maxConnections
-- 100
env3 = env ^. dbConfig
-- DbConfig {_databaseUrl = "db.example.com", _maxConnections = 100}
