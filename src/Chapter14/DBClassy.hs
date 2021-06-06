{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Chapter14.DBClassy (DbConfig(..), HasDbConfig(..), connectDB) where

import Control.Lens
import Control.Monad.Reader

data DbConfig =
  DbConfig { _databaseUrl :: String
           , _maxConnections :: Int
           } deriving Show

makeClassy ''DbConfig

connectDB :: (MonadIO m, HasDbConfig e, MonadReader e m) => m ()
connectDB = do
  url <- view databaseUrl
  numConnections <- view maxConnections
  liftIO
    $ putStrLn ("connecting to db at: "
                 <> url
                 <> " with max connections: "
                 <> show numConnections)
