{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Chapter14.DB (HasDatabaseUrl(..), connectDB) where

import Control.Lens
import Control.Monad.Reader

type DatabaseUrl  = String
-- data DbFields = DbFields { _dbFieldsDatabaseUrl :: DatabaseUrl }
data DbFields = DbFields { _databaseUrl :: DatabaseUrl }

-- makeFields ''DbFields
makeFieldsNoPrefix ''DbFields

connectDB :: (MonadIO m, HasDatabaseUrl e DatabaseUrl, MonadReader e m) => m ()
connectDB = do
  url <- view databaseUrl
  liftIO $ putStrLn ("connecting to db at: " <> url)
