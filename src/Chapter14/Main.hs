{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Chapter14.Main where

import Control.Lens
import Chapter14.DB
import Control.Monad.Reader (ReaderT(runReaderT))

data Env =
  Env { _envPortNumber :: Int
      , _envHostName :: String
      , _envDatabaseUrl :: String
      } deriving Show

makeFields ''Env

mainDB :: IO ()
mainDB = runReaderT connectDB (Env 8000 "example.com" "db.example.com")
