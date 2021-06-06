{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Chapter14.Init (HasHostName(..), HasPortNumber(..)) where

import Control.Lens
import Control.Monad.Reader

data InitFields =
  InitFields { _hostName :: String
             , _portNumber :: Int
             }

makeFieldsNoPrefix ''InitFields

initialize :: ( MonadIO m
              , HasHostName e String
              , HasPortNumber e Int
              , MonadReader e m
              )
           => m ()
initialize = do
  port <- view portNumber
  host <- view hostName
  liftIO $ putStrLn ("initializing server at: " <> host <> ":" <> show port)
