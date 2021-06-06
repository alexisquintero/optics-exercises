{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Chapter14.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad.Writer (MonadIO)
import Control.Monad.Reader (ask, runReaderT, MonadReader)
import Control.Monad.Writer (MonadIO(liftIO))

----------------------------------------
-- 14. Classy Lenses
----------------------------------------

----------------------------------------
-- 14.1 What are classy lenses and when do I need them?
----------------------------------------

----------------------------------------
-- No duplicate record fields
----------------------------------------

data Person =
  Person { _personName :: String
         } deriving Show

data Pet =
  Pet { _petName :: String
      } deriving Show

greetPerson :: Person -> IO ()
greetPerson p = putStrLn $ "Hello " <> _personName p <> "!"

greetPet :: Pet -> IO ()
greetPet p = putStrLn $ "Hello " <> _petName p <> "!"

greetperson = greetPerson (Person "Calvin")
-- Hello Calvin!
greetpet = greetPet (Pet "Hobbes")
-- Hello Hobbes!

makeFields ''Person
makeFields ''Pet

greetByName :: HasName r String => r -> IO ()
greetByName r = putStrLn $ "Hello " <> r ^. name <> "!"

greetbyname1 = greetByName (Person "Calvin")
-- Hello Calvin!
greetbyname2 = greetByName (Pet "Hobbes")
-- Hello Hobbes!

----------------------------------------
-- Separating logic and minimizing global knowledge
----------------------------------------

data Env =
  Env { _portNumber :: Int
      , _hostName :: String
      , _databaseUrl :: String
      } deriving Show

makeLenses ''Env

connectDB :: (MonadIO m, MonadReader Env m) => m ()
connectDB = do
  url <- view databaseUrl
  liftIO $ putStrLn ("connecting to db at: " <> url)

initialize :: (MonadIO m, MonadReader Env m) => m ()
initialize = do
  port <- view portNumber
  host <- view hostName
  liftIO $ putStrLn ("initializing server at: " <> host <> ":" <> show port)

main :: IO ()
main = do
  flip runReaderT (Env 8000 "example.com" "db.example.com") $ do
    initialize
    connectDB
-- initializing server at: example.com:8000
-- connecting to db at: db.example.com

type DatabaseUrl = String
connectDB' :: (MonadIO m, MonadReader DatabaseUrl m) => m ()
connectDB' = do
  url <- ask
  liftIO $ putStrLn ("connecting to db at: " <> url)

main' :: IO ()
main' = do
  flip runReaderT (Env 8000 "example.com" "db.example.com") $ do
    initialize
    ask >>= \e -> runReaderT connectDB' (_databaseUrl e)

main'' :: IO ()
main'' = do
  flip runReaderT (Env 8000 "example.com" "db.example.com") $ do
    initialize
    magnify databaseUrl connectDB'

----------------------------------------
-- Granular dependencies with `makeFields`
----------------------------------------

----------------------------------------
-- Field requirements compose
----------------------------------------

----------------------------------------
-- 14.2 `makeFields` vs `makeClassy`
----------------------------------------
