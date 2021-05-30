{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter13.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Control.Monad.Reader (ask, runReaderT, liftIO, asks, ReaderT)
import Control.Monad.State
import Text.Printf

----------------------------------------
-- 13. Optics and Monads
----------------------------------------

----------------------------------------
-- 13.1 Reader Monad and View
----------------------------------------

type UserName = String
type Password = String
data Env =
  Env { _currentUser :: UserName
      , _users :: M.Map UserName Password
      }
  deriving Show

makeLenses ''Env

printUser :: ReaderT Env IO ()
printUser = do
  user <- asks _currentUser
  liftIO . putStrLn $ "Current user: " <> user

main :: IO ()
main = runReaderT printUser (Env "jenkins" (M.singleton "jenkins" "hunter2"))
-- Current user: jenkins

printUser' :: ReaderT Env IO ()
printUser' = do
  user <- view currentUser
  liftIO . putStrLn $ "Current user: " <> user

-- view :: MonadReader s m => Getting a s a -> m a

getUserPassword :: ReaderT Env IO ()
getUserPassword = do
  userName <- view currentUser
  maybePassword <- preview (users . ix userName)
  liftIO $ print maybePassword

main2 :: IO ()
main2 =
  runReaderT getUserPassword (Env "jenkins" (M.singleton "jenkins" "hunter2"))
-- Just "hunter2"

----------------------------------------
-- 13.2 State Monad Combinators
----------------------------------------

data Till =
  Till { _total :: Double
       , _sales :: [Double]
       , _taxRate :: Double
       } deriving Show

makeLenses ''Till

saleCalculation :: StateT Till IO ()
saleCalculation = do
  total .= 0
  total += 8.55
  total += 7.36
  totalSale <- use total
  liftIO $ printf "Total sale: $%.2f\n" totalSale

-- (.=) :: MonadState s m => Lens s s a b -> b -> m ()

-- use :: MonadState s m => Lens' s a -> m a
-- use :: MonadState s m => Getting a s a -> m a

salecalculation1 = execStateT saleCalculation (Till 0 [] 1.11)
-- Total sale: $15.91
-- Till { _total = 15.91, _sales = [], _taxRate = 1.11 }

saleCalculation' :: StateT Till IO ()
saleCalculation' = do
  total .= 0
  total += 8.55
  total += 7.36
  totalSale <- use total
  liftIO $ printf "Total sale: $%.2f\n" totalSale
  sales <>= [totalSale]
  total <~ uses taxRate (totalSale *)
  taxIncluded <- use total
  liftIO $ printf "Tax included: $%.2f\n" taxIncluded

salecalculation2 = execStateT saleCalculation' (Till 0 [] 1.11)
-- Total sale: $15.91
-- Tax included: $17.66
-- Till { _total = 17.6601, _sales = [15.91], _taxRate = 1.11 }

-- uses :: MonadState s m => Lens' s a -> (a -> r) -> m r

-- (<~) :: MonadState s m => Lens s s a b -> m b -> m ()

----------------------------------------
-- 13.3 Magnify & Zoom
----------------------------------------

-- magnify :: Lens' s a -> ReaderT a m r -> ReaderT s m r
-- magnify :: Getter s a -> ReaderT a m r -> ReaderT s m r

data Weather =
  Weather { _temperature :: Float
          , _pressure :: Float
          }
  deriving Show

makeLenses ''Weather

printData :: String -> ReaderT Float IO ()
printData statName = do
  num <- ask
  liftIO . putStrLn $ statName <> ": " <> show num

weatherStats :: ReaderT Weather IO ()
weatherStats = do
  magnify temperature $ printData "temp"
  magnify pressure (printData "pressure")

weatherstats1 = runReaderT weatherStats (Weather 15 7.2)

-- zoom :: Monad m => Lens' s a -> StateT a m r -> StateT s m r

convertCelciusToFahrenheit :: StateT Float IO ()
convertCelciusToFahrenheit = do
  modify (\celcius -> (celcius * (9/5)) + 32)

weatherStats' :: StateT Weather IO ()
weatherStats' = do
  zoom temperature convertCelciusToFahrenheit

weatherstats2 = execStateT weatherStats' (Weather 32 12)
