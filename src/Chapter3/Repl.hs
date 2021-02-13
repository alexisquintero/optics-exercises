{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Control.Lens.Unsound

ex1 :: String
ex1 = view (_2 . _1) (42, ("hello", False))

-- "hello"

data Ship =
  Ship { _name    :: String
       , _numCrew :: Int
       }
  deriving (Show)

-- getNumCrew :: Ship -> Int
-- getNumCrew = _numCrew

-- setNumCrew :: Ship -> Int -> Ship
-- setNumCrew ship newNumCrew = ship { _numCrew = newNumCrew }

-- numCrew :: Lens' Ship Int
-- numCrew = lens getNumCrew setNumCrew

makeLenses ''Ship

purplePearl :: Ship
purplePearl = Ship { _name =    "Purple Pearl"
                   , _numCrew = 38
                   }

newValue = "Firefly"

setGet :: String
setGet = view _1 (set _1 newValue ("Star Wars", "Star Trek"))

setGetLaw = newValue == setGet

structure = ("Simpsons", "Seinfield")

getSet :: (String, String)
getSet = set _1 (view _1 structure) structure

getSetLaw = structure == getSet

value1 = "Coffe"
value2 = "Red Bull"
structure' = ("Beer", "Tea")

setSet1 :: (String, String)
setSet1 = set _1 value2 (set _1 value1 structure')

setSet2 :: (String, String)
setSet2 = set _1 value2 structure'

setSetLaw = setSet1 == setSet2

type UserName = String
type UserId = String

data Session =
  Session { _userId :: UserId
          , _userName :: UserName
          , _createdTime :: String
          , _expiryTime :: String
          }
          deriving (Show, Eq)

makeLenses ''Session

userInfo :: Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName

session = Session "USER-1234" "Joey Tribbiani" "2019-07-25" "2019-08-25"

usrInfo :: (UserId, UserName)
usrInfo = view userInfo session

alongsideUserId :: Lens' Session (Session, UserId)
alongsideUserId = lensProduct id userId

newSession = session { _userId = "USER-5678" }

setGetProduct :: (Session, UserId)
setGetProduct =
  view alongsideUserId (set alongsideUserId (newSession, "USER-9999") session)

setGetProductLaw = (newSession, "USER-9999") == setGetProduct
-- False

data Temperature =
  Temperature { _location :: String
              , _celsius  :: Float
              }
  deriving (Show)

celsius :: Lens' Temperature Float
celsius = lens _celsius setter
  where setter s a = s { _celsius = a }

temp = Temperature "Berlin" 7.0
tempv = view celsius temp
newTemp = set celsius 13.5 temp
newTemp' = over celsius (+10) temp

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9/5)) + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5/9)

tempf = celsiusToFahrenheit $ view celsius temp
newTempf = set celsius (fahrenheitToCelsius 56.3) temp
newTempf' = over celsius (fahrenheitToCelsius . (+18) . celsiusToFahrenheit) temp

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
  where getter = celsiusToFahrenheit . view celsius
        setter temp f = set celsius (fahrenheitToCelsius f) temp

tempfv = view fahrenheit temp
newTempfl = set fahrenheit 56.3 temp
newTempfl' = over fahrenheit (+18) temp

readTemp :: IO Float
readTemp = pure 1

updateTempReading :: Temperature -> IO Temperature
updateTempReading temp = do
  newTempInCelsius <- readTemp
  return temp { _celsius = newTempInCelsius }

data Temperature' =
  Temperature' { _location' :: String
               , _kelvin    :: Float
               }
  deriving (Show)

makeLenses ''Temperature'

updateTempReading' :: Temperature -> IO Temperature
updateTempReading' (Temperature location _) = do
  newTempInCelsius <- readTemp
  return (Temperature location newTempInCelsius)

updateTempReading'' :: Temperature' -> IO Temperature'
updateTempReading'' (Temperature' location _) = do
  newTempInCelsius <- readTemp
  return (Temperature' location newTempInCelsius)

updateTempReadingLens :: Temperature -> IO Temperature
updateTempReadingLens temp = do
  newTempInCelsius <- readTemp
  return $ set celsius newTempInCelsius temp

celsius' :: Lens' Temperature' Float
celsius' = lens getter setter
  where getter = (subtract 273.15) . view kelvin
        setter temp c = set kelvin (c + 273.15) temp

updateTempReadingLens' :: Temperature' -> IO Temperature'
updateTempReadingLens' temp = do
  newTempInCelsius <- readTemp
  -- return $ set celsius newTempInCelsius temp
  return $ set celsius' newTempInCelsius temp

data Time =
  Time { _hours :: Int
       , _mins :: Int
       }
  deriving (Show)

clamp :: Int -> Int -> Int -> Int
clamp minVal maxVal = min maxVal . max minVal

hours :: Lens' Time Int
hours = lens getter setter
  where getter (Time h _) = h
        setter (Time _ m) newHours = Time (clamp 0 23 newHours) m

mins :: Lens' Time Int
mins = lens getter setter
  where getter (Time _ m) = m
        setter (Time h _) newMins = Time h $ clamp 0 59 newMins

time = Time 3 10
setHours = set hours 40 time
setMins = set mins (-10) time

hours' :: Lens' Time Int
hours' = lens getter setter
  where getter (Time h _) = h
        setter (Time _ m) newHours = Time (newHours `mod` 24) m

mins' :: Lens' Time Int
mins' = lens getter setter
  where getter (Time _ m) = m
        setter (Time h _) newMinutes =
          Time ((h + (newMinutes `div` 60)) `mod` 24) (newMinutes `mod` 60)

overMins1 = over mins' (+ 55) time
overMins2 = over mins' (subtract 20) time
overMins3 = over mins' (+1) (Time 23 59)
