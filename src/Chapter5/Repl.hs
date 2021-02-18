{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter5.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

----------------------------------------
-- 5.1 Lens Operators
----------------------------------------

----------------------------------------
-- 5.2 view a.k.a. ^.
----------------------------------------

data Payload =
  Payload { _weightKilos :: Int
          , _cargo :: String
          }
  deriving (Show)

makeLenses ''Payload

data Ship =
  Ship { _payload :: Payload
       }
  deriving (Show)

makeLenses ''Ship

serenity :: Ship
serenity = Ship (Payload 50000 "Livestock")

v1 = view (payload . cargo) serenity
v2 = serenity ^. payload . cargo
-- v2 = serenity ^.payload.cargo

----------------------------------------
-- 5.3 seet a.k.a. .~
----------------------------------------

s1 = set (payload . cargo) "Medicine" serenity
s2 = serenity & payload . cargo .~ "Medicine"
s3 = (payload . cargo .~ "Medicine") serenity
s4 = payload . cargo .~ "Medicine" $ serenity

----------------------------------------
-- 5.4 Chaining many operations
----------------------------------------

c1 = serenity
  & payload . cargo .~ "Chocolate"
  & payload . weightKilos .~ 2310

c2 = serenity
  & set (payload . cargo) "Chocolate"
  & set (payload . weightKilos) 2310

----------------------------------------
-- 5.5 Using %~ a.k.a. over
----------------------------------------

o1 = serenity
  & payload . weightKilos %~ subtract 1000
  & payload . cargo .~ "Chocolate"

----------------------------------------
-- 5.6 Learning Hieroglyphics
----------------------------------------

h1 = (2, 30) & _2 +~ 5
-- (2 35)
h2 = (2, 30) & _2 //~ 2
-- (2, 15.0)
h3 = (2, 30) & _1 ^~ 3
-- (8, 30)
h4 = (True, 30) & _1 ||~ True
-- (True, 30)
h5 = ("abra", 30) & _1 <>~ "cadabra"
-- ("abracadabra", 30)

----------------------------------------
-- 5.7 Modifiers
----------------------------------------

data Thermometer =
  Thermometer { _temperature :: Int
              }
  deriving (Show)

makeLenses ''Thermometer

m1 = Thermometer 20 & temperature <+~ 15
-- (35, Thermometer { _temperature = 35} )
m2 = Thermometer 20 & temperature <<+~ 15
-- (20, Thermometer { _temperature = 35} )

----------------------------------------
-- 5.8 When to use operators vs named actions?
----------------------------------------

ovn1 = map (view _2) [("Patrick", "Star"), ("Spongebob", "SquarePants")]
-- [ "Star", "SquarePants" ]
ovn2 = map (^. _2) [("Patrick", "Star"), ("Spongebob", "SquarePants")]
-- [ "Star", "SquarePants" ]
ovn3 = map (over _2 reverse) [("Patrick", "Star"), ("Spongebob", "SquarePants")]
-- [("Patrick", "ratS"), ("Spongebob", "stnaPerauqS")]
ovn4 = map (_2 %~ reverse) [("Patrick", "Star"), ("Spongebob", "SquarePants")]
-- [("Patrick", "ratS"), ("Spongebob", "stnaPerauqS")]
