{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Chapter17.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import GHC.Generics (Generic)
import Data.Generics.Product (field, position, typed, the, super, upcast, smash
                             , types, param)
import Data.Generics.Sum (_Ctor, _Typed)

----------------------------------------
-- 17. `generic-lens`
----------------------------------------

----------------------------------------
-- Record Fields
----------------------------------------

data Person =
  Person { name :: String
         , age :: Int
         } deriving (Show, Generic)

jon :: Person
jon = Person "Jon" 32

field1 = jon ^. field @"name"
-- "Jon"
field2 = jon ^. field @"age"
-- 32
-- field3 = jon ^. field @"socialSecurityNumber"

----------------------------------------
-- Positional Lenses
----------------------------------------

data Person' = Person' String Int
  deriving (Show, Generic)

jon' = Person' "Jon" 32

position1 = jon' ^. position @1
-- "Jon"
position2 = jon' ^. position @2
-- 32
-- position3 = jon' ^. position @3

----------------------------------------
-- Typed Lenses
----------------------------------------

typed1 = jon ^. typed @Int
-- 32
typed2 = jon ^. typed @String
-- "Jon"
-- typed3 = jon ^. typed @Bool

data IntPair = IntPair Int Int
  deriving (Show, Generic)

intPair = IntPair 1 2
-- typed4 = intPair ^. typed @Int

----------------------------------------
-- Altogether Now
----------------------------------------

the1 = jon ^. the @"name"
-- "Jon"
the2 = jon ^. the @Int
-- 32
the3 = jon ^. the @1
-- "Jon"

----------------------------------------
-- Subtype Lenses
----------------------------------------

data Dwelling =
  Dwelling { address :: String
           , numBedrooms :: Int
           }
  deriving (Show, Generic)

data Apartment =
  Apartment { address' :: String
            , numBedrooms' :: Int
            , allowsPets :: Bool
            }
  deriving (Show, Generic)

data House =
  House { address'' :: String
        , numBedrooms'' :: Int
        , lotSize :: Int
        }
  deriving (Show, Generic)

apt = Apartment "2020 Willow St. Apt 307" 2 True

-- super1 = apt ^. super @Dwelling
-- Dwelling
--   { address = "2020 Willow St. Apt 307"
--   , numBedrooms = 2
--   }

-- upcast1 = upcast @Dwelling apt
-- Dwelling
--   { address = "2020 Willow St. Apt 307"
--   , numBedrooms = 2
--   }

dwelling = Dwelling "2020 Willow St. Apt 409" 3

-- super2 = apt & super .~ dwelling
-- Apartment
--   { address = "2020 Willow St. Apt 409"
--   , numBedrooms = 3
--   , allowsPets = True
--   }

-- smash1 = smash dwelling apt
-- Apartment
--   { address = "2020 Willow St. Apt 409"
--   , numBedrooms = 3
--   , allowsPets = True
--   }

----------------------------------------
-- 17.2 Generic Traversals
----------------------------------------

----------------------------------------
-- Types Traversal
----------------------------------------

types1 = intPair ^.. types @Int
-- [1, 2]
types2 = intPair & types @Int +~ 10
-- IntPair 11 12
types3 = intPair ^.. types @String
-- []

----------------------------------------
-- Parameter Traversals
----------------------------------------

data ListTuple a b = ListTuple a [b]
  deriving (Show, Generic)

tup = ListTuple "a" ["b1", "b2"] :: ListTuple String String

-- param1 = tup ^.. param @0
-- ["b1", "b2"]
-- param2 = tup ^.. param @1
-- ["a"]
-- param3 = ListTuple True [1, 2, 3] & param @0 +~ 10
-- ListTuple True [11, 12, 13]

----------------------------------------
-- 17.3 Generic Prisms
----------------------------------------

----------------------------------------
-- Constructor Prisms
----------------------------------------

data Video =
    DVD String
  | VHS String
  | LaserDisc String
  deriving (Show, Generic)

ctor1 = DVD "The Princess Bride" ^? _Ctor @"DVD"
-- Just "The Princess Brid"
ctor2 = _Ctor @"LaserDisc" @Video # "King Kong"
-- LaserDisc "King Kong"
-- ctor3 = VHS "101 Dalmatians" ^? _Ctor @"HDDVD"

----------------------------------------
-- Types Prisms
----------------------------------------

data ID
  = NumID Int
  | StrID String
  deriving (Show, Generic)

_typed1 = NumID 3 ^? _Typed @Int
-- Just 3
_typed2 = NumID 3 ^? _Typed @String
-- Nothing
-- _typed3 = NumID 3 ^? _Typed @Bool
-- _typed4 = (Left "ambiguity" :: Either String String) ^? _Typed @String

----------------------------------------
-- 17.4 Exercises - Generic Lens
----------------------------------------

-- TODO

