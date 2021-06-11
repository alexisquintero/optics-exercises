{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Chapter15.JSON where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Text.RawString.QQ (r)

import Data.Text (toUpper)
import Data.Ord (comparing)

import GHC.Generics

import qualified Data.ByteString as BS

----------------------------------------
-- 15. JSON
----------------------------------------

----------------------------------------
-- 15.1 Introspecting JSON
----------------------------------------

aeson1 = ("42" :: String) ^? _Double
-- Just 42.0 :: Maybe Double
aeson2 = ("42" :: String) ^? _String
-- Nothing :: Maybe Text
aeson3 = ("{invalid JSON}" :: String) ^? _String
-- Nothing :: Maybe Text
aeson4 = ("\"Hello, World!\"" :: String) ^? _String
-- Just "Hello, World!" :: Maybe Text
aeson5 = ("\"Hello, World!\"" :: String) ^? _Double
-- Nothing :: Maybe Double

jsonObject :: String
jsonObject = [r|
{
  "name": "Jack Sparrow",
  "rank": "Captain"
}
|]

jsonArray :: String
jsonArray = [r|
[
  "North",
  "East",
  "South",
  "West"
]
|]

aeson6 = jsonObject ^? _Object
-- Just (fromList [("name",String "Jack Sparrow"),("rank",String "Captain")])
-- :: Maybe (HashMap Text Value)
aeson7 = jsonArray ^? _Array
-- Just [String "North",String "East",String "South",String "West"]
-- :: Maybe (Vector Value)

----------------------------------------
-- 15.2 Diving deeper into JSON structures
----------------------------------------

blackPearl :: String
blackPearl = [r|
{
  "name": "Black Pearl",
  "crew": [
  {
    "name": "Jack Sparrow",
    "rank": "Captain"
  },
  {
    "name": "Will Turner",
    "rank": "First Mate"
  }
  ]
}
|]

aeson8 = blackPearl
          ^? _Object
          . ix "crew"
          . _Array
          . ix 0
          . _Object
          . ix "name"
          . _String
-- Just "Jack Sparrow"

-- key :: Text -> Traversal' t Value
-- key k = _Object . ix k
-- nth :: Int  -> Traversal' t Value
-- nth i = _Array . ix i

aeson9 = blackPearl
           ^? key "crew"
           . nth 0
           . key "name"
           . _String
-- Just "Jack Sparrow"

-- myJSON & _Object . at "newKey" ?~ newValue

----------------------------------------
-- 15.3 Traversing into multiple JSON substructures
----------------------------------------

----------------------------------------
-- Traversing Arrays
----------------------------------------

-- values :: AsValue t => IndexedTraversal' Int t Value
-- values = _Array . traversed

aeson10 = ("[1, 2, 3]" :: String) ^.. values . _Integer
-- [1, 2, 3]
aeson11 = ("[1, null, 2, \"Hi mom!\", 3]" :: String) ^.. values . _Integer
-- [1, 2, 3]
aeson12 = ("[\"a\", \"b\", \"c\"]" :: String) ^@.. values . _String
-- [(0, "a'), (1, "b"), (2, "c")]
aeson13 = ("[\"a\", 1, \"b\", null, \"c\", []]" :: String) ^@.. values . _String
-- [(0, "a'), (2, "b"), (4, "c")]
aeson14 = ("[\"a\", \"b\", \"c\"]" :: String) & values . _String %~ toUpper
-- "[\"A\", \"B\", \"C\"]"

fleet :: String
fleet = [r|
[
  {
    "name": "Black Pearl",
    "crew": [
      {
        "name": "Jack Sparrow",
        "rank": "Captain"
      },
      {
        "name": "Will Turner",
        "rank": "First Mate"
      }
    ]
  },
  {
    "name": "Flying Dutchman",
    "crew": [
      {
        "name": "Davy Jones",
        "rank": "Captain"
      },
      {
        "name": "Bootstrap Bill",
        "rank": "First Mate"
      }
    ]
  }
]
|]

aeson15 = fleet ^.. values . key "name" . _String
-- ["Black Pearl", "Flying Dutchman"]
aeson16 = fleet ^.. values . key "crew" . values . key "name" . _String
-- [ "Jack Sparrow"
-- , "Will Turner"
-- , "Davy Jones"
-- , "Bootstrap Bill"
-- ]

aeson17 = fleet
            ^@.. values
            . reindexed (view (key "name" . _String)) selfIndex
            <. (key "crew" . values . key "name" . _String)
-- [ ("Black Pearl" , "Jack Sparrow")
-- , ("Black Pearl" , "Will Turner")
-- , ("Flying Dutchman", "Davy Jones")
-- , ("Flying Dutchman", "Bootstrap Bill")
-- ]

----------------------------------------
-- Traversing Objects
----------------------------------------

-- members :: AsValue t => IndexedTraversal' Text t Value

cargo :: String
cargo = [r|
{
  "emeralds": 327,
  "rubies": 480,
  "sapphires": 621,
  "opals": 92,
  "dubloons": 34
}
|]

aeson20 = cargo ^.. members . _Integer
-- [34, 327, 92, 480]
aeson21 = sumOf (members . _Integer) cargo
-- 1554
aeson22 = cargo ^@.. members . _Integer
-- [ ("dubloons" , 34)
-- , ("emeralds" , 327)
-- , ("opals" , 92)
-- , ("sapphires", 621)
-- , ("rubies" , 480)
-- ]
aeson23 = maximumByOf (members . _Integer . withIndex) (comparing snd) cargo
-- Just ("sapphires", 621)

----------------------------------------
-- 15.4 Filtering JSON Queries
----------------------------------------

aeson24 = fleet
            ^.. values
            . key "crew"
            . values
            . filteredBy (key "rank" . only "Captain")
            . key "name"
            . _String
-- [ "Jack Sparrow"
-- , "Davy Jones"
-- ]

----------------------------------------
-- 15.5 Serializing & Deserializing within an optics path
----------------------------------------

data Creature =
  Creature
    { creatureName :: String
    , creatureSize :: Double
    } deriving stock (Generic, Show)
      deriving anyclass (ToJSON, FromJSON)

makeLensesFor [ ("creatureName", "creatureNameL")
              , ("creatureSize", "creatureSizeL")
              ] '' Creature

creatureSightings :: String
creatureSightings = [r|
{
  "Arctic": [
    {
      "creatureName": "Giant Squid",
      "creatureSize": 45.2
    }
  ],
  "Pacific": [
    {
      "creatureName": "Kraken",
      "creatureSize": 124.4
    },
    {
      "creatureName": "Ogopogo",
      "creatureSize": 34.6
    }
  ]
}
|]

aeson25 :: [Creature]
aeson25 = creatureSightings
            ^.. members
            . values
            . _JSON
-- [ Creature
--   { creatureName = "Kraken"
--   , creatureSize = 124.4
--   }
-- , Creature
--   { creatureName = "Ogopogo"
--   , creatureSize = 34.6
--   }
-- , Creature
--   { creatureName = "Giant Squid"
--   , creatureSize = 45.2
--   }
-- ]

-- _JSON :: (FromJSON a, ToJSON a) => Prism' t a

aeson26 = creatureSightings
            & members
            . values
            . _JSON @_ @Creature
            . creatureSizeL
            *~ 1.2
-- {
--   "Pacific": [
--   {
--     "creatureName": "Kraken",
--       "creatureSize": 149.28
--   },
--   {
--     "creatureName": "Ogopogo",
--     "creatureSize": 41.52
--   }
--   ],
--   "Arctic": [
--   {
--     "creatureName": "Giant Squid",
--     "creatureSize": 54.24
--   }
--   ]
-- }

----------------------------------------
-- 15.6 Exercises: Kubernetes API
----------------------------------------

pods :: BS.ByteString
pods = [r|
{
  "kind": "List",
  "apiVersion": "v1",
  "items": [
    {
      "kind": "Pod",
      "apiVersion": "v1",
      "metadata": {
        "name": "redis-h315w",
        "creationTimestamp": "2019-03-23T19:42:21Z",
        "labels": {
          "name": "redis",
          "region": "usa"
        }
      },
      "spec": {
        "containers": [
          {
            "name": "redis",
            "image": "redis",
            "ports": [
              {
                "name": "redis",
                "hostPort": 27017,
                "containerPort": 27017,
                "protocol": "TCP"
              }
            ],
            "resources": {
              "requests": {
                "cpu": "100m"
              }
            }
          }
        ]
      }
    },
    {
      "kind": "Pod",
      "apiVersion": "v1",
      "metadata": {
        "name": "web-4c5bj",
        "creationTimestamp": "2019-02-24T20:23:56Z",
        "labels": {
          "name": "web",
          "region": "usa"
        }
      },
      "spec": {
        "containers": [
          {
            "name": "web",
            "image": "server",
            "ports": [
              {
                "name": "http-server",
                "containerPort": 3000,
                "protocol": "TCP"
              }
            ],
            "resources": {
              "requests": {
                "cpu": "100m"
              }
            }
          }
        ]
      }
    }
  ]
}
|]

aesonex1 = pods ^? key "apiVersion" . _String
-- Just "v1"

aesonex2 = lengthOf
           (key "items" . values . key "spec" . key "containers" . values)
           pods
-- 2

aesonex3 = pods
            ^.. key "items"
            . values
            . key "spec"
            . key "containers"
            . values
            . filtered (\v -> v^? key "name" == v ^? key "image")
            . key "name"
            . _String
-- [ "redis" ]

aesonex4 =
  toListOf
    (( key "items"
       . values
       . reindexed (view (key "metadata" . key "name" . _String))
       selfIndex
       <. (key "spec"
         . key "containers"
         . values
         . key "ports"
         . values
         . key "containerPort"
         . _Integer))
     . withIndex)
    pods
-- [
--   ( "redis-h315w"
--   , 27017
--   )
-- ,
--   ( "web-4c5bj"
--   , 3000
--   )
-- ]

aesonex5 =
  pods
    & key "items"
    . values
    . key "metadata"
    . key "labels"
    . members
    . _String
    %~ toUpper
-- ...

aesonex6 =
  pods
    & key "items"
    . values
    . key "spec"
    . key "containers"
    . values
    . key "resources"
    . key "requests"
    . _Object
    .  at "memory"
    ?~ "256M"
-- ...
